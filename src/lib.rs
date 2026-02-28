use proc_macro2::Delimiter;
use proc_macro2::{
    Literal, Span, TokenStream, TokenTree, token_stream::IntoIter as TokenStreamIter,
};
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::str::FromStr as _;

#[proc_macro_attribute]
pub fn glob(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr: TokenStream = attr.into();
    let item: TokenStream = item.into();

    match try_glob(attr.clone(), item.clone()) {
        Ok(token_stream) => token_stream.into(),
        Err(err) => {
            let mut input = item;
            input.extend(err.to_token_stream());
            input.into()
        }
    }
}

fn stringify<E: std::error::Error>(e: E) -> Error {
    error(e.to_string(), Span::call_site())
}

/// `#[glob_test::glob("./path/*.txt")] fn`
fn try_glob(attr: TokenStream, item: TokenStream) -> Result<TokenStream> {
    let path = parse_str_literal(&mut attr.clone().into_iter())?;
    let path = path
        .strip_prefix("./")
        .map(|v| v.to_owned())
        .unwrap_or(path);

    let original_attr: proc_macro::TokenStream = attr.into();
    let attr_span = original_attr.into_iter().next().unwrap().span();

    let Some(local_file_path) = attr_span.local_file() else {
        return Ok(item);
    };

    let traverse_root = local_file_path.parent().unwrap().canonicalize().unwrap();

    let resolved_path = traverse_root.join(&path);
    let resolved_path = resolved_path.to_str().unwrap();

    let mut tree = FileTree {
        root: TreeNode {
            children: BTreeMap::new(),
            paths: BTreeSet::new(),
        },
    };

    let paths = glob::glob(resolved_path).map_err(stringify)?;
    for path in paths {
        let path = path.map_err(stringify)?;

        let _ = path.strip_prefix(&traverse_root).map_err(|_| {
            error(
                format!("path cannot be outside {}", traverse_root.display()),
                Span::call_site(),
            )
        })?;

        tree.insert(&traverse_root, path.to_path_buf());
    }

    let mut output = String::new();

    // usage/a.txt -> mod usage { #[test] fn a() {} }
    // usage/b.txt -> mod usage { #[test] fn b() {} }
    // usage/nested/c.txt -> mod usage { mod nested { #[test] fn c() {} } }

    let test_handler = parse_fn(item.clone())?;

    traverse(&mut output, &tree.root.children, &test_handler);

    let mut tests = TokenStream::new();
    tests.extend(TokenStream::from_str("#[allow(dead_code)]").unwrap());
    tests.extend(item);
    tests.extend(TokenStream::from_str(&output).map_err(|err| {
        error(
            format!("failed to parse token stream: {err}"),
            attr_span.into(),
        )
    })?);

    Ok(tests)
}

fn traverse(
    output: &mut String,
    children: &BTreeMap<PathBuf, TreeNode>,
    test_handler: &TestHandler,
) {
    for (dir, node) in children.iter() {
        if let Some(module_name) = dir.file_name() {
            let module_name = normalize(module_name);
            writeln!(
                output,
                "mod {module_name} {{ #[allow(unused_imports)] use super::*; "
            )
            .unwrap();
        }

        for file in &node.paths {
            generate_test(output, file, test_handler);
        }

        traverse(output, &node.children, test_handler);

        if let Some(_) = dir.file_name() {
            writeln!(output, "}}").unwrap();
        }
    }
}

fn generate_test(output: &mut String, path: &Path, test_handler: &TestHandler) {
    let test_name = normalize(path.file_stem().unwrap());
    let path = path.display().to_string();
    let param_name = &test_handler.param_name;
    let body = &test_handler.body;

    writeln!(
        output,
        "#[test]
        fn {test_name}() {{
            (|{param_name}: &::std::path::Path| {body})
            (::std::path::Path::new({path:?}));
        }}"
    )
    .unwrap();
}

fn normalize(s: &OsStr) -> String {
    s.to_string_lossy()
        .chars()
        .map(|c| if c.is_ascii() { c } else { '_' })
        .collect()
}

struct TestHandler {
    param_name: String,
    body: String,
}

/// Parses `fn <ident>(<ident> <anything>) <anything> { <body> }`.
fn parse_fn(tokens: TokenStream) -> Result<TestHandler> {
    let mut tokens = tokens.into_iter();

    let fn_keyword = skip_attributes(&mut tokens)?;

    match &fn_keyword {
        TokenTree::Ident(ident) if ident.to_string() == "fn" => {}
        TokenTree::Ident(ident)
            if matches!(
                ident.to_string().as_str(),
                "const" | "async" | "unsafe" | "extern" | "pub"
            ) =>
        {
            return Err(error(
                format!("`{}` functions are not supported", ident),
                ident.span(),
            ));
        }
        _ => return Err(error("expected `fn`".into(), fn_keyword.span())),
    }

    // function name
    let _fn_name = expect_ident(&mut tokens, "function name")?;

    // generics are not supported, so parameters must come next
    let params_group = expect_params(&mut tokens)?;

    // first token in the parameter list must be an identifier
    let mut param_tokens = params_group.stream().into_iter();
    let param_tt = param_tokens
        .next()
        .ok_or_else(|| error("expected parameter name".into(), params_group.span()))?;
    let param_name = match param_tt {
        TokenTree::Ident(ident) => ident.to_string(),
        // if it's not an identifier, it must be a pattern, which we don't support:
        _ => {
            return Err(error(
                "expected parameter name, not a pattern".into(),
                param_tt.span(),
            ));
        }
    };

    // find body (skip return type, where clause, etc.)
    let body = find_body(&mut tokens)?;

    Ok(TestHandler {
        param_name,
        body: body.to_string(),
    })
}

/// Skips `#[...]` attributes, returning the first non-attribute token.
fn skip_attributes(tokens: &mut TokenStreamIter) -> Result<TokenTree> {
    loop {
        let tt = tokens
            .next()
            .ok_or_else(|| error("expected `fn`".into(), Span::call_site()))?;
        if let TokenTree::Punct(p) = &tt
            && p.as_char() == '#'
        {
            let bracket = tokens
                .next()
                .ok_or_else(|| error("expected `[`".into(), p.span()))?;
            match &bracket {
                TokenTree::Group(g) if g.delimiter() == Delimiter::Bracket => continue,
                _ => return Err(error("expected `[`".into(), bracket.span())),
            }
        }
        return Ok(tt);
    }
}

fn expect_ident(tokens: &mut TokenStreamIter, expected: &str) -> Result<proc_macro2::Ident> {
    let tt = tokens
        .next()
        .ok_or_else(|| error(format!("expected {expected}"), Span::call_site()))?;
    match tt {
        TokenTree::Ident(ident) => Ok(ident),
        _ => Err(error(format!("expected {expected}"), tt.span())),
    }
}

/// `(...)`
fn expect_params(tokens: &mut TokenStreamIter) -> Result<proc_macro2::Group> {
    let tt = tokens
        .next()
        .ok_or_else(|| error("expected `(`".into(), Span::call_site()))?;
    match tt {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => Ok(g),
        TokenTree::Punct(p) if p.as_char() == '<' => Err(error(
            "generic functions are not supported".into(),
            p.span(),
        )),
        _ => Err(error("expected `(`".into(), tt.span())),
    }
}

/// Finds the last `{ ... }` group in the remaining tokens, which is the function body.
fn find_body(tokens: &mut TokenStreamIter) -> Result<proc_macro2::Group> {
    let mut body = None;
    for tt in tokens {
        if let TokenTree::Group(g) = tt
            && g.delimiter() == Delimiter::Brace
        {
            body = Some(g);
        }
    }
    body.ok_or_else(|| error("expected function body".into(), Span::call_site()))
}

#[derive(Debug)]
struct FileTree {
    root: TreeNode,
}
impl FileTree {
    fn insert(&mut self, traverse_root: &Path, path: PathBuf) {
        let path_for_parent = path.strip_prefix(traverse_root).unwrap();
        let parent = self.get_or_insert_dir(path_for_parent.parent().unwrap());
        parent.paths.insert(path);
    }

    fn get_or_insert_dir(&mut self, dir: &Path) -> &mut TreeNode {
        let mut current = &mut self.root;
        for component in dir {
            current = current
                .children
                .entry(PathBuf::from(component))
                .or_default();
        }
        current
    }
}

#[derive(Debug, Default)]
struct TreeNode {
    children: BTreeMap<PathBuf, TreeNode>,
    paths: BTreeSet<PathBuf>,
}

fn parse_str_literal(input: &mut TokenStreamIter) -> Result<String> {
    let tt = input
        .next()
        .ok_or_else(|| error("expected string literal".into(), Span::call_site()))?;

    let lit = match tt {
        TokenTree::Literal(lit) => lit,
        _ => return Err(error("expected string literal".into(), tt.span())),
    };

    let span = lit.span();
    let lit = parse_literal(&lit).map_err(|err| error(err.to_string(), span))?;
    String::from_utf8(lit).map_err(|err| error(err.to_string(), span))
}

struct Error {
    message: String,
    span: Span,
}

fn error(message: String, span: Span) -> Error {
    Error { message, span }
}

impl Error {
    fn to_token_stream(self) -> TokenStream {
        let token_stream =
            TokenStream::from_str(&format!("compile_error!(r##\"{}\"##);", self.message)).unwrap();

        let span = Span::call_site().located_at(self.span);
        token_stream
            .into_iter()
            .map(|mut tt| {
                tt.set_span(span.clone());
                tt
            })
            .collect::<TokenStream>()
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

// code below is from:
// https://github.com/upsuper/cstr/blob/e072e05f6ace9a37f7f9fb385ca2029cba6bd602/src/parse.rs

macro_rules! unexpected_content {
    () => {
        "expected a string literal"
    };
}

fn parse_literal(literal: &Literal) -> Result<Vec<u8>, &'static str> {
    let s = literal.to_string();
    let s = s.as_bytes();
    match s[0] {
        b'"' => Ok(parse_cooked_content(&s)),
        b'r' => Ok(parse_raw_content(&s[1..])),
        b'b' => match s[1] {
            b'"' => Ok(parse_cooked_content(&s[1..])),
            b'r' => Ok(parse_raw_content(&s[2..])),
            _ => Err(unexpected_content!()),
        },
        _ => Err(unexpected_content!()),
    }
}

fn all_pounds(bytes: &[u8]) -> bool {
    bytes.iter().all(|b| *b == b'#')
}

/// Parses raw string / bytes content after `r` prefix.
fn parse_raw_content(s: &[u8]) -> Vec<u8> {
    let q_start = s.iter().position(|b| *b == b'"').unwrap();
    let q_end = s.iter().rposition(|b| *b == b'"').unwrap();
    assert!(all_pounds(&s[0..q_start]));
    assert!(all_pounds(&s[q_end + 1..q_end + q_start + 1]));
    Vec::from(&s[q_start + 1..q_end])
}

/// Parses the cooked string / bytes content within quotes.
fn parse_cooked_content(mut s: &[u8]) -> Vec<u8> {
    s = &s[1..s.iter().rposition(|b| *b == b'"').unwrap()];
    let mut result = Vec::new();
    while !s.is_empty() {
        match s[0] {
            b'\\' => {}
            b'\r' => {
                assert_eq!(s[1], b'\n');
                result.push(b'\n');
                s = &s[2..];
                continue;
            }
            b => {
                result.push(b);
                s = &s[1..];
                continue;
            }
        }
        let b = s[1];
        s = &s[2..];
        match b {
            b'x' => {
                let (b, rest) = backslash_x(&s);
                result.push(b);
                s = rest;
            }
            b'u' => {
                let (c, rest) = backslash_u(&s);
                result.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                s = rest;
            }
            b'n' => result.push(b'\n'),
            b'r' => result.push(b'\r'),
            b't' => result.push(b'\t'),
            b'\\' => result.push(b'\\'),
            b'0' => result.push(b'\0'),
            b'\'' => result.push(b'\''),
            b'"' => result.push(b'"'),
            b'\r' | b'\n' => {
                let next = s.iter().position(|b| {
                    let ch = char::from_u32(u32::from(*b)).unwrap();
                    !ch.is_whitespace()
                });
                match next {
                    Some(pos) => s = &s[pos..],
                    None => s = b"",
                }
            }
            b => panic!("unexpected byte {:?} after \\", b),
        }
    }
    result
}

fn backslash_x(s: &[u8]) -> (u8, &[u8]) {
    let ch = hex_to_u8(s[0]) * 0x10 + hex_to_u8(s[1]);
    (ch, &s[2..])
}

fn hex_to_u8(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'f' => b - b'a' + 10,
        b'A'..=b'F' => b - b'A' + 10,
        _ => unreachable!("unexpected non-hex character {:?} after \\x", b),
    }
}

fn backslash_u(s: &[u8]) -> (char, &[u8]) {
    assert_eq!(s[0], b'{');
    let end = s[1..].iter().position(|b| *b == b'}').unwrap();
    let mut ch = 0;
    for b in &s[1..=end] {
        ch *= 0x10;
        ch += u32::from(hex_to_u8(*b));
    }
    (char::from_u32(ch).unwrap(), &s[end + 2..])
}
