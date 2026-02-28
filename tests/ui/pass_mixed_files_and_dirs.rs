use std::path::Path;

/// `**/*` matches both directories and files recursively.
/// `fn alpha` (from the directory) and `mod alpha` (from files inside it)
/// coexist because they are in different namespaces.
#[glob_test::glob("../usage/dirs/**/*")]
fn test(path: &Path) {
    let _ = path;
}

fn main() {}
