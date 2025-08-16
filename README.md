Read files based on a glob pattern, and generate a separate test for each matching file:

```rust
#[glob_test::glob("./usage/inputs/**/*.txt")]
fn test(path: &std::path::Path) {
    std::fs::read_to_string(path).unwrap();
}
```

<details>
<summary>Macro output</summary>

```rust
mod usage {
    use super::*;
    mod inputs {
        use super::*;

        #[test]
        fn a() {
            (|path: &std::path::Path| {
                std::fs::read_to_string(path).unwrap();
            })(
                ::std::path::Path::new(
                    "CARGO_MANIFEST_DIR/tests/usage/inputs/a.txt"
                )
            )
        }

        #[test]
        fn b() {
            (|path: &std::path::Path| {
                std::fs::read_to_string(path).unwrap();
            })(
                ::std::path::Path::new(
                    "CARGO_MANIFEST_DIR/tests/usage/inputs/b.txt"
                )
            )
        }
    }
}

```

</details>

### Details

- One module is emitted for each nested directory, and each file in that directory becomes a test function:
  - `inputs/a.txt` -> `mod inputs { #[test] fn a() {} }`
  - `inputs/nested/a.txt` -> `mod inputs { mod nested { #[test] fn a() {} } }`
- The proc macro doesn't use `syn` or `quote`, instead parsing from `proc_macro2` tokens.
- Globbing is powered by [`glob`](https://crates.io/crates/glob).

This library was specifically developed for use with [`insta`](https://insta.rs) in mind:

```rust
#[glob_test::glob("./**/*.txt")]
fn snapshots(path: &Path) {
    insta::assert_snapshot!(std::fs::read_to_string(path).unwrap());
}
```

It is recommended that you put a `build.rs` file in any crate which uses this library.
An empty `main` will do:

```rust
// build.rs
fn main() {}
```

That ensures changes to test files are reflected in the test binary.

You can scope Cargo's file change checks using `rerun-if-changed`:

```rust
// build.rs

fn main() {
    println!("cargo:rerun-if-changed=tests/usage/inputs")
}
```

### License

Licensed under either of

- Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license
  ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

