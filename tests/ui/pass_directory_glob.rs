use std::path::Path;

/// Glob matches only immediate subdirectories of `dirs/`.
#[glob_test::glob("../usage/dirs/*/")]
fn test(dir: &Path) {
    assert!(dir.is_dir());
}

fn main() {}
