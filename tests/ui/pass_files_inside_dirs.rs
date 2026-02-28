use std::path::Path;

/// Glob matches files inside directories. Directories become modules, files become tests.
#[glob_test::glob("../usage/dirs/**/*.txt")]
fn test(path: &Path) {
    assert!(path.is_file());
}

fn main() {}
