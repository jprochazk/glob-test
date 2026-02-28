use std::path::Path;

#[glob_test::glob("../usage/inputs/**/*.txt")]
unsafe fn test(path: &Path) {
    let _ = std::fs::read_to_string(path).unwrap();
}

fn main() {}
