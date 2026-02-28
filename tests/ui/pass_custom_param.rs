use std::path::Path;

#[glob_test::glob("../usage/inputs/**/*.txt")]
fn test(file: &Path) {
    let _ = std::fs::read_to_string(file).unwrap();
}

fn main() {}
