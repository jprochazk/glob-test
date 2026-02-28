use std::path::Path;

#[glob_test::glob("../usage/inputs/**/*.txt")]
const fn test(path: &Path) {
    let _ = path;
}

fn main() {}
