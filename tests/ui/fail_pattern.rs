use std::path::Path;

#[glob_test::glob("../usage/inputs/**/*.txt")]
fn test((path, _): (&Path, i32)) {
    let _ = std::fs::read_to_string(path).unwrap();
}

fn main() {}
