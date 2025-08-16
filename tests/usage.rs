use std::path::Path;

#[glob_test::glob("./usage/inputs/**/*.txt")]
fn test(path: &Path) {
    std::fs::read_to_string(path).unwrap();
}
