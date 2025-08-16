use std::path::Path;

#[glob_test::glob("./usage/**/*.txt")]
fn snapshots(path: &Path) {
    insta::assert_snapshot!(std::fs::read_to_string(path).unwrap());
}
