use std::path::{Path, PathBuf};
use assert_cmd::prelude::*;
use std::process::Command;

fn example_path(name: &str) -> PathBuf {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    root.join("examples").join(name)
}

pub fn compile_example(name: &str) {
    let src = example_path(name);
    assert!(src.exists(), "missing example: {}", src.display());

    let mut cmd = Command::cargo_bin("x-cli").expect("x-cli binary");
    cmd.env("XLANG_ENABLE_INVOKE", "0")
        .arg("build")
        .arg(src.as_os_str());

    cmd.assert()
        .success();
}
