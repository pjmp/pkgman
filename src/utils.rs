use std::process::{Command, Output, Stdio};

pub fn exec_command(cmd: &str) -> Output {
    Command::new("sh")
        .arg("-c")
        .arg(format!("{} --noconfirm", cmd))
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|_| panic!("Failed to start '{}'", cmd.to_string()))
}
