use std::io::{self, Write};
use std::process::{exit, Command, Output, Stdio};

pub fn get_pkgman_cmd(cmd: &str) -> Output {
    Command::new("sh")
        .arg("-c")
        .arg(format!("{} --noconfirm", cmd))
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|_| panic!("Failed to start '{}'", cmd.to_string()))
}

#[allow(unused_must_use)]
pub fn write_to_stdout_or_stderr(output: Output) {
    if !output.stdout.is_empty() {
        io::stdout().write_all(&output.stdout);
        exit(0)
    }

    if !output.stderr.is_empty() {
        io::stderr().write_all(&output.stderr);
        exit(1)
    }
}

pub fn exec_command(cmd: &str) {
    write_to_stdout_or_stderr(get_pkgman_cmd(cmd))
}

//pub fn exec_command_with_sudo(cmd: &str) {
//    use std::env;
//
//    if match env::var("EUID") {
//        Ok(usr) => usr == "0".to_string(),
//        Err(_) => false,
//    } {
//        exec_command(cmd);
//    } else {
//        println!("needs admin permissions for this");
//    }
//}
