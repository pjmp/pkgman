use clap::{crate_version, App, AppSettings, Arg};
use std::process::{Command, Output, Stdio};

pub fn new<'a, 'b>() -> App<'a, 'b> {
    App::new("pkgman")
        .version(crate_version!())
        .setting(AppSettings::ArgRequiredElseHelp)
        .setting(AppSettings::ColorAuto)
        .arg(
            Arg::with_name("packger")
                .short("P")
                .long("packger")
                .help("package manager to use, can be either pacman or yay defaulting to yay")
                .takes_value(true)
                .require_equals(true)
                .value_name("packger")
                .validator(|arg| -> Result<(), String> {
                    if arg.eq(&"yay".to_owned()) || arg.eq(&"pacman".to_owned()) {
                        return Ok(());
                    }

                    Err("should be one of yay|pacman".to_string())
                }),
        )
        .arg(
            Arg::with_name("search")
                .short("s")
                .long("search")
                .help("search for given package by name")
                .takes_value(true)
                .value_name("query"),
        )
        .arg(
            Arg::with_name("install")
                .short("i")
                .long("install")
                .help("install a given package")
                .takes_value(true)
                .value_name("package"),
        )
        .arg(
            Arg::with_name("remove")
                .short("r")
                .long("remove")
                .help("remove a given package")
                .takes_value(true)
                .value_name("package"),
        )
        .arg(
            Arg::with_name("upgrade")
                .short("u")
                .long("upgrade")
                .help("check for updates and install"),
        )
        .arg(
            Arg::with_name("purge")
                .short("p")
                .long("purge")
                .help("check for updates and install"),
        )
}

pub fn exec_command(cmd: &str) -> Output {
    Command::new("sh")
        .arg("-c")
        .arg(format!("{} --noconfirm", cmd))
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|_| panic!("Failed to start '{}'", cmd.to_string()))
}
