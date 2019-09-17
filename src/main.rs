mod app;
mod utils;

fn main() {
    let pkgman = app::new().get_matches();
    let has_packger = pkgman.is_present("packger");

    let packger = if has_packger {
        pkgman.value_of("packger").unwrap()
    } else {
        "yay"
    };

    if pkgman.is_present("search") {
        let cmd = format!(
            "{} -Ss {}",
            if has_packger { packger } else { "pacman" },
            pkgman.value_of("search").unwrap()
        );

        let output = utils::exec_command(&cmd);

        if !output.status.success() && !has_packger {
            utils::exec_command(&format!("yay -Ss {}", pkgman.value_of("search").unwrap()));
        }
    }

    if pkgman.is_present("install") {
        let cmd = format!("{} -S {}", packger, pkgman.value_of("install").unwrap());
        utils::exec_command(&cmd);
    }

    if pkgman.is_present("remove") {
        let cmd = format!("{} -R {}", packger, pkgman.value_of("remove").unwrap());
        utils::exec_command(&cmd);
    }

    if pkgman.is_present("upgrade") {
        let cmd = format!("{} -Syyu", packger);
        utils::exec_command(&cmd);
    }

    if pkgman.is_present("purge") {
        let cmd = "sudo -k pacman -Rns $(pacman -Qttdq)".to_string();
        utils::exec_command(&cmd);
    }
}
