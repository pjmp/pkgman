mod pkgman;

fn main() {
    let app = pkgman::new().get_matches();
    let has_packger = app.is_present("packger");

    let packger = if has_packger {
        app.value_of("packger").unwrap()
    } else {
        "yay"
    };

    if app.is_present("search") {
        let cmd = format!(
            "{} -Ss {}",
            if has_packger { packger } else { "pacman" },
            app.value_of("search").unwrap()
        );

        let output = pkgman::exec_command(&cmd);

        if !output.status.success() && !has_packger {
            pkgman::exec_command(&format!("yay -Ss {}", app.value_of("search").unwrap()));
        }
    }

    if app.is_present("install") {
        let cmd = format!("{} -S {}", packger, app.value_of("install").unwrap());
        pkgman::exec_command(&cmd);
    }

    if app.is_present("remove") {
        let cmd = format!("{} -R {}", packger, app.value_of("remove").unwrap());
        pkgman::exec_command(&cmd);
    }

    if app.is_present("upgrade") {
        let cmd = format!("{} -Syyu", packger);
        pkgman::exec_command(&cmd);
    }

    if app.is_present("purge") {
        let cmd = "sudo -k pacman -Rns $(pacman -Qttdq)".to_string();
        pkgman::exec_command(&cmd);
    }
}
