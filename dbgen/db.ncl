let {Package, PackageManager} = import "./schema.ncl" in

let packages = {
    prefix = "~/.local",
    version = "v1",
    packages = [
        {
            name = "go",
            version = "1.20.3",
            file = "go%{version}.linux-amd64.tar.gz",
            url = "https://dl.google.com/go/%{file}",
            description = "The Go Programming Language",
            homepage = "https://go.dev",
            install = m%"
                curl -LO "%{url}"
                rm -rf %{prefix}/go
                tar -C %{prefix} -xzf %{file}
            "%m,
            postinstall = m%"
                path=%{prefix}/go/bin
                if ! echo "$PATH" | grep -qw "$path"; then
                    echo "'$path' doesn't exist in \$PATH, add 'export PATH=\$PATH:$path' to you rc file"
                fi
            "%m,
            check_update = m%"
                node --no-warnings -e '
                (async () => {
                    const path = process.env.PKGMAN_DB_PATH;
                    const pkgName = process.env.PKGMAN_PKG_NAME;

                    if (!path || !pkgName) {
                        console.error(`env ${!path && "PKGMAN_DB_PATH" || !pkgName && "PKGMAN_PKG_NAME"} not found`)
                        process.exit(1)
                    }

                    try {
                        const versions = await (await fetch("https://go.dev/dl/?mode=json")).json();
                        const db = require(path);
                        const [{version: v}] = versions
                        const version = v.replace(/^go/, "");
                        const pkg = db.packages.find((pkg) => pkg.name === pkgName);

                        if (!pkg) {
                            console.error("???")
                            process.exit(1)
                        }

                        const {version: currentVersion} = pkg;

                        currentVersion !== version && console.info("current version is `%s` found `%s`", currentVersion, version);

                    } catch (error) {
                        console.error(error.message);
                        process.exit(1);
                    }
                })()
                '
            "%m
        },
        {
            name = "dart",
            version = "2.19.6",
            file = "%{name}sdk-linux-x64-release.zip",
            url = "https://storage.googleapis.com/dart-archive/channels/stable/release/%{version}/sdk/%{file}",
            description = "Dart Programming Language",
            homepage = "https://dart.dev",
            install = m%"
                curl -LO "%{url}"
                unzip -o %{file} -d %{prefix}
                path=%{prefix}/%{name}-sdk/bin
                if ! echo "$PATH" | grep -qw "$path"; then
                    echo "'$path' doesn't exist in \$PATH, add 'export PATH=\$PATH:$path' to you rc file"
                fi
            "%m,
            postinstall = "",
            check_update = ""
        },
        {
            name = "gh",
            version = "2.27.0",
            file = "%{name}_%{version}_linux_amd64.rpm",
            url = "https://github.com/cli/cli/releases/download/v%{version}/%{file}",
            description = "GitHub's official command line tool",
            homepage = "https://cli.github.com/",
            install = m%"
                curl -LO "%{url}"
                sudo dnf install ./%{file}
            "%m,
            postinstall = "",
            check_update = ""
        },
        {
            name = "fnm",
            version = "1.33.1",
            file = "%{name}-linux.zip",
            url = "https://github.com/Schniz/fnm/releases/download/v%{version}/%{file}",
            description = "🚀 Fast and simple Node.js version manager, built in Rust",
            homepage = "https://fnm.vercel.app",
            install = m%"
                curl -LO "%{url}"
                unzip -o %{file} -d %{prefix}/bin
                chmod --verbose +x %{prefix}/bin/%{name}
            "%m,
            postinstall = "",
            check_update = ""
        },
        {
            name = "gprofile",
            version = "0.2.0",
            file = "%{name}-linux.zip",
            url = "https://github.com/pombadev/gprofile/releases/download/v%{version}/%{file}",
            description = "Quickly switch git user, email with ease.",
            homepage = "https://github.com/pombadev/gprofile",
            install = m%"
                curl -LO "%{url}"
                mv --verbose %{file} %{prefix}/bin
                chmod --verbose +x %{prefix}/bin/git-profile
            "%m,
            postinstall = "",
            check_update = ""
        },
        {
            name = "mkcert",
            version = "1.4.4",
            file = "%{name}-v%{version}-linux-amd64",
            url = "https://github.com/FiloSottile/mkcert/releases/download/v%{version}/%{file}",
            description = "A simple zero-config tool to make locally trusted development certificates with any names you'd like.",
            homepage = "mkcert.dev",
            install = m%"
                curl -LO "%{url}"
                mv --verbose %{file} %{prefix}/bin/%{name}
                chmod --verbose +x %{prefix}/bin/%{name}
            "%m,
            postinstall = "",
            check_update = ""
        },
        {
            name = "act",
            version = "0.2.44",
            file = "%{name}_Linux_x86_64.tar.gz",
            url = "https://github.com/nektos/act/releases/download/v%{version}/%{file}",
            description = "Run your GitHub Actions locally 🚀",
            homepage = "https://github.com/nektos/act",
            install = m%"
                curl -LO "%{url}"
                tar -xzf %{file}
                mv --verbose %{name} %{prefix}/bin/%{name}
                chmod --verbose +x %{prefix}/bin/%{name}
            "%m,
            postinstall = "",
            check_update = ""
        }
    ]
} in
packages | PackageManager
