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
        }
    ]
} in
packages | PackageManager
