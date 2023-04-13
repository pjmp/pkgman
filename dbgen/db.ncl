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
