let {Package, PackageManager} = import "./types.ncl" in

let packages = {
    prefix = "~/.local",
    version = "v1",
    packages = [
        {
            name = "go",
            version = "1.20.3",
            file = "go%{version}.linux-amd64.tar.gz",
            url = "https://dl.google.com/go/%{file}",
            install = m%"
                curl -LO "%{url}"
                rm -rf %{prefix}/go
                tar -C %{prefix} -xzf %{file}
                path=%{prefix}/go/bin
                if ! echo "$PATH" | grep -qw "$path"; then
                    echo "'$path' doesn't exist in \$PATH, add 'export PATH=\$PATH:$path' to you rc file"
                fi
            "%m,
        }
    ]
} in
packages | PackageManager

#  https://dl.google.com/go/go1.18.linux-amd64.tar.gz
#  https://storage.googleapis.com/dart-archive/channels/stable/release/2.16.2/sdk/dartsdk-linux-x64-release.zip
