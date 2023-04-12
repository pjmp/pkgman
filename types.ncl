{
    Package = {
        name | Str,
        version | Str,
        url | Str,
        install | Str,
        file | Str,
        postinstall | Str | default = "",
    },

    PackageManager = {
        prefix | Str ,
        version | Str,
        packages | Array Package,
    }
}