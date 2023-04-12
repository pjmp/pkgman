{
    Package = {
        description | Str,
        file | Str,
        homepage | Str,
        install | Str,
        name | Str,
        postinstall | Str | default = "",
        url | Str,
        version | Str,
    },

    PackageManager = {
        prefix | Str ,
        version | Str,
        packages | Array Package,
    }
}