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
        check_update | Str | default = ""
    },

    PackageManager = {
        prefix | Str ,
        version | Str,
        packages | Array Package,
    }
}