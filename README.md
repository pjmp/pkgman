# pkgman ![ocaml-logo](https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=white)

`pkgman` is a cli tool to download packages from arbitrary sources and apply post/pre processing to the downloaded files/system.

## Installation

> WIP

## Usage

```shell
pkgman -h
```

## Setup

```bash
if [ -d ~/.local/pkgman ]; then
    PKGMAN_PATH="$(find ~/.local/pkgman -type f -executable -exec sh -c 'dirname $1 | tr "\n" ":"' shell {} \;)"

    export PATH="$PATH:$PKGMAN_PATH"
fi
```

## Alternative idea

Resources:

https://github.com/ianchanning/awesome-github-alternatives

https://docs.github.com/en/rest/search?apiVersion=2022-11-28#search-repositories

https://docs.github.com/en/rest/releases/releases?apiVersion=2022-11-28#list-releases

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
