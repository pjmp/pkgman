(lang dune 3.6)

(name pkgman)

(generate_opam_files true)

(source
 (github pombadev/pkgman))

(authors "Pomba Magar")

(maintainers "Pomba Magar")

(license MIT)

(package
 (name pkgman)
 (version 0.1)
 (synopsis "Packages downloader")
 (description
  "`pkgman` is a cli tool to download packages from arbitrary sources and apply post/pre processing to the downloaded files/system.")
 (depends ocaml dune)
 (tags
  (cli downloader "packages downloader")))
