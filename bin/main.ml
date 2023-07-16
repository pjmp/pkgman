module Pkgman = Pkgman_lib
module Types = Pkgman_common.Interface

module App = Cli.CLI (struct
  let run (action : Types.action) (opts : Types.opts) =
    let (module P : Types.Provider) =
      match opts.provider with
      | Types.Github -> (module Pkgman.Github)
      | _ ->
          failwith
            ("Provider "
            ^ Types.show_providers opts.provider
            ^ " is unimplemented")
    in
    match action with
    | Types.Install -> P.install opts
    | Types.Search -> P.search opts
end)

(*
https://gitlab.manjaro.org/packages/core/bash/-/blob/master/dot.bashrc
*.tar.bz2)   tar xjf $1   ;;
*.tar.gz)    tar xzf $1   ;;
*.bz2)       bunzip2 $1   ;;
*.rar)       unrar x $1   ;;
*.gz)        gunzip $1    ;;
*.tar)       tar xf $1    ;;
*.tbz2)      tar xjf $1   ;;
*.tgz)       tar xzf $1   ;;
*.zip)       unzip $1     ;;
*.Z)         uncompress $1;;
*.7z)        7z x $1      ;;
*)

let () =
  let _ = Printexc.record_backtrace true in
  print_newline ();

  App.main ()
