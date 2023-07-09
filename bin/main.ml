let run () =
  let run action query provider ty =
    let (module P : Providers.Types.Provider) =
      match provider with
      | Cli.Github -> (module Providers.Github.Github)
      | _ ->
          failwith
            ("Hosting " ^ Cli.show_providers provider ^ " is unimplemented")
    in
    match action with
    | Cli.Install -> P.install ~query
    | Cli.Search -> P.search ~query ~ty
  in
  run

module App = Cli.CLI (struct
  let run = run ()
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
  App.main ()
