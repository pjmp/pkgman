module App = Cli.CLI (struct
  open Pkgman_common.Interface

  let get_mod : providers -> (module Provider) =
   fun p ->
    match p with
    | Github -> (module Providers.Github)
    | _ -> failwith ("Provider " ^ show_providers p ^ " is unimplemented")

  let list (co : common_opts) ty q =
    let (module P : Provider) = get_mod co.provider in
    P.list co ty q

  let search (co : common_opts) (opts : opts) =
    let (module P : Provider) = get_mod co.provider in
    P.search co opts

  let install (co : common_opts) (opts : opts) =
    let (module P : Provider) = get_mod co.provider in
    P.install co opts
end)

(*
   https://gitlab.manjaro.org/packages/core/bash/-/blob/master/dot.bashrc
   *.7z)        7z x $1      ;;
   *.bz2)       bunzip2 $1   ;;
   *.gz)        gunzip $1    ;;
   *.rar)       unrar x $1   ;;
   *.tar.bz2)   tar xjf $1   ;;
   *.tar.gz)    tar xzf $1   ;;
   *.tar)       tar xf $1    ;;
   *.tbz2)      tar xjf $1   ;;
   *.tgz)       tar xzf $1   ;;
   *.Z)         uncompress $1;;
   *.zip)       unzip $1     ;;
*)

let () =
  let _ = Printexc.record_backtrace true in
  print_newline ();

  App.main ()
