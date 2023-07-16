module Pkgman = Pkgman_lib
module Types = Pkgman_common.Interface

module App = Cli.CLI (struct
  open Types

  let get_mod : providers -> (module Provider) =
   fun p ->
    match p with
    | Github -> (module Pkgman.Github)
    | _ -> failwith ("Provider " ^ show_providers p ^ " is unimplemented")

  let list (co : common_opts) lty =
    let (module P : Provider) = get_mod co.provider in
    P.list co lty

  let search (co : common_opts) (opts : opts) =
    let (module P : Provider) = get_mod co.provider in
    P.search co opts

  let install (co : common_opts) (opts : opts) =
    let (module P : Provider) = get_mod co.provider in
    P.install co opts
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
