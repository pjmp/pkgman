open Pkgman
open Pkgman.Cli

let () =
  print_newline ();
  let { packages; action } = parse () in

  let _ =
    if List.length packages = 0 then
      match action with
      | `Search | `Install ->
          Printf.eprintf "%s\n" usage;
          exit 1
      | `Version | `Init | `List -> ()
  in

  match action with
  | `Search -> Ops.search (List.hd packages)
  | `Install -> Ops.install packages
  | `List -> Ops.list ()
  | `Version -> print_endline "v0.1"
  | `Init -> Arg.usage specs usage
