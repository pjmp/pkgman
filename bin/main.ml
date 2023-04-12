open Pkgman
open Pkgman.Cli

let () =
  print_newline ();
  let { packages; action } = parse () in

  let _ =
    if List.length packages = 0 then
      match action with
      | `Search | `Install ->
          print_endline "is empty";
          exit 1
      | `Version | `Init -> ()
  in

  match action with
  | `Search -> Ops.search (List.hd packages)
  | `Install -> Ops.install packages
  | `Version -> print_endline "v0.1"
  | `Init -> Arg.usage specs usage
