(* open Pkgman *)

let run () =
  let run action query provider =
    let (module P : Providers.Types.Provider) =
      match provider with
      | Cli.Github -> (module Providers.Github.Github)
      | _ ->
          failwith
            ("Hosting " ^ Cli.show_providers provider ^ " is unimplemented")
    in
    match action with
    | Cli.Install -> P.install ~query ~ty:true
    | Cli.Search -> P.search ~query ~ty:true
  in
  run

module App = Cli.CLI (struct
  let run = run ()
end)

let () =
  (* remove this line *)
  print_newline ();
  App.main ()

(* let _ = Github.install Sys.argv.(1) in
   (* let _ = Github.render_progress 1482434 in *)
   let _ = exit 0 in
   let open Pkgman.Cli_old in
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
   | `Search -> Ops.search (List.hd packages) (* handle list *)
   | `Install -> Ops.install packages
   | `List -> Ops.list ()
   | `Init -> Arg.usage specs usage
   | `Version -> (
       Build_info.V1.(
         match version () with
         | Some v -> print_endline ("v" ^ Version.to_string v)
         | None -> print_endline "v0.1")) *)
