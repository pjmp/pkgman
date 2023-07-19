type print_type = {
  description : string option;
  repo_name : string;
  url : string;
  license : string option;
  name : string;
}

let print_tree : string -> print_type list -> unit =
 fun query pkgs ->
  let cols = Option.value ~default:80 (Terminal_size.get_columns ()) - 10 in
  let writer = Wrapper.make ~break_long_words:true ~drop_whitespace:true cols in
  let module B = PrintBox in
  let tree =
    pkgs
    |> List.map (fun (pkg : print_type) ->
           let branch = B.text (" " ^ pkg.name) in

           let leaves =
             Printf.sprintf " %s\n %s\n %s\n %s" pkg.repo_name
               (Wrapper.fill writer
                  (Option.value ~default:"Description: N/A" pkg.description))
               ("License: "
               ^
               match pkg.license with
               | Some name -> name
               | _ -> "N/A")
               pkg.url
           in
           B.tree branch [ B.text leaves ])
  in
  PrintBox_text.output stdout
    (B.tree
       (B.text
          (Printf.sprintf "%d repo(s) matched the query: '%s'"
             (List.length pkgs) query))
       tree);
  print_newline ()
