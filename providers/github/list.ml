open Pkgman_common

type item = {
  created_at : string;
  draft : bool;
  html_url : string;
  name : string;
  prerelease : bool;
  published_at : string;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show { with_path = false }]

type response = item list [@@deriving yojson, show]

let is_executable file_path =
  let open Unix in
  let open_result = openfile file_path [ O_RDONLY ] 0 in
  let file_stat = fstat open_result in
  let permissions = file_stat.st_perm in
  let is_executable = permissions land 0o100 <> 0 in
  close open_result;
  is_executable

let get _ lty query =
  let print_tree box = box |> PrintBox_text.output stdout |> print_newline in

  let module B = PrintBox in
  Interface.(
    match lty with
    | Installed ->
        let app_dir = Utils.app_dir () in

        let _ = Sys.chdir app_dir in

        let dir = Sys.readdir app_dir in

        let is_dir p =
          try Sys.is_directory p with
          | _ -> false
        in

        let cannonize parent child =
          child
          |> Filename.concat (Filename.concat app_dir parent)
          |> Unix.realpath
        in

        let o =
          dir |> Array.to_list
          |> Stdlib.List.filter_map (fun path ->
                 match is_dir path with
                 | false -> if is_executable path then Some path else None
                 | true ->
                     Sys.readdir path
                     |> Array.find_opt (fun inner_path ->
                            let full_path = cannonize path inner_path in
                            (not @@ is_dir full_path) && is_executable full_path))
        in

        o |> String.concat "\n" |> print_endline;

        let _ =
          dir
          |> Array.iter (fun path ->
                 if is_dir path then
                   Sys.readdir path
                   |> Array.iter (fun inner_path ->
                          let full_path = cannonize path inner_path in

                          if is_dir full_path |> not && is_executable full_path
                          then print_endline (full_path ^ " is executable"))
                 else if is_dir path |> not && is_executable path then
                   print_endline (path ^ " is executable"))
        in

        let body =
          dir
          |> Array.mapi (fun i path ->
                 [| B.sprintf "%d" (i + 1); B.sprintf "%s" path |])
        in
        Array.append body
          [| [| B.sprintf "Total"; B.sprintf "%d" (Array.length dir) |] |]
        |> B.grid |> B.frame |> print_tree
    | Available -> (
        match query with
        | Some query ->
            Printf.sprintf "https://api.github.com/repos/%s/releases" query
            |> Fetcher.get_body |> Lwt_main.run |> Yojson.Safe.from_string
            |> response_of_yojson
            |> Stdlib.List.map (fun res ->
                   B.tree
                     (B.text (res.name |> String.trim))
                     [
                       B.text ("Created at: " ^ res.created_at);
                       B.text ("Published at: " ^ res.published_at);
                       B.text ("Draft?: " ^ (res.draft |> string_of_bool));
                       B.text ("Link: " ^ res.html_url);
                     ])
            |> B.tree (B.text query)
            |> print_tree
        | None -> failwith "query is required"))
