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
        Utils.app_dir () |> Sys.readdir
        |> Array.iteri (fun i p -> Printf.printf "%d - %s\n" (i + 1) p)
    | Available -> (
        match query with
        | Some query ->
            Printf.sprintf "https://api.github.com/repos/%s/releases" query
            |> Fetcher.get_body |> Lwt_main.run |> Yojson.Safe.from_string
            |> response_of_yojson
            |> Stdlib.List.map (fun res ->
                   B.tree
                     (B.text (res.name |> String.trim))
                     [ B.text res.html_url ])
            |> B.tree (B.text query)
            |> print_tree
        | None -> failwith "query is required"))
