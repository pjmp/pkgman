module Pkgman = Pkgman_common

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

let get _ lty query =
  Pkgman.Interface.(
    match lty with
    | Installed ->
        let dir = Pkgman.Utils.app_dir () |> Sys.readdir in
        let module B = PrintBox in
        let body =
          dir
          |> Array.mapi (fun i path ->
                 [| B.sprintf "%d" (i + 1); B.sprintf "%s" path |])
        in
        Array.append body
          [| [| B.sprintf "Total"; B.sprintf "%d" (Array.length dir) |] |]
        |> B.grid |> B.frame
        |> PrintBox_text.output stdout
        |> print_newline
    | Available -> (
        match query with
        | Some query ->
            Printf.sprintf "https://api.github.com/repos/%s/releases" query
            |> Pkgman.Fetcher.get_body |> Lwt_main.run
            |> Yojson.Safe.from_string |> response_of_yojson
            |> Stdlib.List.map (fun res ->
                   PrintBox.tree
                     (PrintBox.text (res.name |> String.trim))
                     [
                       PrintBox.text ("Created at: " ^ res.created_at);
                       PrintBox.text ("Published at: " ^ res.published_at);
                       PrintBox.text ("Draft?: " ^ (res.draft |> string_of_bool));
                       PrintBox.text ("Link: " ^ res.html_url);
                     ])
            |> PrintBox.tree (PrintBox.text query)
            |> PrintBox_text.output stdout
            |> print_newline
        | None -> failwith "query is required"))
