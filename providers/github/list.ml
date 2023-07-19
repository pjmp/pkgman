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

let get _ lty query =
  let print_tree box = box |> PrintBox_text.output stdout |> print_newline in

  let module B = PrintBox in
  Interface.(
    match lty with
    | Installed ->
        let dir = Utils.app_dir () |> Sys.readdir in
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
