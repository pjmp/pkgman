open Types
open Lwt

[@@@warning "-27-26"]

type gh_asset = { url : string; name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type gh_assets = { assets : gh_asset list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type gh_license = { name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type gh_response = {
  description : string option;
  full_name : string;
  html_url : string;
  language : string option;
  license : gh_license option;
  name : string;
  stargazers_count : int;
  updated_at : string;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type gh_responses = { items : gh_response list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

let print_tree query pkgs =
  let cols = Option.value ~default:80 (Terminal_size.get_columns ()) - 10 in
  let writer = Wrapper.make ~break_long_words:true ~drop_whitespace:true cols in
  let module B = PrintBox in
  let tree =
    pkgs
    |> List.map (fun pkg ->
           let branch = B.text (" " ^ pkg.name) in

           let leaves =
             Printf.sprintf " %s\n %s\n %s\n %s" pkg.full_name
               (Wrapper.fill writer
                  (Option.value ~default:"Description: N/A" pkg.description))
               ("License: "
               ^ match pkg.license with Some l -> l.name | _ -> "N/A")
               pkg.html_url
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

module Github : Provider = struct
  type t = { name : string }

  let t = { name = "github" }

  let search ~query ~ty =
    let t = if ty then "repositories" else "org" in
    let url =
      Printf.sprintf
        "https://api.github.com/search/%s?per_page=10&sort=best&q=%s" t query
      |> Uri.of_string
    in
    let json =
      Lwt_main.run
        (Fetcher.get url >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string)
    in
    let json = Yojson.Safe.from_string json in
    let res = gh_responses_of_yojson json in
    print_tree query res.items

  let install ~query ~ty = ()
end
