open Pkgman_common

type user = { login : string; html_url : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type search_user = { items : user list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type license = { name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type repo = {
  description : string option;
  full_name : string;
  html_url : string;
  language : string option;
  license : license option;
  name : string;
  stargazers_count : int;
  updated_at : string;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type search_repo = { items : repo list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

let get (_ : Interface.common_opts) (opts : Interface.opts) =
  let search_ty =
    match opts.search_type with
    | Users -> "users"
    | Repo -> "repositories"
  in

  let res =
    Printf.sprintf
      "https://api.github.com/search/%s?per_page=100&sort=best&q=%s" search_ty
      opts.query
    |> Fetcher.get_body |> Lwt_main.run |> Yojson.Safe.from_string
  in

  Printer.(
    match opts.search_type with
    | Repo ->
        let nodes = res |> search_repo_of_yojson in

        print_tree opts.query
          (nodes.items
          |> Stdlib.List.map (fun (t : repo) ->
                 {
                   description = t.description;
                   repo_name = t.full_name;
                   name = t.name;
                   url = t.html_url;
                   license = Option.bind t.license (fun l -> Some l.name);
                 }))
    | Users ->
        let nodes = res |> search_user_of_yojson in

        print_tree opts.query
          (nodes.items
          |> Stdlib.List.map (fun (t : user) ->
                 {
                   description = None;
                   repo_name = t.html_url;
                   name = t.login;
                   url = t.html_url;
                   license = None;
                 })))
