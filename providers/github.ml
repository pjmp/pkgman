open Lwt
open Lwt.Syntax
open Types

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

type gh_repo = { name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson]

module Github : Provider = struct
  type t = { name : string }

  let get_body url =
    let body =
      let* _, body = url |> Uri.of_string |> Fetcher.get in
      Cohttp_lwt.Body.to_string body
    in
    body

  let search ~query ~ty =
    let ty = match ty with `Users -> "users" | `Repo -> "repositories" in

    let res =
      Printf.sprintf
        "https://api.github.com/search/%s?per_page=100&sort=best&q=%s" ty query
      |> get_body |> Lwt_main.run |> Yojson.Safe.from_string
      |> gh_responses_of_yojson
    in
    let open Pkgman.Printer in
    print_tree query
      (res.items
      |> List.map (fun (t : gh_response) ->
             {
               description = t.description;
               repo_name = t.full_name;
               name = t.name;
               url = t.html_url;
               license = Option.bind t.license (fun l -> Some l.name);
             }))

  let install ~query =
    let repo_name =
      let res =
        Printf.sprintf "https://api.github.com/repos/%s" query
        |> get_body |> Lwt_main.run |> Yojson.Safe.from_string
        |> gh_repo_of_yojson
      in
      res.name
    in

    let res =
      Printf.sprintf "https://api.github.com/repos/%s/releases/latest" query
      |> get_body |> Lwt_main.run |> Yojson.Safe.from_string
      |> gh_assets_of_yojson
    in

    let result =
      Inquirer_oc.(
        {
          name = "asset";
          prompt_type = List;
          message = "Please select asset to download";
          choices =
            res.assets
            |> List.map (fun (a : gh_asset) ->
                   let asset : Question_list_type.choice =
                     { word = a.name; value = a.url }
                   in
                   asset);
          page_size = Some (List.length res.assets);
        }
        |> Question_list.list_question)
    in

    let { url; name } = res.assets |> List.find (fun a -> a.url = result) in

    let _ = Filename.get_temp_dir_name () |> Sys.chdir in

    let filename =
      let dir = Sys.getcwd () |> Unix.realpath |> Filename.concat in
      dir name
    in

    let download =
      Fetcher.get ~download:true (url |> Uri.of_string) >>= fun (_, body) ->
      (* let total_file =
           match Cohttp.Header.get resp.headers "Content-Length" with
           | Some length -> Int64.of_string length
           | None -> 0L
         in *)
      let stream = Cohttp_lwt.Body.to_stream body in
      Lwt_io.with_file ~mode:Lwt_io.output filename (fun chan ->
          Lwt_stream.iter_s (fun out -> Lwt_io.write chan out) stream)
    in

    let _ =
      Lwt_main.run (Lwt.pick [ download; Pkgman.Utils.spinner ~message:name ])
    in

    print_newline ();

    let _ = Pkgman.Utils.exec name repo_name in
    ()
end
