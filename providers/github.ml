open Lwt
open Types

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

let get_cmd filename =
  let has e =
    try
      let ext = Printf.sprintf "^*.%s$" e in
      let _ = Str.search_forward (Str.regexp ext) filename 0 in
      true
    with _ -> false
  in

  let c = [ (".tar.gz", "tar xzf"); (".tar", "tar xf"); (".zip", "unzip") ] in

  let file = c |> List.find_opt (fun i -> has (fst i)) in

  match file with
  | Some file -> snd file
  | _ -> failwith (filename ^ " is not supported")

module Github : Provider = struct
  type t = { name : string }

  [@@@warning "-32"]

  let t = { name = "github" }

  let search ~query ~ty =
    let t = match ty with `Users -> "users" | `Repo -> "repositories" in
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

  let install ~query ~ty =
    let url =
      Printf.sprintf "https://api.github.com/repos/%s/releases/latest" query
      |> Uri.of_string
    in
    let json =
      Lwt_main.run
        (Fetcher.get url >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string)
    in
    let json = Yojson.Safe.from_string json in
    let res = gh_assets_of_yojson json in

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

    let dir = Sys.getcwd () |> Unix.realpath |> Filename.concat in

    let filename = dir name in

    print_endline filename;

    let _ =
      Lwt_main.run
        ( Fetcher.get ~download:true (url |> Uri.of_string)
        >>= fun (_resp, body) ->
          let x = Cohttp.Header.get _resp.headers "Content-Length" in
          let length = int_of_string (Option.get x) in
          (* let x = Cohttp.Header.to_string _resp.headers in
             let _ = print_endline x in *)
          let stream = Cohttp_lwt.Body.to_stream body in
          Lwt_io.with_file ~mode:Lwt_io.output filename (fun chan ->
              Lwt_stream.iter_s (fun o -> Lwt_io.write chan o) stream) )
    in

    let exit_code = Sys.command (Printf.sprintf "%s %s" (get_cmd name) name) in

    print_endline ("Exited with: " ^ string_of_int exit_code);

    ()
end
