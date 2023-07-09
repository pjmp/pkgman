open Lwt
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

let print_tree : string -> gh_response list -> unit =
 fun query pkgs ->
  let cols = Option.value ~default:80 (Terminal_size.get_columns ()) - 10 in
  let writer = Wrapper.make ~break_long_words:true ~drop_whitespace:true cols in
  let module B = PrintBox in
  let tree =
    pkgs
    |> List.map (fun (pkg : gh_response) ->
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

let mkdir dir =
  try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ()

let app_dir () =
  let module H = Directories.Base_dirs () in
  let dir =
    Option.bind H.home_dir (fun home ->
        let dir =
          List.fold_right Filename.concat [ home; ".local"; "pkgman" ] ""
        in
        Some dir)
  in

  match dir with
  | Some dir ->
      let _ = mkdir dir in
      Unix.realpath dir
  | None -> failwith "HOME directory not found"

let exec filename repo =
  let has ext =
    try
      let ext = Printf.sprintf "^*.%s$" ext in
      let _ = Str.search_forward (Str.regexp ext) filename 0 in
      true
    with _ -> false
  in

  let cmd =
    Option.bind
      ([
         (".tar.gz", "tar --strip-components=1 -C " ^ repo ^ " -xzf " ^ filename);
         (".tar", "tar --strip-components=1 -C " ^ repo ^ " -xf " ^ filename);
         (".zip", "unzip ");
       ]
      |> List.find_opt (fun ext -> has (fst ext)))
      (fun (_, a) -> Some a)
  in

  match cmd with
  | Some cmd ->
      mkdir repo;
      Sys.command cmd |> exit
  | _ -> failwith (filename ^ " is not supported")

module Github : Provider = struct
  type t = { name : string }

  let search ~query ~ty =
    let ty = match ty with `Users -> "users" | `Repo -> "repositories" in
    let url =
      Printf.sprintf
        "https://api.github.com/search/%s?per_page=100&sort=best&q=%s" ty query
      |> Uri.of_string
    in
    let json =
      Lwt_main.run
        (Fetcher.get url >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string)
    in
    let json = Yojson.Safe.from_string json in
    let res = gh_responses_of_yojson json in
    print_tree query res.items

  let install ~query =
    let repo_url =
      Printf.sprintf "https://api.github.com/repos/%s" query |> Uri.of_string
    in

    let repo_name =
      let res =
        Fetcher.get repo_url >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
      in
      let json = Lwt_main.run res |> Yojson.Safe.from_string in
      let res = gh_repo_of_yojson json in
      res.name
    in

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
      Lwt_main.run (Lwt.pick [ download; Loading.spinner ~message:name ])
    in

    print_newline ();

    let _ = exec name repo_name in

    (* let _ = Sys.rename name repo_name in *)
    ()
end
