open Pkgman_common

type asset = { url : string; name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type gh_repo = { name : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show { with_path = false }]

type gh_install = { assets : asset list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show { with_path = false }]

let get (_ : Interface.common_opts) (opts : Interface.opts) =
  let repo, version =
    match String.split_on_char '@' opts.query with
    | [ repo ] -> (repo, "latest")
    | [ repo; version ] -> (repo, version)
    | _ -> failwith "unexpected query"
  in

  let repo_name =
    let res =
      Printf.sprintf "https://api.github.com/repos/%s" repo
      |> Fetcher.get_body |> Lwt_main.run |> Yojson.Safe.from_string
      |> gh_repo_of_yojson
    in
    res.name
  in

  let res =
    let url =
      Printf.sprintf
      @@
      if version = "latest" then "https://api.github.com/repos/%s/releases/%s"
      else "https://api.github.com/repos/%s/releases/tags/%s"
    in

    url repo version |> Fetcher.get_body |> Lwt_main.run
    |> Yojson.Safe.from_string |> gh_install_of_yojson
  in

  if Stdlib.List.length res.assets = 0 then
    failwith "no assets available to download";

  let result =
    Inquirer_oc.(
      {
        name = "asset";
        prompt_type = List;
        message = "Please select asset to download";
        choices =
          res.assets
          |> Stdlib.List.map (fun (a : asset) ->
                 let asset : Question_list_type.choice =
                   { word = a.name; value = a.url }
                 in
                 asset);
        page_size = Some (Stdlib.List.length res.assets);
      }
      |> Question_list.list_question)
  in

  let { url; name } =
    res.assets |> Stdlib.List.find (fun a -> a.url = result)
  in

  let _ =
    let supported = Utils.is_supported name in
    if not supported then failwith (name ^ " has unsupported extension")
  in

  let _ = Filename.get_temp_dir_name () |> Sys.chdir in

  let filename =
    let dir = Sys.getcwd () |> Unix.realpath |> Filename.concat in
    dir name
  in

  let open Lwt in
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

  let _ = Lwt_main.run (Lwt.pick [ download; Utils.spinner ~message:name ]) in

  print_newline ();

  let _ = Utils.exec name repo_name in
  ()
