open Lwt

(* Search repositories
   https://docs.github.com/en/rest/search?apiVersion=2022-11-28#search-repositories
   curl -L \
     -H "Accept: application/vnd.github+json" \
     -H "Authorization: Bearer <YOUR-TOKEN>"\
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/search/repositories?q=Q *)

(* List releases
   https://docs.github.com/en/rest/releases/releases?apiVersion=2022-11-28#list-releases
   curl -L \
     -H "Accept: application/vnd.github+json" \
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/repos/cli/cli/releases *)

(* Get the latest release
   https://docs.github.com/en/rest/releases/releases?apiVersion=2022-11-28#get-the-latest-release
   curl -L \
     -H "Accept: application/vnd.github+json" \
     -H "Authorization: Bearer <YOUR-TOKEN>"\
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/repos/OWNER/REPO/releases/latest *)

(* Get a release asset or download
   https://docs.github.com/en/rest/releases/assets?apiVersion=2022-11-28#get-a-release-asset
   curl -L \
     -H "Authorization: Bearer <YOUR-TOKEN>"\
     -H "X-GitHub-Api-Version: 2022-11-28" \
     https://api.github.com/repos/OWNER/REPO/releases/assets/ASSET_ID

     curl -L -O \
     -H "Authorization: Bearer <YOUR-TOKEN>"\
     -H "X-GitHub-Api-Version: 2022-11-28" \
     -H "Accept: application/octet-stream" \
     https://api.github.com/repos/cli/cli/releases/assets/105219516
*)

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

let write_strings_to_file lines =
  let oc = open_out "filename.json" in
  let _ = Printf.fprintf oc "%s\n" lines in
  close_out oc

let print_table (packages : gh_response list) =
  let cols = Option.value ~default:80 (Terminal_size.get_columns ()) - 10 in
  let _writer = Wrapper.make ~break_long_words:true cols in
  let module B = PrintBox in
  let table =
    let body =
      Array.of_list packages
      |> Array.map (fun pkg ->
             [|
               B.sprintf "%s" pkg.name;
               B.sprintf "%s" pkg.html_url;
               B.sprintf "%s"
                 (match pkg.description with
                 | None -> "N/A"
                 | Some desc ->
                     if String.length desc > 40 then
                       String.sub desc 0 40 ^ " ... "
                     else desc);
               B.sprintf "%s" (Option.value ~default:"N/A" pkg.language);
               B.sprintf "%s"
                 (match pkg.license with Some l -> l.name | _ -> "N/A");
             |])
    in

    let header =
      [|
        [|
          B.sprintf "%s" "Name";
          B.sprintf "%s" "Repo";
          B.sprintf "%s" "Description";
          B.sprintf "%s" "Language";
          B.sprintf "%s" "License";
        |];
      |]
    in
    Array.append header body
  in
  PrintBox_text.output stdout (table |> B.grid |> B.frame);
  print_newline ()

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

           (* let leaves =
                [|
                  B.text (" " ^ pkg.full_name);
                  B.text
                    (" "
                    ^ Wrapper.fill writer
                        (Option.value ~default:"Description: N/A" pkg.description)
                    );
                  B.text
                    (" License: "
                    ^ match pkg.license with Some l -> l.name | _ -> "N/A");
                  B.text (" " ^ pkg.html_url);
                |]
              in *)

           (* let _ =
                match pkg.language with
                | Some lang -> Array.fill leaves 1 1 (B.text (" " ^ lang))
                | _ -> ()
              in *)
           (* let _ =
                match pkg.topics with
                | Some topics ->
                    Array.fill leaves 1 1
                      (B.text (" " ^ String.concat ", " topics))
                | _ -> ()
              in *)
           B.tree branch [ B.text leaves ])
  in
  PrintBox_text.output stdout
    (B.tree
       (B.text
          (Printf.sprintf "%d repo(s) matched the query: '%s'"
             (List.length pkgs) query))
       tree);
  print_newline ()

let print_apt pkgs =
  let cols = Option.value ~default:80 (Terminal_size.get_columns ()) - 10 in
  let writer = Wrapper.make ~break_long_words:true ~drop_whitespace:true cols in
  pkgs
  |> List.iter (fun pkg ->
         Printf.printf
           "%s\n  %s\n  Repository: %s\n  Language: %s\n  License: %s\n\n"
           pkg.full_name
           (Wrapper.fill writer
              (Option.value ~default:"Description: N/A" pkg.description))
           pkg.html_url
           (Option.value ~default:"N/A" pkg.language)
           (match pkg.license with Some l -> l.name | _ -> "N/A"))

let search ?(ty = "repositories") query =
  let url =
    Printf.sprintf "https://api.github.com/search/%s?per_page=10&sort=best&q=%s"
      ty query
    |> Uri.of_string
  in
  let _ = print_endline (Uri.to_string url) in
  let json =
    Lwt_main.run
      (Fetcher.get url >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string)
  in
  (* let _ = print_string json in *)
  (* let _ = write_strings_to_file json in *)
  let json = Yojson.Safe.from_string json in
  let res = gh_responses_of_yojson json in
  print_tree query res.items

(* print_table res.items *)
(* print_apt res.items *)

let bar ~total =
  let open Progress.Line in
  list
    [
      spinner ~color:(Progress.Color.ansi `green) (); bar total; count_to total;
    ]

let render_progress total current =
  print_newline ();
  Progress.with_reporter (bar ~total:!total) (fun f ->
      f current;
      if current = !total then
        Progress.interject_with (fun () -> print_endline ":: Finished "))

(* curl -L -H 'Accept: application/octet-stream' -H 'X-GitHub-Api-Version: 2022-11-28' 'https://api.github.com/repos/ouch-org/ouch/releases/assets/90656791' -O *)
let install repo =
  let url =
    repo
    |> Printf.sprintf "https://api.github.com/repos/%s/releases/latest"
    |> Uri.of_string
  in
  let json =
    Lwt_main.run
      (Fetcher.get url >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string)
  in
  let _ = write_strings_to_file json in

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

  (* let%lwt = 1 in *)

  (* let open Lwt.Syntax in *)
  (* let open Lwt in *)

  (* let f =  Fetcher.get ~download:true (url |> Uri.of_string) in *)
  (* let* _resp, body =  Fetcher.get ~download:true (url |> Uri.of_string) in *)
  let _ =
    Lwt_main.run
      ( Fetcher.get ~download:true (url |> Uri.of_string)
      >>= fun (_resp, body) ->
        let x = Cohttp.Header.get _resp.headers "Content-Length" in
        let length = int_of_string (Option.get x) in
        (* let x = Cohttp.Header.to_string _resp.headers in
           let _ = print_endline x in *)
        let stream = Cohttp_lwt.Body.to_stream body in
        Lwt_io.with_file ~mode:Lwt_io.output name (fun chan ->
            Lwt_stream.iter_s
              (fun o ->
                (* let _ =
                     print_int (Int64.to_int (Lwt_io.position chan))
                   in *)
                let _ =
                  render_progress (ref length)
                    (Int64.to_int (Lwt_io.position chan))
                in
                Lwt_io.write chan o)
              stream) )
  in
  ()
