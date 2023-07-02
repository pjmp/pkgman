(* https://github.com/mirage/ocaml-cohttp/blob/16e991ec1f7e5f0c99615cd1f58b99b02e3d0499/README.md?plain=1#L274 *)

let ( =: ) a b = Array.fill a (Array.length a - 1) 1 b

let rec get ?(download = false) ?(max_redirects = 5) url =
  let headers =
    [|
      (* ("Accept", "application/vnd.github.text-match+json"); *)
      ("X-GitHub-Api-Version", "2022-11-28");
      (* ("User-Agent", "User agent"); *)
    |]
  in
  let _ =
    if download then headers =: ("Accept", "application/octet-stream");

    match Sys.getenv_opt "GITHUB_PERSONAL_TOKEN" with
    | Some token -> headers =: ("Authorization", "Bearer " ^ token)
    | _ -> ()
  in

  let headers = Cohttp.Header.of_list (Array.to_list headers) in

  let open Lwt.Syntax in
  let* ans = Cohttp_lwt_unix.Client.get ~headers url in
  follow_redirect ~max_redirects url ans

and follow_redirect ~max_redirects request_uri (response, body) =
  let open Lwt.Syntax in
  let status = Cohttp.Response.status response in
  (* The unconsumed body would otherwise leak memory *)
  let* () =
    if status <> `OK then Cohttp_lwt.Body.drain_body body else Lwt.return_unit
  in
  match status with
  | `OK -> Lwt.return (response, body)
  | `Permanent_redirect | `Moved_permanently ->
      handle_redirect ~permanent:true ~max_redirects request_uri response
  | `Found | `Temporary_redirect ->
      handle_redirect ~permanent:false ~max_redirects request_uri response
  | `Not_found | `Gone -> Lwt.fail_with "Not found"
  | status ->
      Lwt.fail_with
        (Printf.sprintf "Unhandled status: %s"
           (Cohttp.Code.string_of_status status))

and handle_redirect ~permanent ~max_redirects request_uri response =
  if max_redirects <= 0 then Lwt.fail_with "Too many redirects"
  else
    let location =
      Cohttp.Header.get (Cohttp.Response.headers response) "location"
    in
    match location with
    | None -> Lwt.fail_with "Redirection without Location header"
    | Some url ->
        let open Lwt.Syntax in
        let uri = Uri.of_string url in
        let* () =
          if permanent then
            Logs_lwt.warn (fun m ->
                m "Permanent redirection from %s to %s"
                  (Uri.to_string request_uri)
                  url)
          else Lwt.return_unit
        in
        get uri ~max_redirects:(max_redirects - 1)
