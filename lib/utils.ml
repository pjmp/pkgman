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
      Sys.command cmd |> ignore;
      Filename.concat (app_dir ()) repo |> FileUtil.mv repo
  | _ -> failwith (filename ^ " is not supported")

let spinner ~message =
  let open Lwt in
  let symbols = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |] in
  let rec loop i =
    let symbol = symbols.(i mod Array.length symbols) in
    Lwt_io.print ("\r" ^ symbol ^ " " ^ message) >>= fun () ->
    Lwt_unix.sleep 0.15 >>= fun () -> loop (i + 1)
  in
  loop 0
