let app_dir () =
  let module Dirs = Directories.Base_dirs () in
  (* let exe_dir =
       (fun dir -> Some (Filename.dirname dir))
       |> Option.bind Dirs.executable_dir
       |> Option.value ~default:".local"
     in *)
  let dir =
    Option.bind Dirs.home_dir (fun home ->
        let dir =
          List.fold_right Filename.concat [ home; ".local"; "pkgman" ] ""
        in
        Some dir)
  in

  match dir with
  | Some dir ->
      let _ = FileUtil.mkdir dir in
      Unix.realpath dir
  | None -> failwith "HOME directory not found"

let has ext filename =
  try
    let ext = Printf.sprintf "^*.%s$" ext in
    let _ = Str.search_forward (Str.regexp ext) filename 0 in
    true
  with
  | _ -> false

let exec filename repo =
  let cmd =
    Option.bind
      ([
         (".tar.gz", "tar --strip-components=1 -C " ^ repo ^ " -xzf " ^ filename);
         (".tar", "tar --strip-components=1 -C " ^ repo ^ " -xf " ^ filename);
         (".zip", "unzip ");
       ]
      |> List.find_opt (fun ext -> has (fst ext) filename))
      (fun (_, a) -> Some a)
  in

  match cmd with
  | Some cmd ->
      FileUtil.mkdir repo;
      Sys.command cmd |> ignore;

      let dir = Filename.concat (app_dir ()) repo in

      if Sys.file_exists dir then FileUtil.rm ~recurse:true [ dir ]
      else
        Filename.concat (app_dir ()) repo
        |> FileUtil.mv ~force:FileUtil.Force repo
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

let is_supported file =
  [| ".tar.gz"; ".tar"; ".zip" |] |> Array.exists (fun ext -> has ext file)
