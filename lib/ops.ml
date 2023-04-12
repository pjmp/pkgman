type package = {
  description : string;
  file : string;
  homepage : string;
  install : string;
  name : string;
  postinstall : string;
  url : string;
  version : string;
}
[@@deriving of_yojson, show]

type package_manager = {
  packages : package list;
  prefix : string;
  version : string;
}
[@@deriving of_yojson, show]

let dir_exist d = try Sys.is_directory d with _ -> false

let db_path () =
  let module B = Directories.Base_dirs () in
  let module U = Directories.User_dirs () in
  let app = "pkgman" in

  let file = "db.json" in

  let dir =
    Option.bind B.config_dir (fun path -> Some (Filename.concat path app))
  in

  let dir = Option.value dir ~default:app in

  let _ = if not (dir_exist dir) then Sys.mkdir dir 0O755 in

  let full_path = Filename.concat dir file in

  let _ =
    if not (Sys.file_exists full_path) then (
      Printf.eprintf "'%s' not found, please create a db file\n" full_path;
      exit 1)
  in

  full_path

let print_table packages =
  let module B = PrintBox in
  let table =
    let body =
      Array.of_list packages
      |> Array.map (fun pkg ->
             [|
               B.sprintf "%s" pkg.name;
               B.sprintf "%s" pkg.version;
               B.sprintf "%s" pkg.homepage;
               B.sprintf "%s" pkg.description;
             |])
    in

    let header =
      [|
        [|
          B.sprintf "%s" "Name";
          B.sprintf "%s" "Version";
          B.sprintf "%s" "Homepage";
          B.sprintf "%s" "Description";
        |];
      |]
    in
    Array.append header body
  in
  PrintBox_text.output stdout (table |> B.grid |> B.frame);
  print_newline ()

let open_db =
  let json = Yojson.Safe.from_file (db_path ()) in

  let pm = package_manager_of_yojson json in

  match pm with
  | Error err ->
      print_endline ("Error parsing `db.json` file: " ^ err);
      None
  | Ok db -> Some db

let search query =
  match open_db with
  | Some db ->
      let matches =
        List.filter
          (fun pkg ->
            (* https://stackoverflow.com/a/8373836 *)
            let contains needle haystick =
              let re = Str.regexp_string_case_fold needle in
              try
                ignore (Str.search_forward re haystick 0);
                true
              with Not_found -> false
            in
            contains query pkg.name || contains query pkg.description)
          db.packages
      in
      if matches = [] then print_endline "no matches" else print_table matches
  | None -> ()

let install pkgs =
  match open_db with
  | None -> ()
  | Some db -> (
      let matches =
        List.filter
          (fun pkg -> List.exists (fun query -> pkg.name = query) pkgs)
          db.packages
      in

      match matches with
      | [] -> print_endline "no matchs"
      | matches ->
          let () = Sys.chdir (Filename.get_temp_dir_name ()) in
          List.map
            (fun pkg ->
              Domain.spawn (fun _ ->
                  print_endline ("Name: " ^ pkg.name);
                  let code = Sys.command pkg.install in
                  print_endline ("Exited with: " ^ string_of_int code)))
            matches
          |> List.iter Domain.join)

let list () =
  match open_db with None -> () | Some db -> print_table db.packages
