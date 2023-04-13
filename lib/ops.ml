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
      Printf.eprintf "Error parsing `db.json` file: %s" err;
      None
  | Ok db -> Some db

let exec cmd =
  match Unix.system cmd with
  (* | WEXITED code | WSIGNALED code | WSTOPPED code ->
      print_endline ("Exited with: " ^ string_of_int code) *)
  | _ -> ()

let prompt ?(msg = "Continue") () =
  Printf.printf "%s [y/N]: " msg;

  let ans = read_line () in

  if String.lowercase_ascii ans = "y" then () else exit 1

let search query =
  match open_db with
  | Some db ->
      (* https://stackoverflow.com/a/8373836 *)
      let contains haystick =
        let re = Str.regexp_string_case_fold query in
        try
          ignore (Str.search_forward re haystick 0);
          true
        with Not_found -> false
      in

      let matches =
        List.filter
          (fun pkg -> contains pkg.name || contains pkg.description)
          db.packages
      in
      if matches = [] then Printf.eprintf "No matches found for: %s" query
      else print_table matches
  | None -> ()

let install pkgs =
  match open_db with
  | None -> ()
  | Some db ->
      let found, not_found =
        pkgs
        |> List.partition_map (fun query ->
               match List.find_opt (fun a -> a.name = query) db.packages with
               | Some pkg -> Either.Left pkg
               | None -> Either.Right query)
      in

      if not_found <> [] then (
        Printf.eprintf "No matches found for: %s\n\n"
          (String.concat ", " not_found);
        flush_all ());

      if found <> [] then
        let _ = print_table found in
        let _ = Printf.printf "Install %d package(s)\n\n" (List.length found) in
        let _ = prompt () in
        let () = Sys.chdir (Filename.get_temp_dir_name ()) in
        List.map
          (fun pkg ->
            Domain.spawn (fun _ ->
                print_endline ("Name: " ^ pkg.name);

                let _ = exec pkg.install in

                if pkg.postinstall <> String.empty then exec pkg.postinstall))
          found
        |> List.iter Domain.join

let list () =
  match open_db with None -> () | Some db -> print_table db.packages
