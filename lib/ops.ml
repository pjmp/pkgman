type package = {
  file : string;
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

let open_db =
  let module U = Directories.Base_dirs () in
  let _ = print_endline (Option.get U.cache_dir) in

  let json = Yojson.Safe.from_file "db.json" in
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
          (fun p -> Str.string_match (Str.regexp_string query) p.name 0)
          db.packages
      in
      if matches = [] then print_endline "no matches"
      else
        let module B = PrintBox in
        let table =
          let body =
            Array.of_list matches
            |> Array.map (fun pkg ->
                   [|
                     B.sprintf "%s" pkg.name;
                     B.sprintf "%s" pkg.version;
                     B.sprintf "%s" pkg.url;
                   |])
          in
          let header =
            [|
              [|
                B.sprintf "%s" "name";
                B.sprintf "%s" "version";
                B.sprintf "%s" "url";
              |];
            |]
          in
          Array.append header body
        in
        PrintBox_text.output stdout (table |> B.grid |> B.frame);
        print_newline ()
  | None -> ()

let install pkgs =
  match open_db with
  | None -> ()
  | Some db -> (
      let matches =
        List.filter
          (fun pkg -> List.exists (fun q -> pkg.name = q) pkgs)
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
