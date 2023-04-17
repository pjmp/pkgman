type act = [ `Search | `Install | `Version | `Init | `List ]
type opts = { packages : string list; action : act }

let usage = "usage: [-install] [-search] [-list] <pkg> [<pkg1>] ... [-version]"
let packages = ref []
let action : act ref = ref `Init

let specs =
  [
    ("-v", Arg.Unit (fun _ -> action := `Version), "Print version information");
    ("-i", Arg.Unit (fun _ -> action := `Install), "Install a package(s)");
    ("-s", Arg.Unit (fun _ -> action := `Search), "Search for a package(s)");
    ("-l", Arg.Unit (fun _ -> action := `List), "List available packages");
    (* ("-c", Arg.Unit (fun _ -> action := `Update), "Check for updates"); *)
  ]

let query_fn package = packages := package :: !packages

let parse () =
  let _ = Arg.parse specs query_fn usage in
  { packages = !packages; action = !action }
