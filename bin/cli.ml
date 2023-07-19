open Pkgman_common.Interface

module CLI (R : Provider) = struct
  open Cmdliner

  let lowercase str = str |> String.lowercase_ascii

  let query_t =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"Search for repo or org." ~docv:"QUERY")

  let provider_t =
    let opts =
      all_of_providers
      |> List.map (fun ty -> (show_providers ty |> lowercase, ty))
      |> Arg.enum
    in
    Arg.(
      value & opt opts Github
      & info [ "p"; "provider" ] ~doc:"Specify a provider." ~docv:"PROVIDER")

  let search_type_t =
    let opts =
      all_of_search_type
      |> List.map (fun ty -> (show_search_type ty |> lowercase, ty))
      |> Arg.enum
    in
    Arg.(
      value & opt opts Repo
      & info [ "t"; "type" ] ~doc:"Search by repo or users." ~docv:"TYPE")

  let verbose_t =
    let doc = "Print whatever pkgman is doing." in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

  let list_t =
    let parser =
      all_of_list_type
      |> List.map (fun lty ->
             let flag = show_list_type lty |> lowercase in
             let prefix = String.get flag 0 |> String.make 1 in
             ( lty,
               Arg.info [ prefix; flag ] ~doc:(Printf.sprintf "List %s." flag)
             ))
    in
    Arg.(last & vflag_all [ Installed ] parser)

  let parse_opts =
    Term.(
      const (fun query search_type -> { query; search_type })
      $ query_t $ search_type_t)

  let parse_common_opts =
    Term.(
      const (fun provider verbose -> { provider; verbose })
      $ provider_t $ verbose_t)

  let make_cmd ~action ~doc ~run =
    let cmd = show_action action |> lowercase in
    let info = Cmd.info cmd ~doc ~exits:[] in
    let term =
      Term.(const (fun co opts -> run co opts) $ parse_common_opts $ parse_opts)
    in
    Cmd.v info term

  let query_opt_t =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~doc:"Search for repo or org." ~docv:"QUERY")

  let parse =
    let version =
      Build_info.V1.(
        match version () with
        | Some v -> Version.to_string v
        | None -> "0.1")
    in
    let info =
      Cmd.info "pkgman" ~version ~exits:[]
        ~doc:"Download releases from various providers."
        ~envs:[ Cmd.Env.info "PROVIDER_APIKEY" ~doc:"API key if needed." ]
    in

    let commands =
      all_of_action
      |> List.map (fun action ->
             match action with
             | List as l ->
                 let info =
                   Cmd.info
                     (show_action l |> lowercase)
                     ~doc:
                       "List either installed or releases available in remote."
                     ~exits:[]
                 in
                 let term =
                   Term.(
                     const (fun opts ty q -> R.list opts ty q)
                     $ parse_common_opts $ list_t $ query_opt_t)
                 in
                 Cmd.v info term
             | act ->
                 let run, doc =
                   match[@warning "-8"] act with
                   | Search -> (R.search, "Search for a given repo.")
                   | Install ->
                       (R.install, "Install the given repo's latest release.")
                 in
                 make_cmd ~doc ~action ~run)
    in

    Cmd.group info commands

  let main () = exit (Cmd.eval parse)
end
