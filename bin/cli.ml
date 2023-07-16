open Pkgman_common.Interface

module CLI (R : Runnable) = struct
  open Cmdliner

  let query_t =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"Search for repo or org." ~docv:"QUERY")

  let provider_t =
    let opts =
      all_of_providers
      |> List.map (fun ty -> (String.lowercase_ascii (show_providers ty), ty))
      |> Arg.enum
    in
    Arg.(
      value & opt opts Github
      & info [ "p"; "provider" ] ~doc:"Specify a provider" ~docv:"PROVIDER")

  let search_type_t =
    let opts =
      all_of_search_type
      |> List.map (fun ty -> (String.lowercase_ascii (show_search_type ty), ty))
      |> Arg.enum
    in
    Arg.(
      value & opt opts `Repo
      & info [ "t"; "type" ] ~doc:"Search by repo or users." ~docv:"TYPE")

  let verbose_t =
    let doc = "Print whatever pkgman is doing." in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

  let parse_opts =
    Term.(
      const (fun provider query search_type verbose ->
          { provider; query; search_type; verbose })
      $ provider_t $ query_t $ search_type_t $ verbose_t)

  let make_cmd ~action ~cmd ~doc =
    let info = Cmd.info cmd ~doc ~exits:[] in
    let term = Term.(const (fun opts -> R.run action opts) $ parse_opts) in
    Cmd.v info Term.(const (fun _ -> ()) $ term)

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
    let default =
      Term.(ret (const (fun _ -> `Help (`Pager, None)) $ parse_opts))
    in

    Cmd.group info ~default
      [
        make_cmd ~cmd:"search" ~doc:"Search for a given repo." ~action:Search;
        make_cmd ~cmd:"install" ~doc:"Install the given repo's latest release."
          ~action:Install;
      ]

  let main () = exit (Cmd.eval parse)
end
