type providers = Github | Gitlab | Bitbucket | Codeberg
[@@deriving show { with_path = false }, enumerate]

type action = Install | Search [@@deriving show { with_path = false }]

module type Runnable = sig
  val run : action -> string -> providers -> unit
end

module CLI (R : Runnable) = struct
  open Cmdliner

  let prefix_of_providers = function
    | Github -> "gh"
    | Gitlab -> "gl"
    | Bitbucket -> "bb"
    | Codeberg -> "cb"

  let query_t =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"foo bar baz" ~docv:"QUERY")

  let provider_t =
    let opts =
      all_of_providers
      |> List.map (fun provider ->
             ( provider,
               Arg.info
                 [
                   prefix_of_providers provider;
                   String.lowercase_ascii (show_providers provider);
                 ]
                 ~doc:("Use " ^ show_providers provider) ))
    in
    Arg.(last & vflag_all [ Github ] opts)

  let make_cmd ~action ~cmd ~doc =
    let info = Cmd.info cmd ~doc ~exits:[] in
    let term = Term.(const (R.run action) $ query_t $ provider_t) in
    Cmd.v info Term.(const Fun.id $ term)

  let main_cmd =
    let version =
      Build_info.V1.(
        match version () with Some v -> Version.to_string v | None -> "0.1")
    in
    let info =
      Cmd.info "pkgman" ~version ~exits:[]
        ~doc:"download releases from various hosting"
        ~envs:[ Cmd.Env.info "PROVIDER_APIKEY" ~doc:"API key if needed" ]
    in
    let default =
      Term.(
        ret
          (const (fun _ -> `Help (`Plain, None))
          $ Term.(const Fun.id $ provider_t)))
    in
    Cmd.group info ~default
      [
        make_cmd ~cmd:"search" ~doc:"Search for given repo" ~action:Search;
        make_cmd ~cmd:"install" ~doc:"Install the given given repo's release"
          ~action:Install;
      ]

  let main () = exit (Cmd.eval main_cmd)
end
