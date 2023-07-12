type providers = Github | Gitlab | Bitbucket | Codeberg
[@@deriving show { with_path = false }, enumerate]

type action = Install | Search [@@deriving show { with_path = false }]
type query_type = [ `Repo | `Users ] [@@deriving enumerate]

let show_query_type = function
  | `Repo -> "repo"
  | `Users -> "users"

module type Runnable = sig
  val run : action -> string -> providers -> query_type -> unit
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

  let query_type_t =
    let opts =
      all_of_query_type
      |> List.map (fun ty -> (String.lowercase_ascii (show_query_type ty), ty))
      |> Arg.enum
    in
    Arg.(
      value & opt opts `Repo
      & info [ "t"; "type" ] ~doc:"Search by repo or users." ~docv:"TYPE")

  let make_cmd ~action ~cmd ~doc =
    let info = Cmd.info cmd ~doc ~exits:[] in
    let term =
      Term.(const (R.run action) $ query_t $ provider_t $ query_type_t)
    in
    Cmd.v info Term.(const (fun _ -> ()) $ term)

  let main_cmd =
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
      Term.(
        ret
          (const (fun _ -> `Help (`Plain, None))
          $ Term.(const (fun _ _ -> ()) $ provider_t $ query_type_t)))
    in
    Cmd.group info ~default
      [
        make_cmd ~cmd:"search" ~doc:"Search for a given repo." ~action:Search;
        make_cmd ~cmd:"install" ~doc:"Install the given repo's latest release."
          ~action:Install;
      ]

  let main () = exit (Cmd.eval main_cmd)
end
