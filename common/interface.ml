type providers = Github | Gitlab | Bitbucket | Codeberg
[@@deriving show { with_path = false }, enumerate]

type action = Install | Search | List
[@@deriving show { with_path = false }, enumerate]

type search_type = Repo | Users
[@@deriving show { with_path = false }, enumerate]

type list_type = Available | Installed
[@@deriving show { with_path = false }, enumerate]

type opts = { query : string; search_type : search_type }
[@@deriving show { with_path = false }]

type common_opts = { provider : providers; verbose : bool }
[@@deriving show { with_path = false }]

module type Provider = sig
  val search : common_opts -> opts -> unit
  val install : common_opts -> opts -> unit
  val list : common_opts -> list_type -> unit
end
