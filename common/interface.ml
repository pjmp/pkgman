type providers = Github | Gitlab | Bitbucket | Codeberg
[@@deriving show { with_path = false }, enumerate]

type action = Install | Search [@@deriving show { with_path = false }]

type search_type = [ `Repo | `Users ]
[@@deriving show { with_path = false }, enumerate]

let show_search_type = function
  | `Repo -> "repo"
  | `Users -> "users"

type opts = {
  provider : providers;
  query : string;
  search_type : search_type;
  verbose : bool;
}
[@@deriving show { with_path = false }]

module type Runnable = sig
  val run : action -> opts -> unit
end

module type Provider = sig
  val search : opts -> unit
  val install : opts -> unit
  (* val list_available : action -> opts -> unit *)
end
