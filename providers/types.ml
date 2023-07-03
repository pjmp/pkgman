type query_type = [ `Repo | `Users ]

module type Provider = sig
  type t = { name : string }

  val search : query:string -> ty:query_type -> unit
  val install : query:string -> ty:query_type -> unit
end
