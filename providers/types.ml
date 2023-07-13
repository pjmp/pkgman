type query_type = [ `Repo | `Users ]

module type Provider = sig
  val search : query:string -> ty:query_type -> unit
  val install : query:string -> unit
end
