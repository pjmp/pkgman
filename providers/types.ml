module type Provider = sig
  type t = { name : string }

  val t : t
  val search : query:string -> ty:bool -> unit
  val install : query:string -> ty:bool -> unit
end
