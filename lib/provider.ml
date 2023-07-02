module type Provider = sig
  type t

  val search : string -> unit
end
