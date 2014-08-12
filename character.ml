(** Immutable Character *)

module type S = sig
    type t
    val create : string -> Posn.t -> t
    val name : t -> string
    val posn : t -> Posn.t
end

module T = (CharacterImpl : S with type t = CharacterImpl.t)
include T
