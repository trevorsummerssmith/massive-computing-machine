(** Immutable Character *)
module type CharacterSig = sig
    type t
    val create : string -> Posn.t -> t
    val name : t -> string
    val posn : t -> Posn.t
end

module T = (CharacterImpl.T : CharacterSig with type t = CharacterImpl.T.t)
include T
