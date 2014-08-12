(** MutableCharacter.

    This should only be used by the Dynamo.
*)
module type S = sig
    include Character.S
    val set_posn : t -> Posn.t -> unit
end

module T = (CharacterImpl : S with type t = CharacterImpl.t)
include T
