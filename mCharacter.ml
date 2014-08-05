(** MutableCharacter.

    This should only be used in toplevel bookeeping code.
*)
module type MutableCharacterSig = sig
    include Character.CharacterSig
    val set_posn : t -> Posn.t -> unit
end

module T = (CharacterImpl.T : MutableCharacterSig with type t = CharacterImpl.T.t)
include T
