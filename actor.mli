(** Character + Brain is an Actor *)
module type S = sig
    module Brain : CharacterBrain.S
    val character : Character.t
end
