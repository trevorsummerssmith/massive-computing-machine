(** Character + Brain is an Actor *)
module type S = sig
    module Brain : CharacterBrain.S
    val character : Character.t
end

(* TEST *)
let make (module B : CharacterBrain.S) character =
  (module struct
     module Brain = B
     let character = character
   end : S)
