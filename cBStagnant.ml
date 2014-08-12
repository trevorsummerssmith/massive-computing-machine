(** A brain that does nothing! *)

open CharacterBrain

let name = "stagnant"

let sense board character =
  {board = board; character = character}

let decide _ = DoNothing
