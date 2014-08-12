(** Implementation for both Character and MutableCharacter *)
type t = {name : string; posn : Posn.t ref}
let create name posn =
  {name = name; posn = ref posn}
let name c = c.name
let posn c = !(c.posn)
let set_posn c posn =
  c.posn := posn
