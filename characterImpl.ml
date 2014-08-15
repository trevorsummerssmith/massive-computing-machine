(** Implementation for both Character and MutableCharacter *)
type t = {name : string; posn : Posn.t ref}
let create name posn =
  {name = name; posn = ref posn}
let name c = c.name
let posn c = !(c.posn)
let set_posn c posn =
  c.posn := posn

let serialize c =
  let (x,y) = !(c.posn) in
  Ezjsonm.(dict [("name", string c.name);
		 ("x", int x);
		 ("y", int y)
  ])

let deserialize json =
  let name = Ezjsonm.(get_string (find json ["name"])) in
  let x = Ezjsonm.(get_int (find json ["x"])) in
  let y = Ezjsonm.(get_int (find json ["y"])) in
  create name (x,y)
