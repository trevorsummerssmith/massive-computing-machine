open Core.Std

type t =
  {
    board : Board.t;
    actors : ((module Actor.S) list) ref;
  }

let serialize_actor (module A : Actor.S) =
  let brain = A.Brain.name in
  let character_json = Character.serialize A.character in
  Ezjsonm.(dict [("brain", string brain);
		 ("character", character_json)])

let deserialize_actor json =
  let brain_name = Ezjsonm.(get_string (find json ["brain"])) in
  let module B =
    (val (match brain_name with (* TODO XXX bad *)
	    "random" -> (module CBRandom)
	  | "findfood" -> (module CBFindFood)
	  | "stagnant" -> (module CBStagnant)
	  | _ -> failwith "unknown brain type") : CharacterBrain.S)
  in
  let character = Character.deserialize (Ezjsonm.find json ["character"]) in
  (module struct
     module Brain = B
     let character = character
   end : Actor.S)

let serialize (world : t) =
  let board_json = Board.serialize world.board in
  let actors = !(world.actors) in
  let actors_json = Ezjsonm.list serialize_actor actors in
  Ezjsonm.(dict [("actors", actors_json);
		 ("board", board_json)])

let deserialize json =
  let actors = Ezjsonm.(get_list deserialize_actor (find json ["actors"])) in
  let board = Board.deserialize (Ezjsonm.find json ["board"]) in
  {board = board; actors = ref actors}
