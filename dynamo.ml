open Core.Std

type t =
  {
    seed : int
  }

let create seed =
  Random.init seed;
  {seed}

let seed dynamo = dynamo.seed

let serialize dynamo =
  Ezjsonm.dict [("seed", Ezjsonm.int dynamo.seed)]

let deserialize json =
  let seed = Ezjsonm.(get_int (find json ["seed"])) in
  {seed}

let save_game dynamo world file_name =
  (* Serialize the dynamo and the world to a file.
   Toplevel json has two keys: 'dyanmo' and 'world'.
   *)
  let dynamo_json = serialize dynamo in
  let world_json = World.serialize world in
  let json = Ezjsonm.dict [("dynamo", dynamo_json);
			   ("world", world_json)]
  in
  (* Write it out *)
  let channel = open_out file_name in
  Ezjsonm.to_channel ~minify:false channel json;
  Out_channel.close channel

let load_game file_name : (t * World.t) =
  let channel = open_in file_name in
  let json = Ezjsonm.from_channel channel in
  In_channel.close channel;
  let dynamo = deserialize (Ezjsonm.find json ["dynamo"]) in
  let world = World.deserialize (Ezjsonm.find json ["world"]) in
  (dynamo, world)

let make_randos posn num : (module Actor.S) list =
  (** Make a list of randomly positioned characters *)
  let rec rando num acc =
    if num > 0 then
      let name = Util.random_name () in
      let posn' = Util.random_posn posn in
      let character = Character.create name posn' in
      let actor = Actor.make (module CBRandom) character in
      rando (num-1) (actor::acc)
    else acc
  in
  let character = Character.create "hunter" (5,5) in
  let actor = Actor.make (module CBFindFood) character in
  actor::(rando num [])

let make_random_terrain (w,h) ~num_hills ~num_holes ~num_food : (MBoard.tile * Posn.t) list =
  (* This will overwrite tiles.
     For now we just take a number of tiles for these guys.
     Later on we can deal with this.
   *)
  let num_tiles = w * h in
  let total_tiles = num_hills + num_holes + num_food in
  assert (total_tiles < num_tiles);
  (* Now actually generate the tiles *)
  let rec rando tile num acc =
    if num > 0 then
      let posn = Util.random_posn (w,h) in
      rando tile (num-1) ((tile, posn)::acc)
    else acc
  in
  let acc = rando MBoard.Hill num_hills [] in
  let acc' = rando MBoard.Hole num_holes acc in
  let acc'' = rando (MBoard.Food 1) num_food acc' in
  acc''

let make_world _ (width, height) num_characters =
  let mBoard = MBoard.create (width, height) in
  let (world : World.t) = {board = mBoard; actors = ref []} in
  (* Make some characters *)
  assert (num_characters <= width * height);
  let add_actor (module A : Actor.S) =
    let character = A.character in
    MBoard.set_tile mBoard (Character character) (Character.posn character);
    world.actors := (module A)::(!(world.actors))
  in
  begin
    let terrain = make_random_terrain (MBoard.dimensions mBoard)
				      ~num_hills:10 ~num_holes:10
				      ~num_food:10
    in
    List.iter ~f:add_actor (make_randos (width, height) num_characters);
    List.iter ~f:(fun (tile, posn) -> MBoard.set_tile mBoard tile posn) terrain;
    world
  end

let update_state (world : World.t) mCharacter action =
  (** Mutates the state of the world and the character,
   based upon a character action *)
  match action with
    CharacterBrain.Move posn' -> begin
		let posn = MCharacter.posn mCharacter in
		MCharacter.set_posn mCharacter posn';
		MBoard.set_tile world.board (Character mCharacter) posn';
		(* Remove the previous space... TODO will replace old environment.
		   MUST CHANGE
		 *)
		if posn <> posn' then
		  MBoard.set_tile world.board EmptyTile posn
	      end
  | Eat -> (* Remove the food from the board by putting an empty tile *)
     MBoard.set_tile world.board EmptyTile (MCharacter.posn mCharacter)
  | DoNothing -> ()

let tick _ (world : World.t) =
  let take_turn (module A : Actor.S) : CharacterBrain.action =
    let umwelt = A.Brain.sense world.board (A.character) in
    let action = A.Brain.decide umwelt in
    action
 in
 let actors = !(world.actors) in
 let actions = List.map actors ~f:take_turn in
 List.iter2_exn actors actions ~f:(fun (module A) a -> update_state world (A.character) a);
 world
