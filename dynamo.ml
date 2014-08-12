open Core.Std

type t =
  {
    seed : int
  }

let create seed =
  Random.init seed;
  {seed}

let seed dynamo = dynamo.seed

let make_actor (module B : CharacterBrain.S) character =
  (module struct
     module Brain = B
     let character = character
   end : Actor.S)

let make_randos posn num : (module Actor.S) list =
  (** Make a list of randomly positioned characters *)
  let rec rando num acc =
    if num > 0 then
      let name = Util.random_name () in
      let posn' = Util.random_posn posn in
      let character = Character.create name posn' in
      let actor = make_actor (module CBRandom) character in
      rando (num-1) (actor::acc)
    else acc
  in
  let character = Character.create "foo" (5,5) in
  let actor = make_actor (module CBStagnant) character in
  actor::(rando num [])

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
    List.iter ~f:add_actor (make_randos (width, height) num_characters);
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
