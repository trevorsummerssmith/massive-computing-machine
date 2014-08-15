open OUnit
open Core.Std

let test_save_and_load_game _ =
  let board = MBoard.create (5, 5) in
  let character1 = Character.create "foo" (3,3) in
  let character2 = Character.create "hunter" (2,2) in
  MBoard.set_tile board (MBoard.Character character1) (3,3);
  MBoard.set_tile board (MBoard.Character character2) (2,2);
  let actor1 = (module struct
		 module Brain = CBRandom
		 let character = character1
	       end : Actor.S) in
  let actor2 = (module struct
		 module Brain = CBFindFood
		 let character = character2
	       end : Actor.S) in
  let world : World.t = {board = board; actors = ref [actor1; actor2]} in
  let dynamo = Dynamo.create 99 in
  let temp_filename = Filename.temp_file "ok" ".json" in
  Dynamo.save_game dynamo world temp_filename;
  (* Now load it back *)
  let (dynamo', world') = Dynamo.load_game temp_filename in
  assert_equal dynamo dynamo';
  assert_equal world.board world'.board;
  (* TODO We cannot compare the actors directly cuz they're modules *)
  assert_equal (List.length !(world.actors)) (List.length !(world'.actors));
  let actor_eq (module A1 : Actor.S) (module A2 : Actor.S) =
    assert_equal A1.character A2.character
  in
  List.iter2_exn ~f:actor_eq !(world.actors) !(world'.actors)

let suite =
"suite">:::
 ["Save & Load Game">:: test_save_and_load_game;
 ]

let () =
  let _ = run_test_tt_main suite in
  ()
