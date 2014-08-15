open OUnit

let test_serialize_and_deserialize _ =
  let board = MBoard.create (5, 5) in
  let character = Character.create "foo" (3,3) in
  MBoard.set_tile board (MBoard.Character character) (3,3);
  let actor = (module struct
		 module Brain = CBRandom
		 let character = character
	       end : Actor.S) in
  let world : World.t = {board = board; actors = ref [actor]} in
  let json = World.serialize world in
  Core.Std.eprintf "XXX\n";
  Core.Std.eprintf "%s\n" (Ezjsonm.to_string json);
  let world' = World.deserialize json in
  assert_equal world world'

let suite =
"suite">:::
 ["Deserialize & Serialize">:: test_serialize_and_deserialize;
 ]

let () =
  let _ = run_test_tt_main suite in
  ()
