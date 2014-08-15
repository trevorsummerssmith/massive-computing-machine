open OUnit

let test_deserialize _ =
  (* One simple test that hits all of the tile types *)
  let str = "{\"width\": 10, \"height\": 12,
	     \"cells\":[
	       {\"x\": 1, \"y\":2, \"t\": \"foo\"},
	       {\"x\": 5, \"y\":4, \"t\": \"Hill\"},
	       {\"x\": 6, \"y\":7, \"t\": \"Hole\"},
	       {\"x\": 8, \"y\":7, \"t\": \"Food\"}
	     ]}" in
  let json = Ezjsonm.from_string str in
  let board = Board.deserialize json in
  let (w, h) = Board.dimensions board in
  let character = Character.create "foo" (1,2) in
  assert_equal w 10;
  assert_equal h 12;
  assert_equal (Board.Character character) (Board.get_tile board (1,2));
  assert_equal Board.Hill (Board.get_tile board (5,4));
  assert_equal Board.Hole (Board.get_tile board (6,7));
  assert_equal (Board.Food 1) (Board.get_tile board (8,7));
  (* Assert a couple of other tiles are empty. *)
  assert_equal Board.EmptyTile (Board.get_tile board (0,0));
  assert_equal Board.EmptyTile (Board.get_tile board (2,2))

let suite =
"suite">:::
 ["Deserialize">:: test_deserialize;
 ]

let () =
  let _ = run_test_tt_main suite in (* this produces a result... *)
  ()
