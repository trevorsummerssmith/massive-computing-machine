open OUnit

let test_serialize_and_deserialize _ =
  let character = Character.create "foo" (1,2) in
  let json = Character.serialize character in
  let character' = Character.deserialize json in
  assert_equal character character'

let suite =
"suite">:::
 ["Deserialize & Serialize">:: test_serialize_and_deserialize;
 ]

let () =
  let _ = run_test_tt_main suite in
  ()
