(* This set of tests is extremely brittle. If the heuristic used in astar
   changes these tests will need to change. Right now all tests have hardcoded
   solutions. This could be changed so that instead of a hardcoded path there is
   a set of possible paths that are all the same cost.

   Below in the comments I little drawings that show the maps that are being
   tested. Key:
     . - empty tile space
     o - hole
     ^ - hill
     + - character (start)
     g - goal
*)

open OUnit
open Game
open Astar

(* pretty print TODO this should live somewhere else. *)
let _tile = function
    EmptyTile -> "."
  | Hole -> "o"
  | Hill -> "^"
  | Character _ -> "+"

let _print_header length =
  Core.Std.eprintf "+";
  for i = 0 to (length-1) do
    let _ = i in
    Core.Std.eprintf "-"
  done;
  Core.Std.eprintf "+\n"

let print_board world (f_overlay : Posn.t -> tile -> string -> string) =
  _print_header world.width;
  for y = 0 to (world.height-1) do
    (* New row start it *)
    Core.Std.eprintf "|";
    for x = 0 to (world.width-1) do
      let t = world.board.(x).(y) in
      let s = _tile t in
      Core.Std.eprintf "%s" (f_overlay (x,y) t s)
    done;
    Core.Std.eprintf "|\n"
  done;
  _print_header world.width

let rec print_path path =
  match path with
    (x,y)::rst -> Core.Std.eprintf "(%d, %d)" x y; print_path rst
  | [] -> Core.Std.eprintf "\n"

let overlay (path : Posn.t list) (g : Posn.t) t (s : string) : string =
  (* Print function to print the path on the board.
   Prints a '*' if there's a path, but only if there is no character there. *)
  match t with
    Character _ -> s
  | _ -> begin if (Core.Std.List.mem path g) then
		 "*"
	       else
		 s
	 end
(* end pretty print *)

let _test ~holes ~hills ~start ~goal ~solution =
  let world = Game.create_world 10 10 in
  List.iter (fun posn -> update_tile world Hole posn) holes;
  List.iter (fun posn -> update_tile world Hill posn) hills;
  let path = find_path world start goal in
  assert_equal path solution;
  print_board world (overlay path);
  print_path path

let test_no_obstacles _ =
  _test ~holes:[] ~hills:[] ~start:(0,0) ~goal:(5,5)
	~solution:[(0,0); (1,1); (2,2); (3,3); (4,4); (5,5)]

let test_small_wall _ =
  (* A small wall. We should walk around it.
     +----------+
     |...o......|
     |.+.o.g....|
     |..*o.*....|
     |..*o*.....|
     |...*......|
     +----------+
   *)
  _test ~holes:[(3,0); (3,1); (3,2); (3,3)] ~hills:[] ~start: (1,1) ~goal:(5,1)
	~solution: [(1, 1); (2, 2); (2, 3); (3, 4); (4, 3); (5, 2); (5, 1)]

let test_dipper_shaped_wall _ =
  (* Multiple 'shortest paths' if there are no obstacles but there are obstacles.
     +----------+
     |.ooo.*....|
     |.+.o*o*...|
     |..*ogo.*..|
     |..*ooo*...|
     |...*.*....|
     |....*.....|
     +----------+
   *)
  let holes = [(1,0); (2,0); (3,0); (3,1); (3,2); (3,3); (4,3);
	       (5,3); (5,2); (5,1)] in
  _test ~holes:holes ~hills:[] ~start:(1,1) ~goal:(4,2)
	~solution: [(1, 1); (2, 2); (2, 3); (3, 4); (4, 5);
		    (5, 4); (6, 3); (7, 2); (6, 1); (5, 0); (4, 1); (4, 2)]

let test_over_the_hill _ =
  (* Make sure hill weights are correctly taken into account. This should go
     over the hill because the holes cannot be gone through, and going around
     the hill is too expensive.

     +----------+
     |.ooo......|
     |.+.^g.....|
     |...^......|
     |...^......|
     |...^......|
     +----------+
 *)
  let holes = [(1,0); (2,0); (3,0)]
  and hills = [(3,1); (3,2); (3,3); (3,4)] in
  _test ~holes:holes ~hills:hills ~start:(1,1) ~goal:(4,1)
	~solution: [(1, 1); (2, 2); (3, 1); (4, 1)]

let test_around_the_hill _ =
  (* Going around the hill is least expensive.
     This is the same map as over_the_hill, except the hill is two tiles shorter.

     +----------+
     |.ooo......|
     |.+.^g.....|
     |..*^*.....|
     |...*......|
     +----------+
   *)
  let holes = [(1,0); (2,0); (3,0)]
  and hills = [(3,1); (3,2)] in
  _test ~holes:holes ~hills:hills ~start:(1,1) ~goal:(4,1)
	~solution: [(1, 1); (2, 2); (3, 3); (4, 2); (4, 1)]

let suite =
"suite">:::
 ["No obstacles">:: test_no_obstacles;
  "Small wall">::test_small_wall;
  "Dipper shaped wall">:: test_dipper_shaped_wall;
  "Over the hill!">:: test_over_the_hill;
  "Around the hill!">:: test_around_the_hill]

let () =
  let _ = run_test_tt_main suite in (* this produces a result... *)
  ()
