(** A random character brain *)

open CharacterBrain

let name = "random"

let sense board character =
  (* This senses the entire board for now *)
  {board = board; character = character}

let positions : Posn.t list =
  [(-1, -1); (0, -1); (1, -1);
   (-1, 0); (0, 0); (1, 0);
   (-1, 1); (0, 1); (1, 1)]
(** Possible relative moves, including not moving.
    A list of length 9 with the following entries:
   -1,-1  0,-1  1,-1
    -1,0  here 1,0
    -1,1  0,1  1,1
*)

let decide umwelt =
  let pick = Random.int 9 in
  let (x_delta, y_delta) = List.nth positions pick in
  let (x,y) = Character.posn umwelt.character in
  let posn' = Board.chomp_coords umwelt.board (x+x_delta, y+y_delta) in
  Move posn'
