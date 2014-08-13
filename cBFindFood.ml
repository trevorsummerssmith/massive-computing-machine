(** A character that finds food.
    Senses the entire board.
    Walks to the closest food.
*)

open Core.Std

open CharacterBrain

let name = "findfood"

let sense board character =
  {board; character}

let decide umwelt =
  (* Slow... for every food on the board find the path to it. Sort the
     paths by their length. Take the shortest path. *)
  let food_posns = Board.filter_board umwelt.board ~f:(fun posn -> (match Board.get_tile umwelt.board posn with Board.Food _ -> true | _ -> false)) in
  (* If there's no food do nothing and bail now. The rest of the code assumes there is food. *)
  if (List.length food_posns) = 0 then
    DoNothing
  else
    let f = (fun g -> Astar.find_path umwelt.board (Character.posn umwelt.character) g) in
    let paths : (Posn.t list) list = List.map ~f:f food_posns in
    (* Which path is shortest? *)
    let paths' : (int * (Posn.t list)) list = List.map ~f:(fun ps -> (List.length ps, ps)) paths in
    let cmp (len1, _) (len2, _) = compare len1 len2 in
    let paths'' = List.sort ~cmp:cmp paths' in
    let (length, closest) = List.hd_exn paths'' in
    (* Now -- we have a direction we either need to walk, or something to eat *)
    match length with
      1 -> Eat
    | _ ->
       (* Let's go here. Easy now. (first element in the path is the start) *)
       CharacterBrain.Move (List.nth_exn closest 1)
