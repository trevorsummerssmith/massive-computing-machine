(** Implementation for both Board and MutableBoard *)

open Core.Std

type tile = (* TODO should tile be defined elsewhere? *)
    EmptyTile
  | Hole (* Cannot go through a hole *)
  | Hill (* Hills are harder to go over *)
  | Character of Character.t
  | Food of int

type t =
    {
      board : tile array array;
      dimensions : Posn.t
    }

let create (width, height) =
  let arr = Array.make_matrix ~dimx:width ~dimy:height EmptyTile in
  {board = arr; dimensions = (width, height)}

let dimensions board = board.dimensions

let chomp_coords board (x, y) =
  let chomp i imax =
    (* TODO this only works for moves of 1. MUST CHANGE *)
    if (i < 0) then
      imax
    else if (i > imax) then
      0
    else
      i
  in
  let (width, height) = board.dimensions in
  (chomp x (width-1), chomp y (height-1))

let neighbors board (x, y) =
  (** Returns the 8 neighbors of a (x,y) in valid board coordinates. *)
  let modified_coords = List.map ~f:(fun (dx, dy) -> (x+dx, y+dy)) Posn.neighbor_deltas in
  List.map ~f:(chomp_coords board) modified_coords

let tile_movement_cost = function
  (** How much to move to an adjacent square?
      This only depends upon terrain type. *)
    EmptyTile
  | Food _ -> 1
  | Hole -> 99999
  | Hill -> 3
  | Character _ -> 99 (* Cannot walk through a character? But what if they move? *)

let all_indices (board : t) : (int * int) list =
  let (width, height) = board.dimensions in
  let rec loop x y acc =
    if (y = height) then
      if (x = width-1) then
	acc
      else
	loop (x+1) 0 acc
    else
      loop x (y+1) ((x,y)::acc)
  in
  List.rev (loop 0 0 [])

let filter_board (board : t) ~f : Posn.t list =
  let indices = all_indices board in
  List.filter ~f indices

let assert_bounds board (x, y) : unit =
  let (w, h) = board.dimensions in
  assert (x < w);
  assert (x >= 0);
  assert (y < h);
  assert (y >= 0)

let get_tile board (x, y) =
  assert_bounds board (x, y);
  board.board.(x).(y)

(* mutable functions *)

let set_tile board tile (x, y) =
  assert_bounds board (x, y);
  board.board.(x).(y) <- tile

(* serialize to json *)

let serialize_tile tile = match tile with
    EmptyTile -> Ezjsonm.string "Empty"
  | Hole -> Ezjsonm.string "Hole"
  | Hill -> Ezjsonm.string "Hill"
  | Character c -> Ezjsonm.string c.name
  | Food _ -> Ezjsonm.string "Food" (* TODO XXX *)

let deserialize_tile json =
  let x = Ezjsonm.(get_int (find json ["x"])) in
  let y = Ezjsonm.(get_int (find json ["y"])) in
  let s = Ezjsonm.(get_string (find json ["t"])) in
  let tile =
    match s with
      "Empty" -> EmptyTile
    | "Hole" -> Hole
    | "Hill" -> Hill
    | "Food" -> Food 1 (* TODO XXX *)
    | _ -> let c = Character.create s (x,y) in Character c (* TODO XXX *)
  in
  (x, y, tile)

let serialize (b : t) =
  let indices = filter_board b ~f:(fun (x,y) -> b.board.(x).(y) <> EmptyTile)
  in
  let to_json (x,y) =
    Ezjsonm.dict [("x", Ezjsonm.int x);
		  ("y", Ezjsonm.int y);
		  ("t", serialize_tile b.board.(x).(y))]
  in
  let board_json = Ezjsonm.list to_json indices in
  let (w, h) = b.dimensions in
  Ezjsonm.dict [("width", Ezjsonm.int w);
		("height", Ezjsonm.int h);
		("cells", board_json)]

let deserialize json =
  let width = Ezjsonm.(get_int (find json ["width"])) in
  let height = Ezjsonm.(get_int (find json ["height"])) in
  let cells = Ezjsonm.(get_list deserialize_tile (find json ["cells"])) in
  let board = create (width, height) in
  (* List elems are json list: x:int, y:int, string: tile *)
  let process cell =
    let (x, y, tile) = cell in
    set_tile board tile (x, y)
  in
  List.iter ~f:process cells;
  board
