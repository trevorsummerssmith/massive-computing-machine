type posn = int * int

type character =
    {
      name : string;
      posn : posn ref;
    }

type tile =
    EmptyTile
  | Character of character

type world =
  { 
    board : tile array array;
    characters : (character list) ref; (* TODO remove mutability *)
    width : int;
    height : int
  }

let nowhere : posn = (max_int, max_int)

let init_random seed =
  Random.init seed

let create_world width height : world =
  let arr = Array.make_matrix width height EmptyTile in
  {board = arr; characters = ref []; width = width; height = height}

let all_indices world : (int * int) list =
  let rec loop x y acc =
    if (y = world.height) then
      if (x = world.width-1) then
      acc
      else
	loop (x+1) 0 acc
    else
      loop x (y+1) ((x,y)::acc)
  in
  List.rev (loop 0 0 [])

let filter_board f world : (int * int ) list =
  let indices = all_indices world in
  List.filter f indices

let iter_cells f world =
  (* junk *)
  for i = 0 to world.width-1 do
    for j = 0 to world.height-1 do
      f (i, j) world.board.(i).(j)
    done
  done

(* serialize. not sure where this goes *)

let serialize_tile tile = match tile with
    EmptyTile -> Ezjsonm.string "Empty"
  | Character c -> Ezjsonm.string c.name

let serialize_world world =
  let indices = filter_board (fun (x,y) -> world.board.(x).(y) <> EmptyTile) world
  in
  let to_json (x,y) =
    Ezjsonm.list (fun i->i) [Ezjsonm.int x; Ezjsonm.int y; serialize_tile world.board.(x).(y)]
  in
  Ezjsonm.list (fun i -> i) (List.map to_json indices)

(* serialize end *)

let assert_bounds world (x, y) : unit =
  assert (x < world.width);
  assert (x >= 0);
  assert (y < world.height);
  assert (y >= 0)

let update_tile world tile (x,y) : unit =
  assert_bounds world (x,y);
  world.board.(x).(y) <- tile

let tile_filled world (x,y) : bool =
  assert_bounds world (x,y);
  world.board.(x).(y) <> EmptyTile

let random_letter () : string =
  Char.escaped (char_of_int (int_of_char 'a' + Random.int 26))

let random_name () : string =
  let len = (Random.int 20) + 4 in
  String.concat "" (Array.to_list (Array.init len (fun _ -> random_letter ())))    

let make_character () : character =
  let name = random_name () in
  { name = name; posn = ref nowhere }

let random_posn world =
  let w = Random.int world.width in
  let h = Random.int world.height in
  (w, h)

let make_randos world num : (posn * character) list =
  let rec rando world num acc =
    if num > 0 then
      let elem = (random_posn world, make_character ()) in
      rando world (num-1) (elem::acc)
    else acc
  in
  rando world num []

let positions : posn list =
  [(-1, -1); (0, -1); (1, -1);
   (-1, 0); (0, 0); (1, 0);
   (-1, 1); (0, 1); (1, 1)]
(** Possible relative moves, including not moving.
    A list of length 9 with the following entries:
   -1,-1  0,-1  1,-1
    -1,0  here 1,0
    -1,1  0,1  1,1
*)

let chomp_coords world (x, y) =
  (** Takes posn and returns a posn inside the world assuming the world wraps. *)
  let chomp i imax =
    if i < 0 then imax else if i > imax then 0 else i
  in
  (chomp x (world.width-1), chomp y (world.height-1))

let make_move world character =
  (** Randomly move. Bad function: mutates state of character AND world. *)
  let pick = Random.int 9 in
  let (x_delta, y_delta) = List.nth positions pick in
  let (x,y) = !(character.posn) in
  let posn' = chomp_coords world (x+x_delta, y+y_delta) in
  let (x', y') = posn' in
  (*Core.Std.eprintf "OK: (%d, %d) + (%d, %d) => (%d, %d)\n%!" x y x_delta y_delta x' y'; XXX *)
  (* bad parts *)
  character.posn := posn';
  update_tile world (Character character) posn';
  (* Don't update with an empty tile if they didn't move! *)
  if ((x_delta, y_delta) <> (0,0)) then (* TODO can just bail earlier if delta is 0 *)
    update_tile world EmptyTile (x,y)

let advance_world world =
  (** Advance time one tick *)
  let f character = make_move world character in
  List.iter f !(world.characters);
  world

let make_world width height num_characters =
  (** Makes a world *)
  assert (num_characters <= width * height);
  let world = create_world width height in
  let add_character (posn, character) = 
    character.posn := posn;
    update_tile world (Character character) posn;
    world.characters := character::(!(world.characters))
  in
  begin
    List.iter add_character (make_randos world num_characters);
    world
  end
