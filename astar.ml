(** Find a path from start to goal on the world. Return the path.

    Astar is implemented with two datastructures:
      1) a hashtable that uses a datatype to represent if the element is
         CLOSED or OPEN. This is 'visited'.
      2) The priority queue that maps the priority to the posn.
         This is 'pqueue'.

    Psuedo code for astar:
      OPEN = priority queue containing START
      CLOSED = empty set
      while lowest rank in OPEN is not the GOAL:
        current = remove lowest rank item from OPEN
        add current to CLOSED
        for neighbors of current:
          cost = g(current) + movementcost(current, neighbor)
          if neighbor in OPEN and cost less than g(neighbor):
            remove neighbor from OPEN, because new path is better
          if neighbor in CLOSED and cost less than g(neighbor): **
            remove neighbor from CLOSED
          if neighbor not in OPEN and neighbor not in CLOSED:
            set g(neighbor) to cost
            add neighbor to OPEN
            set priority queue rank to g(neighbor) + h(neighbor)
            set neighbor's parent to current

      reconstruct reverse path from goal to start
      by following parent pointers

    Note -- XXX this assumes that 1) this will always be called with a valid
    path and 2) it assumes that the graph structure that exists will always
    have neighbors. This is a reasonable assumption given that, at the moment
    it uses the board. However if we change that we need to change this.
    Also, we might want to return an error type 'no_path' for the no path case.
*)

open Core.Std
open Core.Std.Timing_wheel (* for a priority queue *)

type astar_status = Open | Closed
(** If the location is in the open or closed set *)

type location = {
  posn: Posn.t;
  cost: int;
  parent: location option;
  status: astar_status }
(** A posn with metadata used by astar *)

let d = 1;; (* TODO what should this be? See link in heuristic *)

let heuristic (x1, y1) (x2, y2) =
(** This heuristic was recommended here for square grids with diagonal movement
   http://theory.stanford.edu/~amitp/GameProgramming/Heuristics.html
*)
  let dx = abs (x1 - x2)
  and dy = abs (y1 - y2) in
  d * max dx dy

let movement_cost (world : Game.world) (x,y) =
  (* Current implementation the movement cost is the cost of the terrain. *)
  let tile = world.board.(x).(y) in
  Game.movement_cost tile

let pqueue_min_exn pqueue =
  (** The priority queue interface is annoying. This wraps so that the minimum
   element is returned and removed from the priority queue. This returns the value
   that was initially added to the priority queue (not the internal priority queue
   element type) *)
  let option = Priority_queue.min_elt pqueue in
  match option with
    Some elem -> (let value = Priority_queue.Elt.value pqueue elem in
		  Priority_queue.remove pqueue elem;
		  value)
  | None -> failwith "This should never happen. (Probably called with no valid path) :)"

let rec loc_chain_to_list location acc =
  (* Traverse a location chain and return the list. The first element
   of the returned list is the start of the path. The last is the end. *)
  match location.parent with
    Some parent -> loc_chain_to_list parent (location.posn::acc)
  | None -> location.posn::acc

let find_path world start goal =
  let visited : location Posn.Table.t = Posn.Table.create ()
  and pqueue : Posn.t Priority_queue.t = Priority_queue.create () in
  let add_to_open posn cost parent =
    (* Add posn to the open set with cost set to cost and parent set to parent. *)
    let site = {posn = posn; cost = cost; parent = Some parent; status=Open} in
    let h = heuristic posn parent.posn in
    Hashtbl.replace visited ~key:posn ~data:site;
    let _ = Priority_queue.add pqueue ~key:(cost+h) posn in
    ()
  in
  let process_neighbor current neighbor_posn =
    let option = (Hashtbl.find visited neighbor_posn) in
    let cost = current.cost + (movement_cost world current.posn) in
    match option with
      Some neighbor_location ->
      begin
	(* Open: If the cost is less then this path is better so use it.*)
	(* Closed: Heuristic messed up. Remove it and continue *)
	if cost < neighbor_location.cost then
	  (Hashtbl.remove visited neighbor_location.posn;
	   add_to_open neighbor_location.posn cost current)
      end
    | None -> add_to_open neighbor_posn cost current
  in
  let rec loop (current : location) =
    if current.posn = goal then
      current
    else
      begin
	(* Replace it. It should be closed now. *)
	let closed = {posn=current.posn;
		      cost=current.cost;
		      parent=current.parent;
		      status=Closed} in
	Hashtbl.replace visited ~key:current.posn ~data:closed;
	let neighbors = Game.neighbors world current.posn in
	List.iter ~f:(fun n -> process_neighbor current n) neighbors;
	(* Done with the neighbors. Loop! *)
	let next = pqueue_min_exn pqueue in
	loop (Hashtbl.find_exn visited next)
      end
  in
  let ret = loop {posn=start; cost=1; parent=None; status=Open} in
  loc_chain_to_list ret []
