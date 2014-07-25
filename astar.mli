val find_path : Game.world -> Posn.t -> Posn.t -> Posn.t list
(** Find a path from start to goal in world. Returns this as a list where the
    first element of the list is the start of the path (ie equal to start) and the
    last element of the list is equal to goal.
 *)
