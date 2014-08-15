(** Wrapper for all of the things in the game. *)
type t =
  {
    board : Board.t;
    actors : ((module Actor.S) list) ref;
  }

val serialize: t -> Ezjsonm.t

val deserialize: Ezjsonm.t -> t

