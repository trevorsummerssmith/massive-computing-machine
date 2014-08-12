(** The engine of the game.

    The only thing in the game that mutates state.
*)

type t

val create : int -> t
(** seed is the int *)

val make_world : t -> Posn.t -> int -> World.t
(** Dimensions -> number of characters *)

val seed : t -> int
(** Return the dynamo's seed *)

val tick : t -> World.t -> World.t
(**
    Phases:
      - Each character 1) senses, then 2) decides
      - Collect all actions and environmental changes
      - the Dynamo applies these changes
*)
