type umwelt =
    (** The world as it is experienced by a particular organism. *)
    {
      board : Board.t; (* TODO XXX Tmp for now *)
      character : Character.t;
    }

type action = Move of Posn.t | DoNothing | Eat

module type S = sig

    val name : string
    (** Name of this brain type *)

    val sense : Board.t -> Character.t -> umwelt

    val decide : umwelt -> action
end
