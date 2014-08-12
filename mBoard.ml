module type S = sig
    include Board.S

    val set_tile : t -> tile -> Posn.t -> unit
    (** Replace board's posn with tile *)

end

module T = (BoardImpl : S with type t = BoardImpl.t)
include T

