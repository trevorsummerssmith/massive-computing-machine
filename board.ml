module type S = sig
    type t

    type tile =
	EmptyTile
      | Hole (* Cannot go through a hole *)
      | Hill (* Hills are harder to go over *)
      | Character of Character.t

    val create : Posn.t -> t
    (** width,height -> board Tiles are initialized to EmptyTile *)

    val dimensions : t -> Posn.t

    val get_tile : t -> Posn.t -> tile

    val chomp_coords : t -> Posn.t -> Posn.t
    (** Returns a posn inside of width, height, assuming the width and height wrap
     TODO Figure out where this belongs...
     *)

    val neighbors : t -> Posn.t -> Posn.t list
    (** Returns the 8 neighbors of a (x,y) in valid board coordinates. *)

    val tile_movement_cost : tile -> int
    (** Relative cost to move to a tile. This is a property inherit to the tile.
        So, an emptytile has cost 1. A hill might have cost 3.
     *)

    val serialize : t -> Ezjsonm.t
    (** Serialize to json *)

end

module T = (BoardImpl : S with type t = BoardImpl.t)
include T
