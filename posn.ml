(** Coordinates on the board in the form of (x,y).
    That are compareable and hashable.
    This entire module is really just 'type posn = int * int'.
*)

open Core.Std

module T = struct
  type t = int * int with sexp, compare
  let hash (x,y) =
    (Int.hash x) lxor (Int.hash y) (* TODO XXX make sure this is good *)
end

include T
include Comparable.Make(T)
include Hashable.Make(T)

let nowhere : t = (Int.max_value, Int.max_value)
(** We need initialization and this comes up. *)
