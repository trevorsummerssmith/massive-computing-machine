(** Random stuff *)

let random_letter () : string =
  Char.escaped (char_of_int (int_of_char 'a' + Random.int 26))

let random_name () : string =
  let len = (Random.int 20) + 4 in
  String.concat "" (Array.to_list (Array.init len (fun _ -> random_letter ())))

let random_posn (x, y) =
  (** Draws a random posn [0, x], [0, y]*)
  (Random.int x, Random.int y)
