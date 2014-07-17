open Core.Std
open Async.Std
open Cohttp_async

open Ezjsonm

(* start game *)
open Game

(* This is dumb but fine for now. *)
let world = ref (make_world 10 10 0)

let send_game () =
(* json the game then return it *)
  let board = serialize_world !world in
  let json = dict [
    ("width", int !world.width);
    ("height", int !world.height);
    ("board", board)
  ] in
  let ret = Ezjsonm.to_string json
  in
  Server.respond_with_string ret

let advance_game () =
  (* Advance the game world one tick and return it *)
  let world' = advance_world !world in
  world := world';
  send_game ()

(* end game *)

let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with 
  | "/test" ->
     Uri.get_query_param uri "hello"
  |> Option.map ~f:(fun v -> "hello: " ^ string_of_int((int_of_string v)+1))
  |> Option.value ~default:"No param hello supplied"
  |> Server.respond_with_string
  | "/game" -> send_game ()
  | "/advance-game" -> advance_game ()
  | _ ->
     Server.respond_with_string ~code:`Not_found "Route not found"    
 
let start_server port world_width world_height num_characters rand_seed () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Initialized PRNG with random seed: %d\n" rand_seed;
  Game.init_random rand_seed;
  world := make_world world_width world_height num_characters;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler
  >>= fun _ -> Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start a hello world Async server"
    Command.Spec.
    (empty
     +> flag "-p" (optional_with_default 8080 int)
       ~doc:"int Source port to listen on. Defaults to 8080"
     +> flag "-world-width" (optional_with_default 100 int)
       ~doc:"int Width of the world. Defaults to 100"
     +> flag "-world-height" (optional_with_default 100 int)
       ~doc:"int Height of the world. Defaults to 100"
     +> flag "-num-characters" (optional_with_default 20 int)
       ~doc:"int Number of characters to place on the world. Defaults to 20"
     +> flag "-rand-seed" (optional_with_default (Core.Std.Float.to_int (Unix.time ())) int)
       ~doc:"int Seed for PRNG. Defaults to epoch time."
    ) start_server
    
  |> Command.run
