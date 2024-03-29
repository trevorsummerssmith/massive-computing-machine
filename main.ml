open Core.Std
open Async.Std
open Cohttp_async

open Ezjsonm

(* This is dumb but fine for now.
   Both will be overriden on initialization below *)
let dynamo = ref (Dynamo.create 0)
let world = ref (Dynamo.make_world !dynamo (10, 10) 1)

let print_request req =
  let path = Uri.path (Cohttp.Request.uri req) in
  eprintf "REQ %s %s\n"
	  (Cohttp.(Code.string_of_method (Request.meth req)))
	  path

let respond ?(code=`OK) str =
  (* This is just a wrapper around Server.respond_with_string
     that first prints out the response
   *)
  eprintf "%d: %s\n\n" (Cohttp.Code.code_of_status code) str;
  (* Add Access-Control-Allow-Origin: * header to let me develop locally
   and not have to serve the html from here. *)
  let headers = Cohttp.Header.init () in
  let headers = Cohttp.Header.add headers "Access-Control-Allow-Origin" "*" in
  Server.respond_with_string ~code:code ~headers:headers str

let send_game () =
(* json the game then return it *)
  let json = Board.serialize !world.board in
  let ret = Ezjsonm.to_string json in
  respond ret

let advance_game () =
  (* Advance the game world one tick and return it *)
  let world' = Dynamo.tick !dynamo !world in
  world := world';
  send_game ()

let save_file_name prefix =
  let time = Time.now () in
  let time_str = Time.format time "%Y-%m-%d_%H-%M-%S" in
  String.concat [prefix; "-"; time_str; ".json"]

let save_game prefix world' =
  (* Receive a game world and save it to disk *)
  let filename = save_file_name prefix in
  Dynamo.save_game !dynamo world' filename;
  respond (String.concat ["{\"filename\" : \""; filename; "\"}"])

(* end game *)

let handler ~(body:Cohttp_async.Body.t) _sock req =
  print_request req;
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with 
  | "/test" ->
     Uri.get_query_param uri "hello"
  |> Option.map ~f:(fun v -> "hello: " ^ string_of_int((int_of_string v)+1))
  |> Option.value ~default:"No param hello supplied"
  |> Server.respond_with_string
  | "/game" -> send_game ()
  | "/advance-game" -> advance_game ()
  | "/save-game" -> begin
		    (* There's a ton of logic in here right now because this
                       gets a board from the front end (not a world with actors)
		       Later on, when I need it, I'll change the edit front end
		       to deal with the actor stuff as well, then this will get
		       cleaned up. *)
		    Cohttp_async.Body.to_string body
		    >>= fun s ->
		    Core.Std.eprintf "Got: %s\n" s;
		    let board = Board.deserialize (Ezjsonm.from_string s) in
		    (* We have a board choose some defaults (for now) for the
		     characters *)
		    let f posn = match (Board.get_tile board posn) with
			Board.Character _ -> true
		      | _ -> false in
		    let character_posns = Board.filter_board board ~f:f in
		    let make_actors p =
		      (* Everyone is random for now. *)
		      match (Board.get_tile board p) with
			Board.Character c -> Actor.make (module CBRandom) c
		      | _ -> failwith "Not possible"
		    in
		    let actors = ref (List.map ~f:make_actors character_posns) in
		    save_game "ok" {board; actors}
		  end
  | _ ->
     Server.respond_with_string ~code:`Not_found "Route not found"    
 
let start_server port world_width world_height num_characters rand_seed board_name () =
  eprintf "Initialized PRNG with random seed: %d\n" rand_seed;
  (match board_name with
     None -> begin
	    eprintf "Generating new game width: %d height: %d" world_width world_height;
	    dynamo := (Dynamo.create rand_seed);
	    world := (Dynamo.make_world !dynamo (world_width, world_height) num_characters)
	  end
  | Some filename -> begin
		     eprintf "Loading game from file %s" filename;
		     let (dynamo', world') = Dynamo.load_game filename in
		     let (w, h) = Board.dimensions world'.board in
		     eprintf "Successfully loaded game.\n";
		     eprintf "  World width: %d height: %d\n" w h;
		     eprintf "  Game has %d characters\n" (List.length !(world'.actors));
		     dynamo := dynamo';
		     world := world'
		   end);
  eprintf "Listening for HTTP on port %d\n" port;
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
     +> flag "-b" (optional file)
       ~doc:"filename.json Board file to use for the game. If not provided generatees one."
    ) start_server
    
  |> Command.run
