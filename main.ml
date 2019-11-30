
(*
open Graphics
(*############https://stackoverflow.com/questions/34509522/detect-window-closing-ocaml-graphics*)




let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
  in create_rows m n []

let print_tuple (a, b) = 
  print_endline ("(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")")

let draw_board m n h w = 
  let pairs_list = create_pairs m n in 
  let ten_magni = List.map (fun (x, y) -> 50 * x, 50 * y) pairs_list in 
  let height_width_added = List.map (fun (x, y) -> x, y, h, w) ten_magni in 
  List.map (fun (x, y, h, w) -> draw_rect x y h w) height_width_added |> ignore


let rec loop () = set_color red; (if button_down () then let (x, y) =  mouse_pos () in draw_circle x y 100); loop ()
let () =

  try

    Graphics.open_graph " 600x600";
    set_color blue;
    draw_board 10 10 50 50;
    loop ()

  with
  | Graphic_failure("fatal I/O error") ->
    print_string "Caught exception";
    print_newline ()


(*############https://stackoverflow.com/questions/34509522/detect-window-closing-ocaml-graphics*)

*)

(*

OLD STUFF : Probably Deprecated



let n_cols = 10
let n_rows = 10

let cHIT_CHAR = "X"
let cMISS_CHAR = "O"
let cUNKNOWN_CHAR  = "?"

type hit = 
  | Hit
  | Miss
  | Unknown

type damage = 
  | NoDamage
  | Damage

type ship_type = 
  | Battleship of damage
  | AircraftCarrier of damage
  | Destroyer of damage
  | Cruiser of damage
  | Submarine of damage

let print_player_guesses matrix =
  for i = 0 to (n_rows - 1) do
    let s = ref "" in 
    for j = 0 to (n_cols - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Hit -> cHIT_CHAR
        | Miss -> cMISS_CHAR
        | Unknown -> cUNKNOWN_CHAR  
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done

let print_player_ship_board matrix =
  for i = 0 to (n_rows - 1) do
    let s = ref "" in 
    for j = 0 to (n_cols - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Battleship NoDamage -> "B"
        | Battleship Damage -> "b"
        | AircraftCarrier NoDamage -> "A"
        | AircraftCarrier Damage -> "a"
        | Destroyer NoDamage -> "D"
        | Destroyer Damage -> "d"
        | Cruiser NoDamage -> "C"
        | Cruiser Damage -> "c"
        | Submarine NoDamage -> "S"
        | Submarine Damage -> "s"
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done


let func1 () = 
  let new_board1 = Array.make_matrix n_rows n_cols (AircraftCarrier NoDamage)
  in new_board1.(0).(1) <- Submarine Damage;
  new_board1.(9).(9) <- Destroyer Damage;
  new_board1.(8).(9) <- Destroyer Damage;
  print_player_ship_board new_board1;
  print_endline ""

let func2 () = 
  let new_board1 = Array.make_matrix n_rows n_cols (Unknown)
  in new_board1.(0).(1) <- Hit;
  new_board1.(9).(9) <- Miss;
  new_board1.(8).(9) <- Miss;
  print_player_guesses new_board1;
  print_endline ""

let () = func1 ()
let () = func2 ()

let rec get_name () = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name: \n");
  print_string  "> ";
  match read_line () with
  | s -> if s = "" then get_name () else s

type name = string
type player = 
  | Player1 of name
  | Player2 of name

let get_both_player_names () = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name Player One. \n");
  let plyr1 = get_name () in 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name Player Two. \n");
  let plyr2 = get_name () in 
  (plyr1, plyr2)

let () =  
  match get_both_player_names () with
  | (s1, s2) -> print_endline s1; print_endline s2
(*| _ -> failwith "illegal"*)

*)

(* ############ Opening All Modules needed ############# *)

open Command
open Battleship
open State

(* ############ Opening All Modules needed ############# *)

(* ############ Welcome Message ############# *)

let print_welcome_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to the 3110 Battleship Game.\n");
  ()

(* ############ Welcome Message ############# *)

type ai_difficulty = 
  | AIEasy
  | AIMedium

(* ########### MONADS ############# *)
(* MONAD in order to simplify code *)
type main_state = 
  | ContinueGame of (State.t option) * Battleship.t * bool * ai_difficulty
  | VictoryGame
  | EndGame

(* Monad bind *)
let (>>>) f x = 
  match x with
  | ContinueGame (Some state, battleship, ai_status, diff ) -> 
    f (Some state) battleship ai_status diff
  | ContinueGame (None, battleship, ai_status, diff) -> 
    f None battleship ai_status diff
  | VictoryGame -> VictoryGame
  | EndGame -> EndGame

(* initialize monad *)
let initialize_main () = 
  ContinueGame (None, Battleship.empty, false, AIEasy)

(* ########### MONADS ############# *)

(* ########### Process AI ############# *)

(* Determine if player wants AI or not *)
let print_ai_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease choose whether 
                  you would like to play against an AI or not.\n");
  ANSITerminal.(print_string [green]
                  "\n\nIf you choose to play against AI, 
                  you are player 1 and the AI is player 2.\n");
  ANSITerminal.(print_string [green]
                  "\nPlease Enter yes, no, or quit.\n")

let print_ai_failure_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease enter yes, no or quit.\n")

let print_difficulty_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease choose whether 
                  the difficulty of the AI.\n");
  ANSITerminal.(print_string [green]
                  "\n\nYou can choose easy or medium. .\n")

let print_difficulty_error () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease Enter yes, no, or quit.\n")

type check_ai = 
  | AIContinue of bool
  | AIQuit

let rec choose_ai () =
  print_ai_message ();
  match () |> read_line |> parse with
  | YesNo true -> AIContinue true
  | YesNo false -> AIContinue false
  | Quit -> AIQuit
  | _ -> print_ai_failure_message (); choose_ai ()

let determine_ai_status state battleship ai_status diff = 
  match choose_ai () with
  | AIContinue true -> ContinueGame (state, battleship, true, diff)
  | AIContinue false -> ContinueGame (state, battleship, false, diff)
  | AIQuit -> EndGame

let rec choose_difficulty () = 
  print_difficulty_message ();
  match () |> read_line |> parse_difficulty with
  | Easy -> AIEasy
  | Medium -> AIMedium
  | InvalidDifficulty -> 
    print_difficulty_error (); choose_difficulty ()

let determine_ai_difficulty state battleship ai_status diff = 
  if not ai_status then ContinueGame (state, battleship, ai_status, diff)
  else begin
    match choose_difficulty () with
    | AIEasy -> ContinueGame (state, battleship, ai_status, AIEasy)
    | AIMedium -> ContinueGame (state, battleship, ai_status, AIMedium)
  end

(* ########### Process AI ############# *)

(* ########### Announce Player turn to lay down ships ############# *)

let print_player1_add_ships state battleship ai_status diff = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease place your ships Player One. \n");
  ContinueGame (state, battleship, ai_status, diff)

let print_player2_add_ships state battleship ai_status diff = 
  ANSITerminal.(print_string [blue]
                  "\n\nPlease place your ships Player Two. \n");
  ContinueGame (state, battleship, ai_status, diff)

(* ########### Announce Player turn to lay down ships ############# *)

(* ########### Pregame : add ships to board ############# *)

let print_lay_down_ships_phase color = 
  ANSITerminal.(print_string [color]
                  "\n\nPlease lay down your ships on the map. \n");
  ANSITerminal.(print_string [color]
                  ("\n\nSpecify the placement as a column, followed
                  by a space, then the row, followed by the space,
                                   for where you want the head of the ship to be. Next
                                         add a space and put in either left, right, up or down
                                                                                      for the orientation of the ship. Finally,
                                                                                                             put in the type of ship, which is one
  of battleship, aircraft carrier, destroyer,
     cruiser or submarine. \n"))

let print_quit () = 
  ANSITerminal.(print_string [green]
                  "\n\nQuitting game.\n")

let print_syntax_error () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour declaration had an error. Try again.\n")

let print_wrong_phase () = 
  ANSITerminal.(print_string [green]
                  "\n\nWe are the pregame phase. Try again.\n")

let print_yes_no_phrase () = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is not a yes/no question. Try again.\n")

let print_off_gameboard () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour coordinates were not on the grid. Try again.\n")

let print_occupied_location () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour coordinates for the ship 
                  were occupied already. Try again.\n")

let print_placed_already () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou have placed all 
                  the ships of that type already. Try again.\n")

let print_remaining_ships player battleship = 
  let remaining_ships = 
    Battleship.(remaining_ships_to_place (choose_player player) battleship) in 
  print_endline "These are your remaining ships to place. ";
  List.map print_endline remaining_ships |> ignore

type pregame_state = 
  | PGQuit
  | PGContinue of Battleship.t

let parse_single_ship battleship player str = 
  match parse str with
  | Quit -> print_quit (); PGQuit
  | YesNo _ -> print_yes_no_phrase (); PGContinue battleship
  | InvalidCommand -> print_syntax_error (); PGContinue battleship
  | Target (_, _) -> print_wrong_phase (); PGContinue battleship
  | Valid (x, y, direction, ship) -> begin
      let direction' = string_to_direction direction in 
      let ship' = string_to_ship ship in 
      let player' = choose_player player in 
      match insert_ship (x, y) direction' ship' player' battleship with
      | Battleship.Success battleship' -> 
        print_player_ship_board battleship' (choose_player player);
        PGContinue battleship'
      | Battleship.Failure (battleship', BoundsError) ->
        print_off_gameboard ();
        PGContinue battleship'
      | Battleship.Failure (battleship', OccupiedTile) ->
        print_occupied_location ();
        PGContinue battleship'
      | Battleship.Failure (battleship', OutOfShips) ->
        print_placed_already ();
        PGContinue battleship'
      | Battleship.Failure (_, NonexistShip) ->
        failwith "Remove Unimplemented"
    end

let ai_to_string ((x, y), direction, ship) = 
  string_of_int (x) ^ "," ^ string_of_int (y) ^ "," ^ direction ^ "," ^ ship

let rec place_player1_ships state battleship ai_status diff = 
  if Battleship.(remaining_ships (choose_player true) battleship) = 0 
  then ContinueGame (state, battleship, ai_status, diff)
  else begin
    let () = print_remaining_ships true battleship in 
    let color = ANSITerminal.red in (* for player 1, use red *)
    print_lay_down_ships_phase color;
    let result_of_adding_ship = 
      () 
      |> read_line 
      |> parse_single_ship battleship true in
    match result_of_adding_ship with
    | PGQuit -> EndGame
    | PGContinue battleship' -> 
      place_player1_ships state battleship' ai_status diff
  end

let rec place_player2_ships state battleship ai_status diff = 
  if Battleship.(remaining_ships (choose_player false) battleship) = 0 
  then ContinueGame (state, battleship, ai_status, diff)
  else begin
    let () = print_remaining_ships false battleship in 
    let color = ANSITerminal.blue in (* for player 2, use blue *)
    print_lay_down_ships_phase color;
    if ai_status then 
      let ai_result = 
        battleship 
        |> randomly_laydown_ships 
        |> ai_to_string 
        |> parse_single_ship battleship false in begin
        match ai_result with
        | PGQuit -> EndGame
        | PGContinue battleship' -> 
          place_player2_ships state battleship' ai_status diff
      end
    else
      let result_of_adding_ship = 
        () 
        |> read_line 
        |> parse_single_ship battleship false in begin
        match result_of_adding_ship with
        | PGQuit -> EndGame
        | PGContinue battleship' -> 
          place_player2_ships state battleship' ai_status diff
      end
  end

(* ########### Pregame : add ships to board ############# *)

(* ########### Entering Trageting Phase ############# *)

let print_entering_targeting_phase state battleship ai_status diff = 
  ANSITerminal.(print_string [green]
                  "We are entering the targeting phase of the game.\n");
  ANSITerminal.(print_string [green]
                  "Player 1 will target first and alternate with player 2.\n");
  ANSITerminal.(print_string [green]
                  "The game continues until victory or a player quits. \n");
  ContinueGame (state, battleship, ai_status, diff)

let build_in_game_state state battleship ai_status diff = 
  if ai_status then 
    let in_game_state = 
      initialize_ai true false 
        (get_player_dict (choose_player true) battleship) 
        (get_player_dict (choose_player false) battleship) in 
    ContinueGame (Some in_game_state, battleship, ai_status, diff)
  else
    let in_game_state = 
      init_state true false 
        (get_player_dict (choose_player true) battleship) 
        (get_player_dict (choose_player false) battleship) in 
    ContinueGame (Some in_game_state, battleship, ai_status, diff)

(* ########### Entering Trageting Phase ############# *)

(* ########### In Game ############# *)

let print_player_move_message player = 
  if player then ANSITerminal.(print_string [red]
                                 "\n\nPlayer One, it is your turn to move\n")
  else ANSITerminal.(print_string [blue]
                       "\n\nPlayer Two, it is your turn to move\n")

let print_opponent_grid player state = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your opponent grid.\n");
  State.print_guesses (State.bool_to_player player) state;
  print_newline ()

let print_player_grid player state = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your grid.\n");      
  State.print_player_dict (State.bool_to_player player) state;
  print_newline ()

let print_boards_side_by_side player state = 
  ANSITerminal.(print_string [green]
                  "\n\nYour grid is the left and opponent is the right.\n");
  let player_dict = 
    State.string_of_player_dict (State.bool_to_player player) state in 
  let player_guesses = 
    State.string_of_guesses (State.bool_to_player player) state in 
  let combined = State.combine_boards player_dict player_guesses in 
  State.print_boards combined

let print_targeting_rules color = 
  ANSITerminal.(print_string [color]
                  "\n\nPlease target a location on the enemy map. \n");
  ANSITerminal.(print_string [color]
                  "\n\nSpecify the placement as a column by typing in
target, a comma, the x coordinate, comma,
and the y coordinate. \n")

let print_in_main_phase () = 
  ANSITerminal.(print_string [green]
                  "\n\nWe are no longer in the pregame phase. Try again.\n")

let print_already_targeted () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou cannot target a location you previously targeted.\n")

let hit_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou hit a ship.\n")

let miss_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou did not hit a ship.\n")

let sink_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou sunk a ship.\n")   

let not_sink_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou did not sink a ship.\n")

let after_move_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nThese are the grids after you made your move.\n")

let print_sunk_hit_message ship_hit ship_sunk = 
  (if ship_hit 
   then hit_ship_message ()
   else miss_ship_message ()) |> ignore;
  (if ship_sunk 
   then sink_ship_message ()
   else not_sink_ship_message ()) |> ignore

let print_player1_wins () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlayer One, you win!.\n")

let print_player2_wins ai_status = 
  if ai_status then ANSITerminal.(print_string [green]
                                    "\n\nThe AI wins!.\n")
  else ANSITerminal.(print_string [green]
                       "\n\nPlayer Two, you win!.\n")

let print_winner player ai_status = 
  (if player then print_player1_wins ()
   else print_player2_wins ai_status) |> ignore

let get_state_from state_option = 
  match state_option with
  | Some state -> state
  | None -> failwith "No state was given. "

let choose_color player = 
  if player then ANSITerminal.red 
  else ANSITerminal.blue

let get_dfficulty_targeting_func diff = 
  match diff with
  | AIEasy -> target_ai
  | AIMedium -> target_medium_ai

let legal_target rec_func x y player state battleship ai_status diff = 
  match target_ship (x, y) (bool_to_player player) state with
  | State.Failure (new_state, CoordinateVisited) ->
    print_already_targeted (); 
    rec_func (Some new_state) battleship ai_status diff
  | State.Failure (new_state, OutOfBounds) ->
    print_off_gameboard (); rec_func (Some new_state) battleship ai_status diff
  | State.Success (new_state, ship_hit, ship_sunk) ->
    after_move_message ();
    print_boards_side_by_side player new_state;
    (*print_opponent_grid player new_state;
      print_player_grid player new_state; *)
    print_sunk_hit_message ship_hit ship_sunk;
    if check_victory (bool_to_player player) new_state 
    then let () = print_winner player ai_status in VictoryGame
    else rec_func (Some (new_state |> update_player)) battleship ai_status diff

let rec target state_option battleship ai_status diff = 
  let state = get_state_from state_option in 
  let player = get_current_player state in
  let color = choose_color player in 
  print_player_move_message player;
  print_boards_side_by_side player state;
  (*print_opponent_grid player state;
    print_player_grid player state;*)
  print_targeting_rules color;
  if player = false && ai_status then 
    let targeting_func = get_dfficulty_targeting_func diff in 
    let (x, y), new_state = targeting_func state in 
    legal_target 
      target (x - 1) (y - 1) player new_state battleship ai_status diff
  else begin
    match () |> read_line |> parse with
    | YesNo _ -> 
      print_yes_no_phrase (); target (Some state) battleship ai_status diff
    | Quit -> 
      print_quit (); EndGame
    | InvalidCommand -> 
      print_syntax_error (); target (Some state) battleship ai_status diff
    | Valid (_, _, _, _) -> 
      print_in_main_phase (); target (Some state) battleship ai_status diff
    | Target (x, y) -> 
      legal_target target x y player state battleship ai_status diff
  end


(* ########### In Game ############# *)

(* ########### End Game ############# *)

let print_ending_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nQuitting game.\n")

let print_victory_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nGood game. Game over.\n")

let finish_game game_status = 
  match game_status with
  | ContinueGame _ -> failwith "Illegal: Battleship games always terminate. "
  | EndGame -> print_ending_message ();
  | VictoryGame -> print_victory_message ()

(* ########### End Game ############# *)

(* ########### Running Game ############# *)

let () = 
  ()
  |> print_welcome_message 
  |> initialize_main 
  |> (>>>) determine_ai_status
  |> (>>>) determine_ai_difficulty
  |> (>>>) print_player1_add_ships
  |> (>>>) place_player1_ships 
  |> (>>>) print_player2_add_ships
  |> (>>>) place_player2_ships
  |> (>>>) print_entering_targeting_phase 
  |> (>>>) build_in_game_state
  |> (>>>) target
  |> finish_game

(* ########### Running Game ############# *)


(*

let ai_message = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease choose whether you would like to play against an AI or not.\n");
  ANSITerminal.(print_string [green]
                  "\n\nIf you choose to play against AI, you are player 1 and the AI is player 2.\n");
  ANSITerminal.(print_string [green]
                  "\n\nEnter yes or no below.\n")

let ai_failure_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease enter yes, no or quit.\n")


type ai_check = 
  | AIContinue of bool
  | AIQuit

let rec choose_ai () =
  ai_message;
  match () |> read_line |> Command.parse with
  | Command.YesNo true -> AIContinue true
  | Command.YesNo false -> AIContinue false
  | Command.Quit -> AIQuit
  | _ -> ai_failure_message (); choose_ai ()

type pre_game_state = 
  | Quit
  | Continue of Battleship.t

let print_lay_down_ships_phase color = 
  ANSITerminal.(print_string [color]
                  "\n\nPlease lay down your ships on the map. \n");
  ANSITerminal.(print_string [color]
                  {|\n\nSpecify the placement as a column, followed
by a space, then the row, followed by the space,
for where you want the head of the ship to be. Next
add a space and put in either left, right, up or down
for the orientation of the ship. Finally,
put in the type of ship, which is one
of battleship, aircraft carrier, destroyer,
cruiser or submarine. \n|})

let quit_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nQuitting game.\n")
let syntax_error_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour declaration had an error. Try again.\n")
let pre_game_phase_error_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nWe are the pregame phase. Try again.\n")
let pre_game_bounds_error_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour coordinates were not on the grid. Try again.\n")
let pre_game_occupied_error_message () = 
  ANSITerminal.(print_string [green]
                  {|\n\nYour coordinates for the ship 
                  were occupied already. Try again.\n|})
let pre_game_ship_type_placed_error_message () = 
  ANSITerminal.(print_string [green]
                  {|\n\nYou have placed all 
                  the ships of that type already. Try again.\n|})

let rec parse_lay_down_ship recursive_func pre_game player s = 
  match Command.parse s with
  | Command.YesNo _ -> failwith "unimplemented"
  | Command.Quit -> quit_message (); Quit
  | Command.InvalidCommand -> 
    syntax_error_message ();
    recursive_func pre_game player
  | Command.Target (_, _) ->
    pre_game_phase_error_message ();
    recursive_func pre_game player
  | Command.Valid (x, y, direction, ship) -> begin
      match 
        Battleship.(insert_ship 
                      (x, y) 
                      (string_to_direction direction) 
                      (string_to_ship ship) 
                      (choose_player player) 
                      pre_game)
      (*(Battleship.insert_ship (x, y) 
         (Battleship.string_to_direction direction) 
         (Battleship.string_to_ship ship) 
         (Battleship.choose_player player) pre_game)  *)
      with
      | Battleship.Success new_pre_game -> 
        Battleship.print_player_ship_board new_pre_game (Battleship.choose_player player);
        Continue new_pre_game
      | Battleship.Failure (p_game, Battleship.BoundsError) ->
        pre_game_bounds_error_message ();
        recursive_func p_game player
      | Battleship.Failure (p_game, Battleship.OccupiedTile) ->
        pre_game_occupied_error_message ();
        recursive_func p_game player
      | Battleship.Failure (p_game, Battleship.OutOfShips) ->
        pre_game_ship_type_placed_error_message ();
        recursive_func p_game player
      | Battleship.Failure (_, Battleship.NonexistShip) ->
        failwith "remove not implemented yet"
    end



let rec lay_down_ship pre_game player =
  let color = 
    if player then ANSITerminal.red 
    else ANSITerminal.blue 
  in print_lay_down_ships_phase color;
  () |> read_line |> parse_lay_down_ship lay_down_ship pre_game player 

let ai_to_string ((x, y), direction, ship) = 
  string_of_int (x) ^ "," ^ string_of_int (y) ^ "," ^ direction ^ "," ^ ship

let rec lay_down_ai_ship pre_game player =
  let color = 
    if player then ANSITerminal.red 
    else ANSITerminal.blue 
  in print_lay_down_ships_phase color;
  if player then () |> read_line |> parse_lay_down_ship lay_down_ship pre_game player 
  else pre_game |> Battleship.randomly_laydown_ships |> ai_to_string |> parse_lay_down_ship lay_down_ai_ship pre_game player 

(*match read_line () with
  | s -> 
   match Command.parse s with
     | Command.Quit -> 
     ANSITerminal.(print_string [green]
                     "\n\nQuitting game.\n");
     Quit
     | Command.InvalidCommand -> 
     ANSITerminal.(print_string [green]
                     "\n\nYour declaration had an error. Try again.\n");
     lay_down_ship pre_game player
     | Command.Target (_, _) ->
     ANSITerminal.(print_string [green]
                     "\n\nWe are the pregame phase. Try again.\n");
     lay_down_ship pre_game player
     | Command.Valid (x, y, direction, ship) -> 
     match (Battleship.insert_ship (x, y) (Battleship.string_to_direction direction) (Battleship.string_to_ship ship) (Battleship.choose_player player) pre_game) with
     | Battleship.Success new_pre_game -> 
       Battleship.print_player_ship_board new_pre_game (Battleship.choose_player player);
       Continue new_pre_game
     | Battleship.Failure (p_game, Battleship.BoundsError) ->
       ANSITerminal.(print_string [green]
                       "\n\nYour coordinates were not on the grid. Try again.\n");
       lay_down_ship p_game player
     | Battleship.Failure (p_game, Battleship.OccupiedTile) ->
       ANSITerminal.(print_string [green]
                       "\n\nYour coordinates for the ship were occupied already. Try again.\n");
       lay_down_ship p_game player
     | Battleship.Failure (p_game, Battleship.OutOfShips) ->
       ANSITerminal.(print_string [green]
                       "\n\nYou have placed all the ships of that type already. Try again.\n");
       lay_down_ship p_game player
     | Battleship.Failure (_, Battleship.NonexistShip) ->
       failwith "remove not implemented yet" *)

let rec lay_down_player_ships pre_game player = 
  if Battleship.remaining_ships (Battleship.choose_player player) pre_game = 0 then Continue pre_game
  else 
    let remaining_ships = (Battleship.remaining_ships_to_place (Battleship.choose_player player) pre_game) in 
    print_endline "These are your remaining ships to place. ";
    List.map print_endline remaining_ships |> ignore;
    match lay_down_ship pre_game player with
    | Quit -> Quit
    | Continue new_pre_game -> lay_down_player_ships new_pre_game player

let rec lay_down_player_ships_with_ai pre_game player = 
  if Battleship.remaining_ships (Battleship.choose_player player) pre_game = 0 then Continue pre_game
  else 
    let remaining_ships = (Battleship.remaining_ships_to_place (Battleship.choose_player player) pre_game) in 
    print_endline "These are your remaining ships to place. ";
    List.map print_endline remaining_ships |> ignore;
    match lay_down_ai_ship pre_game player with
    | Quit -> Quit
    | Continue new_pre_game -> lay_down_player_ships_with_ai new_pre_game player

let lay_down_both_ships pre_game = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease place your ships Player One. \n");
  let result_1 = 
    lay_down_player_ships pre_game true in 
  match result_1 with
  | Quit -> Quit
  | Continue new_pre_game ->
    ANSITerminal.(print_string [blue]
                    "\n\nPlease place your ships Player Two. \n");
    let result_2 = lay_down_player_ships new_pre_game false in 
    match result_2 with
    | Quit -> Quit
    | Continue final_pre_game -> Continue final_pre_game

let lay_down_both_ships_with_ai pre_game = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease place your ships Player One. \n");
  let result_1 = 
    lay_down_player_ships_with_ai pre_game true in 
  match result_1 with
  | Quit -> Quit
  | Continue new_pre_game ->
    ANSITerminal.(print_string [blue]
                    "\n\nPlease place your ships Player Two. \n");
    let result_2 = lay_down_player_ships_with_ai new_pre_game false in 
    match result_2 with
    | Quit -> Quit
    | Continue final_pre_game -> Continue final_pre_game

let pre_game pre_game = 
  lay_down_both_ships pre_game

let pre_game_with_ai pre_game = 
  lay_down_both_ships_with_ai pre_game

type game_state = 
  | End
  | Victory
  | ContinueGame of State.t

let rec target_ship_with_ai state = 
  let player = State.get_current_player state in
  (if player then ANSITerminal.(print_string [green]
                                  "\n\nPlayer One, it is your turn to move\n")
   else ANSITerminal.(print_string [green]
                        "\n\nPlayer Two, it is your turn to move\n"));
  let color = 
    if player then ANSITerminal.red 
    else ANSITerminal.blue in 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your opponent grid.\n");
  State.print_guesses (State.bool_to_player player) state;
  print_newline ();
  ANSITerminal.(print_string [green]
                  "\n\nThis is your grid.\n");      
  State.print_player_dict (State.bool_to_player player) state;
  print_newline ();
  ANSITerminal.(print_string [color]
                  "\n\nPlease target a location on the enemy map. \n");
  ANSITerminal.(print_string [color]
                  "\n\nSpecify the placement as a column by typing in
target, a comma, the x coordinate, comma,
and the y coordinate. \n");
  begin if player then 
      match read_line () with
      | s -> 
        match Command.parse s with
        | Command.YesNo _ -> failwith "Unimplemented"
        | Command.Quit -> 
          ANSITerminal.(print_string [green]
                          "\n\nQuitting game.\n");
          End
        | Command.InvalidCommand -> 
          ANSITerminal.(print_string [green]
                          "\n\nYour declaration had an error. Try again.\n");
          target_ship_with_ai state
        | Command.Valid (x, y, direction, ship) -> 
          ANSITerminal.(print_string [green]
                          "\n\nWe are no longer in the pregame phase. Try again.\n");
          target_ship_with_ai state
        | Command.Target (x, y) ->
          match State.target_ship (x, y) (State.bool_to_player player) state with
          | State.Failure (new_state, State.CoordinateVisited) ->
            ANSITerminal.(print_string [green]
                            "\n\nYou cannot target a location you previously targeted.\n");
            target_ship_with_ai new_state
          | State.Failure (new_state, State.OutOfBounds) ->
            ANSITerminal.(print_string [green]
                            "\n\nYour coordinate to target was out of bounds.\n");
            target_ship_with_ai new_state
          | State.Success (new_state, ship_hit, ship_sunk) ->
            ANSITerminal.(print_string [green]
                            "\n\nThis is your opponent grid.\n");
            State.print_guesses (State.bool_to_player player) new_state;
            print_newline ();
            ANSITerminal.(print_string [green]
                            "\n\nThis is your grid.\n");      
            State.print_player_dict (State.bool_to_player player) new_state;
            print_newline ();
            (if ship_hit 
             then ANSITerminal.(print_string [green]
                                  "\n\nYou hit a ship.\n")
             else ANSITerminal.(print_string [green]
                                  "\n\nYou did not hit a ship.\n")) |> ignore;
            (if ship_sunk 
             then ANSITerminal.(print_string [green]
                                  "\n\nYou sunk a ship.\n")
             else ANSITerminal.(print_string [green]
                                  "\n\nYou did not sink a ship.\n")) |> ignore;

            if State.check_victory (State.bool_to_player player) new_state 
            then 
              (
                (if player then ANSITerminal.(print_string [green]
                                                "\n\nPlayer One, you win!.\n")
                 else ANSITerminal.(print_string [green]
                                      "\n\nPlayer Two, you win!.\n")) |> ignore;
                Victory)              
            else ContinueGame (new_state |> State.update_player)
    else 
      let (x, y), state' = State.target_ai state in 
      match State.target_ship (x - 1, y - 1) (State.bool_to_player player) state' with
      | State.Failure (new_state, State.CoordinateVisited) ->
        ANSITerminal.(print_string [green]
                        "\n\nYou cannot target a location you previously targeted.\n");
        target_ship_with_ai new_state
      | State.Failure (new_state, State.OutOfBounds) ->
        ANSITerminal.(print_string [green]
                        "\n\nYour coordinate to target was out of bounds.\n");
        target_ship_with_ai new_state
      | State.Success (new_state, ship_hit, ship_sunk) ->
        ANSITerminal.(print_string [green]
                        "\n\nThis is your opponent grid.\n");
        State.print_guesses (State.bool_to_player player) new_state;
        print_newline ();
        ANSITerminal.(print_string [green]
                        "\n\nThis is your grid.\n");      
        State.print_player_dict (State.bool_to_player player) new_state;
        print_newline ();
        (if ship_hit 
         then ANSITerminal.(print_string [green]
                              "\n\nYou hit a ship.\n")
         else ANSITerminal.(print_string [green]
                              "\n\nYou did not hit a ship.\n")) |> ignore;
        (if ship_sunk 
         then ANSITerminal.(print_string [green]
                              "\n\nYou sunk a ship.\n")
         else ANSITerminal.(print_string [green]
                              "\n\nYou did not sink a ship.\n")) |> ignore;

        if State.check_victory (State.bool_to_player player) new_state 
        then 
          (
            (if player then ANSITerminal.(print_string [green]
                                            "\n\nPlayer One, you win!.\n")
             else ANSITerminal.(print_string [green]
                                  "\n\nPlayer Two, you win!.\n")) |> ignore;
            Victory)              
        else ContinueGame (new_state |> State.update_player)
  end


let rec target_ship state = 
  let player = State.get_current_player state in
  (if player then ANSITerminal.(print_string [green]
                                  "\n\nPlayer One, it is your turn to move\n")
   else ANSITerminal.(print_string [green]
                        "\n\nPlayer Two, it is your turn to move\n"));
  let color = 
    if player then ANSITerminal.red 
    else ANSITerminal.blue in 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your opponent grid.\n");
  State.print_guesses (State.bool_to_player player) state;
  print_newline ();
  ANSITerminal.(print_string [green]
                  "\n\nThis is your grid.\n");      
  State.print_player_dict (State.bool_to_player player) state;
  print_newline ();
  ANSITerminal.(print_string [color]
                  "\n\nPlease target a location on the enemy map. \n");
  ANSITerminal.(print_string [color]
                  "\n\nSpecify the placement as a column by typing in
target, a comma, the x coordinate, comma,
and the y coordinate. \n");
  match read_line () with
  | s -> 
    match Command.parse s with
    | Command.YesNo _ -> failwith "Unimplemented"
    | Command.Quit -> 
      ANSITerminal.(print_string [green]
                      "\n\nQuitting game.\n");
      End
    | Command.InvalidCommand -> 
      ANSITerminal.(print_string [green]
                      "\n\nYour declaration had an error. Try again.\n");
      target_ship state
    | Command.Valid (x, y, direction, ship) -> 
      ANSITerminal.(print_string [green]
                      "\n\nWe are no longer in the pregame phase. Try again.\n");
      target_ship state
    | Command.Target (x, y) ->
      match State.target_ship (x, y) (State.bool_to_player player) state with
      | State.Failure (new_state, State.CoordinateVisited) ->
        ANSITerminal.(print_string [green]
                        "\n\nYou cannot target a location you previously targeted.\n");
        target_ship new_state
      | State.Failure (new_state, State.OutOfBounds) ->
        ANSITerminal.(print_string [green]
                        "\n\nYour coordinate to target was out of bounds.\n");
        target_ship new_state
      | State.Success (new_state, ship_hit, ship_sunk) ->
        ANSITerminal.(print_string [green]
                        "\n\nThis is your opponent grid.\n");
        State.print_guesses (State.bool_to_player player) new_state;
        print_newline ();
        ANSITerminal.(print_string [green]
                        "\n\nThis is your grid.\n");      
        State.print_player_dict (State.bool_to_player player) new_state;
        print_newline ();
        (if ship_hit 
         then ANSITerminal.(print_string [green]
                              "\n\nYou hit a ship.\n")
         else ANSITerminal.(print_string [green]
                              "\n\nYou did not hit a ship.\n")) |> ignore;
        (if ship_sunk 
         then ANSITerminal.(print_string [green]
                              "\n\nYou sunk a ship.\n")
         else ANSITerminal.(print_string [green]
                              "\n\nYou did not sink a ship.\n")) |> ignore;

        if State.check_victory (State.bool_to_player player) new_state 
        then 
          (
            (if player then ANSITerminal.(print_string [green]
                                            "\n\nPlayer One, you win!.\n")
             else ANSITerminal.(print_string [green]
                                  "\n\nPlayer Two, you win!.\n")) |> ignore;
            Victory)              
        else ContinueGame (new_state |> State.update_player)

let rec main_game_loop_with_ai state = 
  match target_ship_with_ai state with
  | End ->  
    ANSITerminal.(print_string [green]
                    "\n\nQuitting game.\n");
  | Victory -> 
    ANSITerminal.(print_string [green]
                    "\n\nGood game. Game over.\n");
  | ContinueGame new_state -> 
    main_game_loop_with_ai new_state

let run_game_with_ai () = 
  match pre_game_with_ai (Battleship.empty) with
  | Quit -> print_endline "Closing game";
  | Continue final_pre_game -> 
    main_game_loop_with_ai
      (State.initialize_ai
         true false 
         (Battleship.get_player_dict (Battleship.choose_player true) final_pre_game) 
         (Battleship.get_player_dict (Battleship.choose_player false) final_pre_game))


let rec main_game_loop state =
  match target_ship state with
  | End ->  
    ANSITerminal.(print_string [green]
                    "\n\nQuitting game.\n");
  | Victory -> 
    ANSITerminal.(print_string [green]
                    "\n\nGood game. Game over.\n");
  | ContinueGame new_state -> 
    main_game_loop new_state

let run_game () = 
  match pre_game (Battleship.empty) with
  | Quit -> print_endline "Closing game";
  | Continue final_pre_game -> 
    main_game_loop
      (State.init_state 
         true false 
         (Battleship.get_player_dict (Battleship.choose_player true) final_pre_game) 
         (Battleship.get_player_dict (Battleship.choose_player false) final_pre_game)) 


let determine_ai () = 
  match choose_ai () with
  | AIContinue true -> run_game_with_ai ()
  | AIContinue false -> run_game () 
  | AIQuit -> print_endline "Closing game"

let main () = 
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to the 3110 Battleship Game.\n");
  determine_ai ()
(*run_game ()*)

let () = main ()


(*
open Battleship
open Command
open State
let get_coord lst = 
  (List.nth lst ((List.length lst)-2), List.nth lst ((List.length lst)-1))

let rec get_ship_name lst acc= 
  match lst with 
  | a :: b :: [] -> acc 
  | h :: t -> get_ship_name t (h ^ " " ^ acc)
  | _ -> acc

let rec continue_game state bs = 
  failwith "unimplemented"

let rec place_ship player = 
  if player = "player 1" then 
    begin
      match read_line () with 
      | exception End_of_file -> ()
      | comm ->
        match (parse comm) with 
        | Finished -> 
          if player = "player 1" then
            begin
              if true (* change later *) then place_ship "player 2"
              else print_endline "Can't finish unless all ships placed!";
              place_ship player
            end
        | Place lst -> 
          let (x, y) = get_coord lst in 
          let ship = get_ship_name lst in 
          failwith "unimplemented"
        | Remove lst -> 
          let (x, y) = get_coord lst in 
          failwith "unimplemented"
        | Quit ->
          print_endline "You've quit the game. Goodbye!\n";
          exit 0
        | _ -> print_endline "Invalid command during placing ships!";
          place_ship player
    end
  else 
    begin
    end

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Battleship Game.\n");
  print_endline "Please place your ships on the grid!";
  place_ship "player_1"

let () = main ()
*)

*)