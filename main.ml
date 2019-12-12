(* ############ Opening All Modules needed ############# *)

open Command
open Battleship
open State
open Hard_ai

(* ############ Opening All Modules needed ############# *)

(* ############ Welcome Message ############# *)

let credits () =  
  {| 
  _____  _____   ____  __ __  ___    ____        _   _   _           _     _       
 / ____|/ ____| |___ \/_ /_ |/ _ \  |  _ \      | | | | | |         | |   (_)      
| |    | (___     __) || || | | | | | |_) | __ _| |_| |_| | ___  ___| |__  _ _ __  
| |     \___ \   |__ < | || | | | | |  _ < / _` | __| __| |/ _ \/ __| '_ \| | '_ \ 
| |____ ____) |  ___) || || | |_| | | |_) | (_| | |_| |_| |  __/\__ \ | | | | |_) |
 \_____|_____/  |____/ |_||_|\___/  |____/ \__,_|\__|\__|_|\___||___/_| |_|_| .__/ 
                                                                            | |    
                                                                            |_|    
 ____                 _                   _   _                   _______                 __ _     _ _   ___   _____  
|  _ \       _       | |                 | | | |                 |__   __|               / /(_)   | | | / _ \ / _ \ \ 
| |_) |_   _(_)      | | ___  _ __   __ _| |_| |__   __ _ _ __      | |_ __ __ _ _ __   | |  _  __| | || (_) | (_) | |
|  _ <| | | |    _   | |/ _ \| '_ \ / _` | __| '_ \ / _` | '_ \     | | '__/ _` | '_ \  | | | |/ _` | __\__, |> _ <| |
| |_) | |_| |_  | |__| | (_) | | | | (_| | |_| | | | (_| | | | |    | | | | (_| | | | | | | | | (_| | |_  / /| (_) | |
|____/ \__, (_)  \____/ \___/|_| |_|\__,_|\__|_| |_|\__,_|_| |_|    |_|_|  \__,_|_| |_| | | | |\__,_|\__|/_/  \___/| |
        __/ |                                                                            \_\/ |                   /_/ 
       |___/                                                                              |__/                        
        ____       _                    _                           ___     _ ___  _____ _  _ __  
       |  _ \     (_)                  | |                         / / |   (_)__ \| ____| || |\ \ 
       | |_) |_ __ _  __ _ _ __        | | ___  ___  _ __   __ _  | || |__  _   ) | |__ | || |_| |
       |  _ <| '__| |/ _` | '_ \   _   | |/ _ \/ _ \| '_ \ / _` | | || '_ \| | / /|___ \|__   _| |
       | |_) | |  | | (_| | | | | | |__| |  __/ (_) | | | | (_| | | || |_) | |/ /_ ___) |  | | | |
       |____/|_|  |_|\__,_|_| |_|  \____/ \___|\___/|_| |_|\__, | | ||_.__/| |____|____/   |_| | |
                                                            __/ |  \_\    _/ |                /_/ 
                                                           |___/         |__/                     
        ______              _____      _    _    _          __          ___  ____   _____  
       |  ____|            / ____|    | |  | |  (_)        / /         |__ \|___ \ / _ \ \ 
       | |__   __ _  ___  | (___   ___| | _| | ___ _ __   | | ___  __ _   ) | __) | (_) | |
       |  __| / _` |/ _ \  \___ \ / _ \ |/ / |/ / | '_ \  | |/ _ \/ _` | / / |__ < > _ <| |
       | |___| (_| |  __/  ____) |  __/   <|   <| | | | | | |  __/ (_| |/ /_ ___) | (_) | |
       |______\__, |\___| |_____/ \___|_|\_\_|\_\_|_| |_| | |\___|\__, |____|____/ \___/| |
               __/ |                                       \_\     __/ |               /_/ 
              |___/                                               |___/                    |}


(** [print_welcome_message ()] has the effect of printing the welcome message
    on the screen, then giving unit. *)
let print_welcome_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to the 3110 Battleship Game.\n");
  ANSITerminal.scroll 20;
  Unix.sleep 2;
  () |> credits |> print_endline;
  Unix.sleep 2;
  ANSITerminal.erase Screen;
  ()

(* ############ Welcome Message ############# *)

(* [ai_difficulty] represents the difficulty of the AI *)
type ai_difficulty = 
  | AIEasy
  | AIMedium
  | AIHard
  | AIInsane

(* ########### Instantiate Hard AI ############# *)

let hard_ai = initialize_hard_ai

(* ########### Instantiate Hard AI ############# *)

(* ########### MONADS ############# *)
(* MONAD in order to simplify code : Actually not exactly a monad since
   it violates all the monad laws. *)

(* [main_state] represents what state the game is in:
   [ContinueGame] to contine, [VictoryGame] if the game is won
   and [EndGame] if the game has been quit. *)
type main_state = 
  | ContinueGame 
    of (State.t option) * Battleship.t * bool * ai_difficulty * Hard_ai.t
  | VictoryGame
  | EndGame

(* Monad bind *)
(** [f >>> x] is the new game state with [f] applied to state [x]. *)
let (>>>) f x = 
  match x with
  | ContinueGame (Some state, battleship, ai_status, diff, ai) -> 
    f (Some state) battleship ai_status diff ai
  | ContinueGame (None, battleship, ai_status, diff, ai) -> 
    f None battleship ai_status diff ai
  | VictoryGame -> VictoryGame
  | EndGame -> EndGame

(* initialize monad *)
(** [initialize_main ()] is almost like return of a monad,
    and it has the effect of initalizing the game state. *)
let initialize_main () = 
  ContinueGame (None, Battleship.empty, false, AIEasy, hard_ai)

(* ########### MONADS ############# *)

(* ########### Process AI ############# *)

(* Determine if player wants AI or not *)

(** [print_ai_message ()] has the effect of asking the player
    if he wants an AI or not. *)
let print_ai_message () = 
  ANSITerminal.(
    print_string [green]
      "\n\nPlease choose if you want an AI or not.\n");
  ANSITerminal.(
    print_string [green]
      "If you choose an AI, you are player 1 and the AI is player 2.\n");
  ANSITerminal.(print_string [green]
                  "Please Enter yes, no, or quit.\n")

(** [print_ai_failure_message ()] has the effect of telling the player
    he made a mistake in asking for an AI.  *)
let print_ai_failure_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease enter yes, no or quit.\n")

(** [print_difficulty_message ()] has the effect of asking the player
    what AI difficulty is wanted.   *)
let print_difficulty_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease choose whether the difficulty of the AI.\n");
  ANSITerminal.(print_string [green]
                  "You can choose easy, medium, hard or insane.\n")

(** [print_difficulty_error ()] has the effect of telling the player
    he made a mistake in asking for an AI difficulty.  *) 
let print_difficulty_error () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlease enter easy, medium, hard or insane.\n")

(* [check_ai] represents either [AIContinue] that an AI is wanted or not
   or [AIQuit] indicating the player typed quit in. *)
type check_ai = 
  | AIContinue of bool
  | AIQuit

(** [choose_ai ()] is [AIContinue true] if an AI is chosen, 
    [AIContinue false] if ai is not chosen and [Quit] if quit is chosen
    otherwise a failure message is displayed and the same function is called. *)
let rec choose_ai () =
  print_ai_message ();
  match () |> read_line |> parse with
  | YesNo true -> ANSITerminal.erase Screen; AIContinue true
  | YesNo false -> ANSITerminal.erase Screen; AIContinue false
  | Quit -> ANSITerminal.erase Screen; AIQuit
  | _ -> ANSITerminal.erase Screen; print_ai_failure_message (); choose_ai ()

(** [determine_ai_status state battleship ai_status diff ai] is 
    the game state with an AI or not, e.g. if an AI is wanted
    then [ContinueGame] with [ai_status] as [true] otherwise [false]
    and if the player wants to quit, then [EndGame]. *)
let determine_ai_status state battleship ai_status diff ai = 
  match choose_ai () with
  | AIContinue true -> 
    ANSITerminal.erase Screen; 
    ContinueGame (state, battleship, true, diff, ai)
  | AIContinue false -> 
    ANSITerminal.erase Screen; 
    ContinueGame (state, battleship, false, diff, ai)
  | AIQuit -> 
    ANSITerminal.erase Screen; 
    EndGame

(** [choose_difficulty ()] is the difficulty the player wants for the AI.*)
let rec choose_difficulty () = 
  print_difficulty_message ();
  match () |> read_line |> parse_difficulty with
  | Easy -> AIEasy
  | Medium -> AIMedium
  | Hard -> AIHard
  | Insane -> AIInsane
  | InvalidDifficulty -> 
    ANSITerminal.erase Screen;  print_difficulty_error (); choose_difficulty ()

(** [determine_ai_difficulty state battleship ai_status diff ai] is the game
    with the difficulty of Ai the player wants, e.g.
    [ContinueGame] with [diff] aa one of [AIEasy, AIHard, AIMedium or AIINSANE]
*)
let determine_ai_difficulty state battleship ai_status diff ai = 
  if not ai_status then 
    begin 
      ANSITerminal.erase Screen;
      ContinueGame (state, battleship, ai_status, diff, ai);
    end
  else begin
    match choose_difficulty () with
    | AIEasy -> ANSITerminal.erase Screen;
      ContinueGame (state, battleship, ai_status, AIEasy, ai)
    | AIMedium -> ANSITerminal.erase Screen;
      ContinueGame (state, battleship, ai_status, AIMedium, ai)
    | AIHard -> ANSITerminal.erase Screen;
      ContinueGame (state, battleship, ai_status, AIHard, ai)
    | AIInsane -> ANSITerminal.erase Screen;
      ContinueGame (state, battleship, ai_status, AIInsane, ai)
  end

(* ########### Process AI ############# *)

(* ########### Announce Player turn to lay down ships ############# *)

(** [print_player1_add_ships state battleship ai_status diff ai] has the 
    effect of asking player one to place ships and continuing the game. *)
let print_player1_add_ships state battleship ai_status diff ai = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease place your ships Player One. \n");
  ContinueGame (state, battleship, ai_status, diff, ai)

(** [print_player2_add_ships state battleship ai_status diff ai] has the 
    effect of asking player two to place ships and continuing the game
    unless if [ai_status] is [true] in which case
    the effect is that the player is told Ai will place its ships,
    and then the game contineus. . *)
let print_player2_add_ships state battleship ai_status diff ai = 
  if ai_status then 
    let () = ANSITerminal.(print_string [blue]
                             "\n\nThe AI will now lay down its ships. \n") in 
    ContinueGame (state, battleship, ai_status, diff, ai)
  else
    let () = ANSITerminal.(print_string [blue]
                             "\n\nPlease place your ships Player Two. \n") in 
    ContinueGame (state, battleship, ai_status, diff, ai)

(* ########### Announce Player turn to lay down ships ############# *)

(* ########### Delay ############# *)

(** [delay input] has the effect of clearing the screen if
    delay is typed in, an otherwise blocks. *)
let rec delay input =
  ANSITerminal.(print_string [green]
                  "Type clear to wipe the screen.\n");
  match String.trim (String.lowercase_ascii (read_line input)) with
  | "clear" -> ANSITerminal.erase Screen
  | _ -> delay input

(* ########### Delay ############# *)

(* ########### Pregame : add ships to board ############# *)

(** [print_lay_down_ships_phase color] has the effect
    of displaying the rules for laying down ships with [color] being
    which player's color used, e.g. red for player 1 and 
    blue for player2.  *)
let print_lay_down_ships_phase color = 
  ANSITerminal.(print_string [color]
                  "\n\nPlease lay down your ships on the map. \n");
  ANSITerminal.(print_string [color] 
                  ("\n\nSpecify the placement as (column, row, direction, ship"
                   ^ " type). 
The coordinates represent the head of the ship and the direction of the ship"
                   ^ " can be listed as left, right, up, and down.\n"))

(** [print_quit ()] has the effect
    printing a quit message.   *)
let print_quit () = 
  ANSITerminal.erase Screen; 
  ANSITerminal.(print_string [green]
                  "\n\nQuitting game.\n");
  ANSITerminal.scroll 20

(** [print_syntax_error ()] has the effect
    printing a syntax error message.   *)
let print_syntax_error () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour declaration had an error. Try again.\n")

(** [print_wrong_phase () ] has the effect
    printing a wrong phase message.   *)
let print_wrong_phase () = 
  ANSITerminal.(print_string [green]
                  "\n\nWe are the pregame phase. Try again.\n")

(** [print_yes_no_phrase () ] has the effect
    printing a yes no error essage.   *)
let print_yes_no_phrase () = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is not a yes/no question. Try again.\n")

(** [print_off_gameboard ()] has the effect
    printing a gameboard coordinate error message.   *)
let print_off_gameboard () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour coordinates were not on the grid. Try again.\n")

(** [print_occupied_location ()] has the effect
    printing a location occupied message.   *)
let print_occupied_location () = 
  ANSITerminal.(print_string [green]
                  "\n\nYour coordinates for the ship 
                  were occupied already. Try again.\n")

(** [print_placed_already ()] has the effect
    printing a ship placed already message.   *)
let print_placed_already () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou have placed all 
                  the ships of that type already. Try again.\n")

(** [print_remaining_ships player battleship] has the effect
    printing aout all the remaining shiops a player has not placed.   *)
let print_remaining_ships player battleship = 
  let color_player = 
    match player with 
    | true -> ANSITerminal.red 
    | false -> ANSITerminal.blue in
  let remaining_ships = 
    Battleship.(remaining_ships_to_place (choose_player player) battleship) in 
  ANSITerminal.(print_string [color_player]
                  "These are your remaining ships to place.\n");
  List.map print_endline remaining_ships |> ignore

(* [pregame_state] represents what action a palyer does during the pregame,
   including [PGQuit] or quitting, [PGRandom], or asking all the ships
   to be placed randomly, [PGFinishturn] to finish the placement and 
   [PGCOntinue] to continue placement *)
type pregame_state = 
  | PGQuit
  | PGRandom
  | PGFinishTurn of Battleship.t
  | PGContinue of Battleship.t

(** [place_player_ship battleship player x y direction ship ] places
    a ship for the [player] on the [battleship] dictionary at
    [(x, y)] with [direction] orientation of the ship and with type
    [ship] *)
let place_player_ship battleship player x y direction ship =
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

(** [parse_player_command battleship player str] determiens what action
    the player wants to do, including [Quit] if quit, [Target] if a target
    is specific, [InvalidCommand] for invalid commands, [YEsNo], 
    and placing a ship with [Random] or the coordinates are [Valid]. *)
let parse_player_command battleship player str = 
  match parse str with
  | Quit -> print_quit (); PGQuit
  | YesNo _ -> print_yes_no_phrase (); PGContinue battleship
  | InvalidCommand -> print_syntax_error (); PGContinue battleship
  | Target (_, _) -> print_wrong_phase (); PGContinue battleship
  | Remove ship -> 
    PGContinue (battleship |> remove_ship ship (choose_player player))
  | FinishPlacement ->
    if Battleship.(remaining_ships (choose_player true) battleship) = 0 
    then PGFinishTurn (battleship)
    else PGContinue (battleship)
  | Valid (x, y, direction, ship) -> 
    place_player_ship battleship player x y direction ship
  | Random -> PGRandom
  | Use (_,_,_) -> print_wrong_phase (); PGContinue battleship
  | Powerups -> print_wrong_phase (); PGContinue battleship

(** [ ai_to_string ((x, y), direction, ship)] is a string corresponding
    to an AI command to target [(x, y)] with orientation
    [direction] and type [ship]. *)
let ai_to_string ((x, y), direction, ship) = 
  string_of_int (x) ^ " " ^ string_of_int (y) ^ " " ^ direction ^ " " ^ ship 

(** [ place_single_ai_ship battleship player str] places a single ship
    for the AI based lon the Ai's targeting algorithm  determing
    a location that is equivalent to [str].

    Raises: ["Violate precondition place_single_ai_ship,place_ai_player2_ships"]
    if the AI does not give a Valid command to place.  *)
let rec place_single_ai_ship battleship player str = 
  match parse str with
  | Quit | InvalidCommand | YesNo _ | Target _| Remove _ | FinishPlacement 
  | Random | Use (_,_,_) | Powerups -> failwith ("Violates preconditions")
  | Valid (x, y, direction, ship) -> 
    let direction' = string_to_direction direction in 
    let ship' = string_to_ship ship in 
    let player' = choose_player player in 
    begin match insert_ship (x, y) direction' ship' player' battleship with
      | Battleship.Success battleship' -> 
        PGContinue battleship'
      | Battleship.Failure (battleship', BoundsError) ->
        PGContinue battleship'
      | Battleship.Failure (battleship', OccupiedTile) ->
        PGContinue battleship'
      | Battleship.Failure (battleship', OutOfShips) ->
        PGContinue battleship'
      | Battleship.Failure (_, NonexistShip) ->
        failwith "Remove Unimplemented" 
    end

(** [place_player_ships_randomly state battleship ai_status diff ai player] 
    places a single ship for the player until all its ships have been placed.

    Raises: ["illegal for AI"]
    if the AI  gives a quit, finishturn or random command.  *)
let rec place_player_ships_randomly state battleship ai_status diff ai player = 
  if Battleship.(remaining_ships (choose_player player) battleship) = 0 
  then begin
    if not ai_status then delay (); ANSITerminal.erase Screen; delay ();
    ContinueGame (state, battleship, ai_status, diff, ai) end
  (* place_player_ships state battleship ai_status diff ai player *)
  (* ContinueGame (state, battleship, ai_status, diff, ai) *)
  else 
    let ai_result = 
      battleship 
      |> random_ship (choose_player player)
      |> ai_to_string 
      |> place_single_ai_ship battleship player in begin
      match ai_result with
      | PGQuit -> (* impossible for AI to do *)
        failwith "illegal for AI"
      | PGContinue battleship' -> 
        place_player_ships_randomly state battleship' ai_status diff ai player
      | PGFinishTurn battleship' -> (* impossible for AI to do *)
        failwith "illegal for AI"
      | PGRandom -> (* impossible for AI to do *)
        failwith "illegal for AI"
    end

(** [place_ai_player2_ships state battleship ai_status diff ai player] 
    places a single ship for the AI until all its ships have been placed.

    Raises: ["illegal for AI"]
    if the AI  gives a quit, finishturn or random command.  *)
let rec place_ai_player2_ships state battleship ai_status diff ai player =
  place_player_ships_randomly state battleship ai_status diff ai player

(** [place_player_ships state battleship ai_status diff ai player] 
    allows the [player] to place ships on the [battleship]
    data structure, and also has the effect of advancing game state. 

    The [player] can choose to [PGQuit] or end the game, 
    [PGCOntinue] to lay down ships and remove them
    or [PGRandom] to randomly lay down ships or if their ships are 
    all polaced [PGFinish] turn to end. 

    Requires:if [player] is [false] the [player] is human.  *)
let rec place_player_ships state battleship ai_status diff ai player = 
  let () = print_remaining_ships player battleship in 
  let color = if player then ANSITerminal.red else ANSITerminal.blue in 
  ANSITerminal.(print_string [color] "type finish to end turn");
  print_lay_down_ships_phase color;
  let result_of_adding_ship = 
    () |> read_line |> parse_player_command battleship player in
  match result_of_adding_ship with
  | PGQuit -> EndGame
  | PGContinue battleship' -> 
    place_player_ships state battleship' ai_status diff ai player
  | PGFinishTurn battleship' ->
    (if player then
       ANSITerminal.(print_string [green] "\nPass computer to player 2.\n")
     else 
       ANSITerminal.(print_string [green] "\nPass computer to player 1.\n"));
    if not ai_status then delay (); ANSITerminal.erase Screen; delay ();
    ContinueGame (state, battleship', ai_status, diff, ai)
  | PGRandom ->
    place_player_ships_randomly state battleship ai_status diff ai player

(** [place_player1_ships state battleship ai_status diff ai] 
    allows the [player1] to place ships on the [battleship]
    data structure, and also has the effect of advancing game state. 

    The [player] can choose to [PGQuit] or end the game, 
    [PGCOntinue] to lay down ships and remove them
    or [PGRandom] to randomly lay down ships or if their ships are 
    all polaced [PGFinish] turn to end. *)
let rec place_player1_ships state battleship ai_status diff ai = 
  place_player_ships state battleship ai_status diff ai true

(** [place_player2_ships state battleship ai_status diff ai] 
    allows the [player2] to place ships on the [battleship]
    data structure, and also has the effect of advancing game state. 

    The [player] can choose to [PGQuit] or end the game, 
    [PGCOntinue] to lay down ships and remove them
    or [PGRandom] to randomly lay down ships or if their ships are 
    all polaced [PGFinish] turn to end. 

    Requires: Player2 is a human. *)
let rec place_human_player2_ships state battleship ai_status diff ai = 
  place_player_ships state battleship ai_status diff ai false 


(** [place_player2_ships state battleship ai_status diff ai] 
    allows the [player2] to place ships on the [battleship]
    data structure, and also has the effect of advancing game state. 

    The [player] can choose to [PGQuit] or end the game, 
    [PGCOntinue] to lay down ships and remove them
    or [PGRandom] to randomly lay down ships or if their ships are 
    all polaced [PGFinish] turn to end.  *)
let rec place_player2_ships state battleship ai_status diff ai = 
  if ai_status 
  then place_ai_player2_ships state battleship ai_status diff ai false
  else place_human_player2_ships state battleship ai_status diff ai

(* ########### Pregame : add ships to board ############# *)

(* ########### Entering Trageting Phase ############# *)

(** [print_entering_targeting_phase state battleship ai_status diff ai] has
    the effect of telling the player they are entering the targeting phase.  *)
let print_entering_targeting_phase state battleship ai_status diff ai = 
  ANSITerminal.erase Screen; 
  ANSITerminal.(print_string [green]
                  "We are entering the targeting phase of the game.\n");
  ANSITerminal.(print_string [green]
                  "Player 1 will target first and alternate with player 2.\n");
  ANSITerminal.(print_string [green]
                  "The game continues until victory or a player quits. \n\n");   
  Unix.sleep 1;           
  ContinueGame (state, battleship, ai_status, diff, ai)

(** [build_in_game_state state battleship ai_status diff ai] creates
    the in game state based on the [battleship] data from
    laying down ships. *)
let build_in_game_state state battleship ai_status diff ai = 
  if ai_status then 
    let in_game_state = 
      initialize_ai true false 
        (get_player_dict (choose_player true) battleship) 
        (get_player_dict (choose_player false) battleship) in 
    ContinueGame (Some in_game_state, battleship, ai_status, diff, ai)
  else
    let in_game_state = 
      init_state true false 
        (get_player_dict (choose_player true) battleship) 
        (get_player_dict (choose_player false) battleship) in 
    ContinueGame (Some in_game_state, battleship, ai_status, diff, ai)

(* ########### Entering Trageting Phase ############# *)

(* ########### Powerups ############# *)

(** [update_player1_powerups state battleship ai_status diff ai] 
    has the effect of assigning powerups to the game for player 1. *)
let update_player1_powerups state battleship ai_status diff ai = 
  let battleship' = assign_powerups (choose_player true) battleship in 
  ContinueGame (state, battleship', ai_status, diff, ai)

(** [update_player2_powerups state battleship ai_status diff ai] 
    has the effect of assigning powerups to the game for player 2. *)
let update_player2_powerups state battleship ai_status diff ai = 
  let battleship' = assign_powerups (choose_player false) battleship in 
  ContinueGame (state, battleship', ai_status, diff, ai)

(** [print_player1_powerups state battleship ai_status diff ai] 
    has the effect of printing powerups to the terminal for player 1. *)
let print_player1_powerups state battleship ai_status diff ai = 
  print_power_ups (choose_player true) battleship;
  ContinueGame (state, battleship, ai_status, diff, ai)

(** [print_player2_powerups state battleship ai_status diff ai] 
    has the effect of printing powerups to the terminal for player 2. *)
let print_player2_powerups state battleship ai_status diff ai = 
  print_power_ups (choose_player false) battleship;
  ContinueGame (state, battleship, ai_status, diff, ai)

(* ########### Powerups ############# *)

(* ########### In Game ############# *)

(** [print_player_move_message player] prints
    which player is to move.  *)
let print_player_move_message player = 
  if player then ANSITerminal.(print_string [red]
                                 "\n\nPlayer One, it is your turn to move\n")
  else ANSITerminal.(print_string [blue]
                       "\n\nPlayer Two, it is your turn to move\n")

(** [print_opponent_grid player state ] prints
    the opponent's grid.  *)
let print_opponent_grid player state = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your opponent grid.\n");
  State.print_guesses (State.bool_to_player player) state;
  print_newline ()

(** [print_player_grid player state] prints
    the player's grid.  *)
let print_player_grid player state = 
  ANSITerminal.(print_string [green]
                  "\n\nThis is your grid.\n");      
  State.print_player_dict (State.bool_to_player player) state;
  print_newline ()

(** [print_boards_side_by_side player state] prints
    the player's grid along with opponent's grid side by side.  *)
let print_boards_side_by_side player state = 
  ANSITerminal.(print_string [green]
                  "\n\nYour grid is the left and opponent is the right.\n\n");
  let player_dict = 
    State.string_of_player_dict (State.bool_to_player player) state in 
  let player_guesses = 
    State.string_of_guesses (State.bool_to_player player) state in 
  let combined = State.combine_boards player_dict player_guesses in 
  State.print_boards combined

(** [print_targeting_rules color ] is the rules for targeting
    with the color corespondign to red as player1 and blue as player2.  *)
let print_targeting_rules color = 
  ANSITerminal.(print_string [color]
                  "\n\nPlease target a location on the enemy map. \n");
  ANSITerminal.(print_string [color]
                  "\n\nSpecify the placement as a column by typing in
    target, a comma, the x coordinate, comma,
    and the y coordinate. \n")

(** [ print_in_main_phase ()] is a message for errror you are in main phase. *)
let print_in_main_phase () = 
  ANSITerminal.(print_string [green]
                  "\nWe are no longer in the pregame phase. Try again.\n")

(** [print_already_targeted ()] is a message saying
    you targeted a location you previously targeted. *)
let print_already_targeted () = 
  ANSITerminal.(print_string [green]
                  "\nYou cannot target a location you previously targeted.\n")

(** [hit_ship_message ()] is ayou hit a ship. *)
let hit_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\nYou hit a ship.\n")

(** [miss_ship_message ()] is ayou miss a ship. *)
let miss_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\nYou did not hit a ship.\n\n")

(** [sink_ship_message ()] is ayou sunk a ship. *)
let sink_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nYou sunk a ship.\n")   

(** [not_sink_ship_message ()] is did not sink a ship. *)
let not_sink_ship_message () = 
  ANSITerminal.(print_string [green]
                  "\nYou did not sink a ship.\n\n")

(** [after_move_message ()] is the message announcing
    the new board of after your move will be displayed *)
let after_move_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nThese are the grids after you made your move.\n")

(** [print_sunk_hit_message ship_hit ship_sunk] prints whehter [ship_hit]
    and [ship_sunk]. *)
let print_sunk_hit_message ship_hit ship_sunk = 
  (if ship_hit 
   then hit_ship_message ()
   else miss_ship_message ()) |> ignore;
  (if ship_sunk 
   then sink_ship_message ()
   else not_sink_ship_message ()) |> ignore

(** [print_player1_wins ()] announcs player 1 won. *)
let print_player1_wins () = 
  ANSITerminal.(print_string [green]
                  "\n\nPlayer One, you win!.\n")

(** [print_player2_wins ai_stauts] announcs player 2 won
    including if it is an ai based on [ai_status]. *)
let print_player2_wins ai_status = 
  if ai_status then ANSITerminal.(print_string [green]
                                    "\n\nThe AI wins!.\n")
  else ANSITerminal.(print_string [green]
                       "\n\nPlayer Two, you win!.\n")

(** [print_winner player ai_status] announcs the winner. *)
let print_winner player ai_status = 
  (if player then print_player1_wins ()
   else print_player2_wins ai_status) |> ignore

(** [get_state_from state_option] gets the state from [state_option]. 

    Raises : ["No state was given. "] if [state_option] is [None] *)
let get_state_from state_option = 
  match state_option with
  | Some state -> state
  | None -> failwith "No state was given. "

(** [choose_color player ] is red if [player] is [true]
    blue otherwise. *)
let choose_color player = 
  if player then ANSITerminal.red 
  else ANSITerminal.blue


(** [get_difficulty_targeting_func diff ] is the correct targeting
    function based on ai difficulty [diff]. *)
let get_difficulty_targeting_func diff = 
  match diff with
  | AIEasy -> target_ai
  | AIMedium -> target_medium_ai
  | AIHard -> target_hard_ai
  | AIInsane -> target_insane_ai

(** [resolve_state_change 
    player rec_func x y player state battleship 
    ai_status diff ai hit sunk string_opp_player]
    is the new game state after the player
    change is updated for humans and AIs. *)
let resolve_state_change 
    player rec_func x y state battleship 
    ai_status diff ai hit sunk string_opp_player = 
  if (not player) && (ai_status) && (diff = AIEasy || diff = AIMedium) then
    rec_func 
      (Some (state |> update_player)) 
      battleship ai_status diff ai
  else if player = false && diff = AIHard then 
    (* let () = delay () in 
       ANSITerminal.(print_string [green] 
                    ("\nPass the computer to " ^ string_opp_player ^ " .\n\n"));  *)
    rec_func 
      (Some (update_hard_ai state hit sunk (x + 1, y + 1) |> update_player)) 
      battleship ai_status diff ai
  else if 
    player = false && diff = AIInsane then 
    (* let () = delay () in 
       ANSITerminal.(print_string [green] 
                      ("\nPass the computer to " ^ string_opp_player ^ " .\n\n"));  *)
    rec_func 
      (Some (update_insane_ai state hit sunk (x + 1, y + 1) |> update_player))
      battleship ai_status diff ai
  else 
    let () = delay () in 
    ANSITerminal.(print_string [green] 
                    ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
    let () = delay () in
    rec_func (Some (state |> update_player)) battleship ai_status diff ai

(** [handle_succ 
    rec_func x y player battleship state' 
    ship_hit ship_sunk ai_status diff ai string_opp_player] is the game
    updated after a successful target is made by humans or the Ai. *)
let handle_succ 
    rec_func x y player battleship state' 
    ship_hit ship_sunk ai_status diff ai string_opp_player =
  let new_state = 
    (if check_coord_in_powerups x y (choose_player player) battleship 
     then 
       let 
         power_name = get_powerup_name x y (choose_player player) battleship in 
       State.add_powerup (bool_to_player player) state' power_name
     else state') in
  if player = true || (player = false && ai_status = false) then 
    (after_move_message ();
     print_boards_side_by_side player new_state;
     print_sunk_hit_message ship_hit ship_sunk);
  if check_victory (bool_to_player player) new_state 
  then let () = print_winner player ai_status in VictoryGame
  else resolve_state_change player rec_func x y new_state battleship
      ai_status diff ai ship_hit ship_sunk string_opp_player

(** [legal_target rec_func x y player state battleship ai_status diff ai]
    is the new game state after a legal targeting location ahs been made.  *)
let legal_target rec_func x y player state battleship ai_status diff ai = 
  let string_opp_player =
    match player with
    | true -> "Player 2"
    | false -> "Player 1"
  in match target_ship (x, y) (bool_to_player player) state with
  | State.Failure (new_state, CoordinateVisited) ->
    print_already_targeted (); 
    rec_func (Some new_state) battleship ai_status diff ai
  | State.Failure (new_state, OutOfBounds) ->
    print_off_gameboard (); 
    rec_func (Some new_state) battleship ai_status diff ai
  | State.Success (state', ship_hit, ship_sunk) ->
    handle_succ rec_func x y player battleship state' ship_hit ship_sunk
      ai_status diff ai string_opp_player

(*
  let new_state = 
      (if check_coord_in_powerups x y (choose_player player) battleship 
       then 
         let power_name = get_powerup_name x y (choose_player player) battleship in 
         State.add_powerup (bool_to_player player) state' power_name
       else state') in
    after_move_message ();
    print_boards_side_by_side player new_state;
    print_sunk_hit_message ship_hit ship_sunk;
    if check_victory (bool_to_player player) new_state 
    then let () = print_winner player ai_status in VictoryGame
    else 
    if player = false && diff = AIHard then begin
      delay ();
      ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
      rec_func (Some (update_hard_ai new_state ship_hit ship_sunk (x + 1, y + 1) |> update_player)) battleship ai_status diff ai
    end
    else if player = false && diff = AIInsane then begin
      delay ();
      ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
      rec_func (Some (update_insane_ai new_state ship_hit ship_sunk (x + 1, y + 1) |> update_player)) battleship ai_status diff ai
    end
    else
      begin
        delay ();
        ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
        rec_func (Some (new_state |> update_player)) battleship ai_status diff ai
      end
  *)

(** [print_square_hit_cannot_use ()] is the effect of square hit not
    being able to used. *)
let print_square_hit_cannot_use () = 
  ANSITerminal.(print_string [green] "Square hit could be used.");
  ANSITerminal.
    (print_string [green] "All corners of the square must be on the board.");
  ANSITerminal.
    (print_string [green] "No corner of the square can be targeted already.");
  print_newline ()

(** [print_rehit_cannot_use ()] is the effect of re hit not
    being able to used. *)
let print_rehit_cannot_use () = 
  ANSITerminal.(print_string [green] "Rehit could be used.");
  ANSITerminal.
    (print_string [green] "Re hit must lie on board.");
  ANSITerminal.
    (print_string [green] "Re hit cannot target previous location.");
  print_newline ()

(** [print_ikill_cannot_use ()] is the effect of instakill not
    being able to used. *)
let print_ikill_cannot_use () = 
  ANSITerminal.(print_string [green] "Instakill could be used.");
  ANSITerminal.
    (print_string [green] "Instakill must lie on board.");
  ANSITerminal.
    (print_string [green] "Instakill cannot target previous location.");
  print_newline ()

(** [target state_option battleship ai_status diff ai]
    allows the player to make a target on the opponent board.  *)
let rec target state_option battleship ai_status diff ai = 
  let state = get_state_from state_option in 
  let player = get_current_player state in
  let color = choose_color player in 
  if player = true || (player = false && ai_status = false) then
    (print_player_move_message player;
     print_boards_side_by_side player state;
     print_targeting_rules color);
  if player = false && ai_status then 
    let targeting_func = get_difficulty_targeting_func diff in 
    let (x, y), new_state = targeting_func state in 
    legal_target 
      target (x - 1) (y - 1) player new_state battleship ai_status diff ai
  else 
    handle_target_result state battleship ai_status diff ai player

(** [handle_squarehit_use 
    hit_result player state battleship ai_status diff ai string_opp_player] 
    is the resulf or using a squarehit successfully for [player] 
    in [state]. *)
and handle_squarehit_use 
    hit_result player state battleship ai_status diff ai string_opp_player = 
  match hit_result with 
  | Usable (new_state, hit, sunk) ->
    after_move_message ();
    print_boards_side_by_side player new_state;
    print_sunk_hit_message hit sunk;
    if check_victory (bool_to_player player) new_state 
    then let () = print_winner player ai_status in VictoryGame
    else
      let () = delay () in
      ANSITerminal.
        (print_string [green] 
           ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
      target (Some (new_state |> update_player)) battleship ai_status diff ai (* square hit used succesfully so player ends turn) *)
  | Unusable ->
    print_square_hit_cannot_use ();
    target (Some state) battleship ai_status diff ai

(** [and handle_squarehit 
    pow x y player state battleship ai_status diff ai string_opp_player]
    is the result of using an instakill [pow] for
    [player] in [state].  *)
and handle_squarehit 
    pow x y player state battleship ai_status diff ai string_opp_player = 
  if List.mem pow (get_player_powerups (bool_to_player player) state) (*&& square_check_bounds (x,y) *)
  then let () = ANSITerminal.(print_string [green] 
                                "\nUsing SQUAREHIT! Boom! \n") in
    let squarehit_result = 
      (update_powerup_state x y (bool_to_player player) state "squarehit") in 
    handle_squarehit_use 
      squarehit_result player 
      state battleship ai_status diff ai string_opp_player
  else target (Some state) battleship ai_status diff ai (* No power up used  because does notnhave square hit*)

(** [handle_rehit_use 
    hit_result player state battleship ai_status diff ai string_opp_player] 
    is the resulf or using a squarehit successfully for [player] 
    in [state]. *)
and handle_rehit_use 
    hit_result player state battleship ai_status diff ai string_opp_player = 
  match hit_result with 
  | Usable (new_state, hit, sunk) ->
    after_move_message ();
    print_boards_side_by_side player new_state;
    print_sunk_hit_message hit sunk;
    if check_victory (bool_to_player player) new_state 
    then let () = print_winner player ai_status in VictoryGame
    else 
      target (Some (new_state)) battleship ai_status diff ai (* rehit used succesfully, so player gets a secomnd attack ) *)
  | Unusable ->
    print_rehit_cannot_use ();
    target (Some state) battleship ai_status diff ai

(** [handle_rehit
    pow x y player state battleship ai_status diff ai string_opp_player]
    is the result of using an instakill [pow] for
    [player] in [state].  *)
and handle_rehit 
    pow x y player state battleship ai_status diff ai string_opp_player = 
  if List.mem pow (get_player_powerups (bool_to_player player) state) 
  then 
    let () = ANSITerminal.(print_string [green]
                             "\nUsing REHIT! Boom! \n") in 
    let rehit_result = 
      (update_powerup_state x y (bool_to_player player) state "rehit") in 
    handle_rehit_use 
      rehit_result player state battleship ai_status diff ai string_opp_player
  else target (Some state) battleship ai_status diff ai (* No power up used because does not have rehit*)

(** [handle_instakill_use 
    hit_result player state battleship ai_status diff ai string_opp_player] 
    is the resulf or using a squarehit successfully for [player] 
    in [state]. *)
and handle_instakill_use 
    hit_result player state battleship ai_status diff ai string_opp_player = 
  match hit_result with 
  | Usable (new_state, hit, sunk) ->
    after_move_message ();
    print_boards_side_by_side player new_state;
    print_sunk_hit_message hit sunk;
    if check_victory (bool_to_player player) new_state 
    then let () = print_winner player ai_status in VictoryGame
    else
      let () = delay () in
      ANSITerminal.
        (print_string [green] 
           ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
      target (Some (new_state |> update_player)) battleship ai_status diff ai (* instalkill used succesfully and so new player) *)
  | Unusable ->
    print_ikill_cannot_use ();
    target (Some state) battleship ai_status diff ai

(** [handle_instakill
    pow x y player state battleship ai_status diff ai string_opp_player]
    is the result of using an instakill [pow] for
    [player] in [state].  *)
and handle_instakill
    pow x y player state battleship ai_status diff ai string_opp_player =
  if List.mem pow (get_player_powerups (bool_to_player player) state) 
  then 
    let () = ANSITerminal.(print_string [green]
                             "\nUsing Instakill! Boom! \n") in 
    let ikill_result = 
      (update_powerup_state x y (bool_to_player player) state "instakill") in 
    handle_instakill_use 
      ikill_result player state battleship ai_status diff ai string_opp_player
  else target (Some state) battleship ai_status diff ai

(** [use_powerups pow x y player state battleship ai_status diff ai] 
    is the application of using powerups for [player] 
    at [x] [y] with power [pow] in [state]. 

    Raises: ["not a valid power-up :("] if the powerup is not one
    of ["rehit", "instakill" or "squarehit"]. *)
and use_powerups pow x y player state battleship ai_status diff ai =
  let string_opp_player =
    match player with
    | true -> "Player 2"
    | false -> "Player 1" in
  match pow with 
  |"squarehit" -> 
    handle_squarehit
      pow x y player state battleship ai_status diff ai string_opp_player 
  | "rehit" -> 
    handle_rehit 
      pow x y player state battleship ai_status diff ai string_opp_player
  | "instakill" -> 
    handle_instakill 
      pow x y player state battleship ai_status diff ai string_opp_player
  | _ -> failwith "not a valid power-up :("
(* if List.mem pow (get_player_powerups (bool_to_player player) state) (*&& square_check_bounds (x,y) *)
   then 
   let () = ANSITerminal.(print_string [green]
                           "\nUsing SQUAREHIT! Boom! \n") in
   let squarehit_result = 
    (update_powerup_state x y (bool_to_player player) state "squarehit") in begin
    match squarehit_result with 
    | Usable (new_state, hit, sunk) ->
      after_move_message ();
      print_boards_side_by_side player new_state;
      print_sunk_hit_message hit sunk;
      if check_victory (bool_to_player player) new_state 
      then let () = print_winner player ai_status in VictoryGame
      else
        let () = delay () in
        ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
        target 
          (Some (new_state |> update_player)) battleship ai_status diff ai (* square hit used succesfully so player ends turn) *)
    | Unusable ->
      print_square_hit_cannot_use ();
      target (Some state ) battleship ai_status diff ai(* No power up used because squarehit conditions violated*)
   end
   else target (Some state ) battleship ai_status diff ai (* No power up used  because does notnhave square hit*)
   end *)
(* begin
    if List.mem pow (get_player_powerups (bool_to_player player) state) 
    then 
      let () = ANSITerminal.(print_string [green]
                               "\nUsing REHIT! Boom! \n") in 
      let rehit_result = 
        (update_powerup_state x y (bool_to_player player) state "rehit") in begin
        match rehit_result with 
        | Usable (new_state, hit, sunk) ->
          after_move_message ();
          print_boards_side_by_side player new_state;
          print_sunk_hit_message hit sunk;
          if check_victory (bool_to_player player) new_state 
          then let () = print_winner player ai_status in VictoryGame
          else
            let () = delay () in
            ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
            target 
              (Some (new_state)) battleship ai_status diff ai (* rehit used succesfully, so player gets a secomnd attack ) *)
        | Unusable ->
          print_rehit_cannot_use ();
          target (Some state) battleship ai_status diff ai(* No power up used because rehit conditions violated*)
      end
    else target (Some state) battleship ai_status diff ai (* No power up used because does not have rehit*)
   end *)
(*if List.mem pow (get_player_powerups (bool_to_player player) state) 
  then 
  let () = ANSITerminal.(print_string [green]
                           "\nUsing Instakill! Boom! \n") in 
  let ikill_result = 
    (update_powerup_state x y (bool_to_player player) state "instakill") in begin
    match ikill_result with 
    | Usable (new_state, hit, sunk) ->
      after_move_message ();
      print_boards_side_by_side player new_state;
      print_sunk_hit_message hit sunk;
      if check_victory (bool_to_player player) new_state 
      then let () = print_winner player ai_status in VictoryGame
      else
        let () = delay () in
        ANSITerminal.(print_string [green] ("\nPass the computer to " ^ string_opp_player ^ " .\n\n")); 
        target 
          (Some (new_state |> update_player)) battleship ai_status diff ai (* instalkill used succesfully and so new player) *)
    | Unusable ->
      print_ikill_cannot_use ();
      target (Some state) battleship ai_status diff ai(* No power up used because rehit conditions violated*)
  end (*add instakill*)
  else target (Some state) battleship ai_status diff ai *)(*does not have instakill*)

(** [handle_target_result state battleship ai_status diff ai player]
    deals with a player command to target.  *)
and handle_target_result state battleship ai_status diff ai player = 
  match () |> read_line |> parse with
  | YesNo _ -> print_yes_no_phrase ();
    target (Some state) battleship ai_status diff ai
  | Quit -> print_quit (); EndGame
  | InvalidCommand -> 
    print_syntax_error (); target (Some state) battleship ai_status diff ai
  | Valid (_, _, _, _) -> print_in_main_phase (); 
    target (Some state) battleship ai_status diff ai
  | Target (x, y) -> 
    legal_target target x y player state battleship ai_status diff ai
  | Remove _ -> print_in_main_phase (); 
    target (Some state) battleship ai_status diff ai
  | FinishPlacement -> print_in_main_phase (); 
    target (Some state) battleship ai_status diff ai
  | Random -> print_in_main_phase (); 
    target (Some state) battleship ai_status diff ai
  | Use (x,y,pow) ->
    use_powerups pow x y player state battleship ai_status diff ai
  | Powerups -> print_powerups (bool_to_player player) state; 
    target (Some state) battleship ai_status diff ai

(* ########### In Game ############# *)


(* ########### End Game ############# *)

(** [print_ending_message ())] is the quitting message. *)
let print_ending_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nLeaving Terminal.\n")

(** [finish_game ())] is the winning message. *)
let print_victory_message () = 
  ANSITerminal.(print_string [green]
                  "\n\nGood game. Game over.\n")

(** [finish_game game_status] is the ending message based on
    [game_status] *)
let finish_game game_status = 
  match game_status with
  | ContinueGame _ -> failwith "Illegal: Battleship games always terminate. "
  | EndGame -> print_ending_message ();
  | VictoryGame -> print_victory_message ()

(* ########### End Game ############# *)

(* ########### Running Game ############# *)

let () = 
  ()
  |> (fun () -> ANSITerminal.resize 125 40)
  |> (fun () -> ANSITerminal.erase Screen)
  |> print_welcome_message 
  |> initialize_main 
  |> (>>>) determine_ai_status
  |> (>>>) determine_ai_difficulty
  |> (>>>) print_player1_add_ships
  |> (>>>) place_player1_ships 
  |> (>>>) update_player1_powerups 
  (* |> (>>>) print_player1_powerups *)
  |> (>>>) print_player2_add_ships
  |> (>>>) place_player2_ships
  |> (>>>) update_player2_powerups
  (* |> (>>>) print_player2_powerups *)
  |> (>>>) print_entering_targeting_phase 
  |> (>>>) build_in_game_state
  |> (>>>) target
  |> finish_game

(* ########### Running Game ############# *)

