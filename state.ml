(* constants relevant to board size *)
let c_ROWS = 10
let c_COLS = 10

(* constant relevant to displaying board for player to view *)
let c_BOARD_SEP = "    "

type player = 
  | Player1
  | Player2

let bool_to_player b = 
  if b then Player1 else Player2

type grid_guess = 
  | Miss
  | Hit

type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

type powerup_type =
  | SquareHit
  | ReHit
  | InstaKill

open Hard_ai

(* [t] is represents the state of the game 
   [current_player]  is the player to make a move this turn
   [next_player] is the player who will move next, but does not move this turn
   [player_1_grid_guesses] are the locations where [player1] has targeted, 
   tagged with a hit or miss.
   [player_1_ship_dict] are the locations whre [player1] placed his ships.
   [player_2_grid_guesses] are the locations where [player2] has targeted, 
   tagged with a hit or miss.
   [player_2_ship_dict] are the locations whre [player2] placed his ships.
   [pos_targeted] are the enemy locations the easy AI has targeted already.
   [pos_remaining] are the enemy board locations where the easy AI
   has not yet targeted. 
   [surrounding_positions] is the locations that the medium Ai can target
   that surround a targeted position. 
   [guess_phase] is the phase of targeting: guess or smart. 
   [hard_ai] is the AI data for hard and insane AI. *)
type t = {
  current_player : bool;
  next_player : bool;
  player_1_grid_guesses : (int * int * grid_guess) list;
  player_1_ship_dict : Battleship.list_t;
  (*ships_player_1_sunk : string list; *)
  player_2_grid_guesses : (int * int * grid_guess) list;
  player_2_ship_dict : Battleship.list_t;
  (* ships_player_2_sunk : string list;*)
  pos_targeted : (int * int) list;
  pos_remaining : (int * int) list;
  surrounding_positions : (int * int) list;
  guess_phase : bool;
  hard_ai : Hard_ai.t;


  player_1_inv : string list;
  player_2_inv : string list;

}

type powerup_status = 
  | Usable of t * bool * bool
  | Unusable 

let init_state player_1 player_2 player_1_pregame player_2_pregame = {
  current_player = player_1;
  next_player = player_2;
  player_1_grid_guesses = [];
  player_1_ship_dict = player_1_pregame;
  player_2_grid_guesses = [];
  player_2_ship_dict = player_2_pregame;
  pos_targeted = [];
  pos_remaining = [];
  surrounding_positions = [];
  guess_phase = true;
  hard_ai = initialize_hard_ai;

  player_1_inv = [];
  player_2_inv = [];
}

let update_player state = 
  {state with 
   current_player = state.next_player;
   next_player = state.current_player;}

let get_current_player state = 
  state.current_player

(** [get_next_player st] is the next player to move, while on this turn, 
    with [true] for player1, [false] for player2. *)
let get_next_player state = 
  state.next_player

(** [check_coordinate_guessed (x, y) player state] is [true] iff [(x, y)]
    was a location that [player] has already targeted.  *)
let check_coordinate_guessed (x, y) player state = 
  match player with
  | Player1 -> 
    state.player_1_grid_guesses 
    |> List.map (fun (x, y, guess) -> (x, y)) 
    |> List.mem (x, y)
  | Player2 -> 
    state.player_2_grid_guesses 
    |> List.map (fun (x, y, guess) -> (x, y)) 
    |> List.mem (x, y)

(** [check_bounds (x, y)] is [true] iff both [x] and [y]
    are between 0 and 9 inclusive. *)
let check_bounds (x, y)  = 
  x >= 0 && x < 10 && y >= 0 && y < 10

(** [check_coordinate (x, y) player state] is [true] iff both [x] and [y]
    are between 0 and 9 inclusive AND [(x, y)] was not a location
    that the [player] had previously targeted. *)
let check_coordinate (x, y) player state = 
  check_bounds (x, y) && not (check_coordinate_guessed (x, y) player state)

(** [target (x, y) player state] is the updated [state] with 
    the location [(x, y)] with the damage from of the hit recorded on
    the opposing player's ship idctionary, and with the player's grid guesses
    updated with the coordinate targeted and whether it was a hit or a miss. *)
let target (x, y) player state = 
  match player with
  | Player1 ->
    if Battleship.check_cell_occupied (x, y) state.player_2_ship_dict 
    then {state with 
          player_2_ship_dict = 
            Battleship.change_to_damage (x, y) state.player_2_ship_dict;
          player_1_grid_guesses = (x, y, Hit) :: state.player_1_grid_guesses}
    else {state with 
          player_1_grid_guesses = (x, y, Miss) :: state.player_1_grid_guesses}
  | Player2 ->
    if Battleship.check_cell_occupied (x, y) state.player_1_ship_dict
    then {state with 
          player_1_ship_dict = 
            Battleship.change_to_damage (x, y) state.player_1_ship_dict;
          player_2_grid_guesses = (x, y, Hit) :: state.player_2_grid_guesses}
    else {state with 
          player_2_grid_guesses = (x, y, Miss) :: state.player_2_grid_guesses}

(** [get_hit_status (x, y) dict] is [true] if the location
    in the [dict] of guesses was Hit previously, otherwise if there was a miss,
    then [false].

    Raises: ["(x, y) not a location you targeted"] if [(x, y)] was a location
    that was not previously targeted by the player.  *)
let rec get_hit_status (x, y) dict = 
  match dict with
  | [] -> failwith "(x, y) not a location you targeted"
  | (x', y', status) :: t -> 
    if x = x' && y = y' then status = Hit
    else get_hit_status (x, y) t

(** [get_player_guess (x, y) player state] is [true] if the location
    the player guesses was Hit previously, otherwise if there was a miss,
    then [false].

    Raises: ["(x, y) not a location you targeted"] if [(x, y)] was a location
    that was not previously targeted by the player.  *)
let get_player_guess (x, y) player state = 
  match player with
  | Player1 -> 
    get_hit_status (x, y) state.player_1_grid_guesses
  | Player2 ->
    get_hit_status (x, y) state.player_2_grid_guesses


(** [check_ship_sunk (x, y) player state] is [true] iff the location
    the [player] guesses [(x, y)] ultimately caused an enemy ship to sink.

    Raises: ["(x, y) not a location you targeted"] if [(x, y)] was a location
    that was not previously targeted by the player.  *)
let check_ship_sunk (x, y) player state = 
  match player with
  | Player1 -> 
    if not (get_player_guess (x, y) player state) then false
    else Battleship.coordinate_to_ship_position (x, y) state.player_2_ship_dict
  | Player2 ->
    if not (get_player_guess (x, y) player state) then false
    else Battleship.coordinate_to_ship_position (x, y) state.player_1_ship_dict

(* [error] represents a mistake in targeting, for example,
   an error is [CoordinateVisited] if that location was visited previously,
   and [OutOfBounds] if it was not on the map *)
type error = 
  | CoordinateVisited
  | OutOfBounds

(* [action] represents if a targeting action can be carried out or not,
   and is either a [Success] of a new state, with a ship hit or not,
   and a ship sunk or not,
   or a [failure] with an exception type. *)
type action = 
  | Success of t * bool * bool
  | Failure of t * error

let target_ship (x, y) player state = 
  if not (check_bounds (x, y)) then 
    Failure (state, OutOfBounds) 
  else if not (check_coordinate (x, y) player state) then 
    Failure (state, CoordinateVisited)
  else 
    let new_state = target (x, y) player state in 
    let hit_or_miss_status = get_player_guess (x, y) player new_state in 
    let ship_sunk = check_ship_sunk (x, y) player new_state in 
    Success (new_state, hit_or_miss_status, ship_sunk)

(** [guesses_to_matrix player game] is the matrix of guesses
    that the player made.  *)
let guesses_to_matrix player game = 
  let player_dict = 
    (match player with
     | Player1 -> game.player_1_grid_guesses
     | Player2 -> game.player_2_grid_guesses) in
  let grid = Array.make_matrix c_ROWS c_COLS "?" in
  let rec guesses_to_matrix_helper player_dict grid = 
    match player_dict with
    | [] -> grid
    | (x, y, Miss) :: t ->
      grid.(y).(x) <- "O";
      guesses_to_matrix_helper t grid
    | (x, y, Hit) :: t ->
      grid.(y).(x) <- "X";
      guesses_to_matrix_helper t grid
  in guesses_to_matrix_helper player_dict grid

let string_of_guesses player game = 
  game |> guesses_to_matrix player |> Battleship.string_of_matrix

let string_of_player_dict player game = 
  match player with 
  | Player1 ->
    game.player_1_ship_dict |> Battleship.string_of_dict
  | Player2 ->
    game.player_2_ship_dict |> Battleship.string_of_dict

(** [string_of_digit digit] is the string correspinding to a digit
    , e.g. [10] becomes ["10"] and one digit numbers become 
    [1] is [" 1"]. 

    Requires: [digit] is between [0] and [10] inclusive. *)
let string_of_digit digit = 
  match digit with
  | 10 -> string_of_int 10;
  | x -> " " ^ (string_of_int x)

(** [combine_helper lst1 lst2 acc digit] combines the two lists of strings
    via the rule [[a1;b1;c1;...]] [[a2;b2;c2;...]] becomes
    [[a1 ^ a2; b1 ^ b2; c1 ^ c2...]]. 

    Requires : [lst1] and [lst2] are lists of player boards.
    Requires: [digit] is between [0] and [10] inclusive. 

    Raises : ["lst2 longer than lst1"] if [lst2] length is greater than length 
    of[lst1]. 
    Raises : ["lst1 longer than lst2"] if [lst1] length is greater than length 
    of[lst2]. *)
let rec combine_helper lst1 lst2 acc digit =  
  match lst1, lst2 with
  | [], [] -> acc
  | [], _ -> failwith "lst2 longer than lst1"
  | _, [] -> failwith "lst1 longer than lst2"
  | h1 :: t1, h2 :: t2 -> 
    let digit_string = string_of_digit digit in 
    combine_helper t1 t2 
      (acc @ [digit_string ^ " " ^ h1 ^ c_BOARD_SEP ^ digit_string  ^ " " ^ h2]) 
      (digit + 1)

let combine_boards lst1 lst2 = 
  let x_axis = 
    "    A  B  C  D  E  F  G  H  I  J" 
    ^ c_BOARD_SEP 
    ^ "     A B C D E F G H I J" in
  let empty = "" in 
  x_axis :: empty :: (combine_helper lst1 lst2 [] 1)

let rec print_boards board_list = 
  match board_list with
  | [] -> ()
  | h :: t -> 
    print_endline h; 
    print_boards t

let print_guesses player game = 
  game |> guesses_to_matrix player |> Battleship.print_matrix

let print_player_dict player game = 
  match player with 
  | Player1 ->
    game.player_1_ship_dict |> Battleship.print_dict 
  | Player2 ->
    game.player_2_ship_dict |> Battleship.print_dict 

let check_victory player game =
  match player with
  | Player1 ->
    Battleship.check_all_ships_damaged game.player_2_ship_dict
  | Player2 -> 
    Battleship.check_all_ships_damaged game.player_1_ship_dict

(* AI Related Stuff *)

(** [create_pairs m n] are all the possible pairs with
    first element from [1] to [m] inclusive
    and the second from [1] to [n] inclusive. 

    Requires: [m] and [n] are both non-zero positive integers. *)
let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
  in create_rows m n []

(** [choose_target pairs_list] is the random target that the Easy 
    and medium and hard AI's choose. It is an element of 
    [pairs_list]. 

    Requires: [pairs_list] consists of only elements never targeted before,
    and that [pairs_list] is not empty. *) 
let choose_target pairs_list = 
  let () = Random.self_init () in   
  pairs_list 
  |> List.length 
  |> Random.int 
  |> List.nth pairs_list

(** [get_hit_or_miss (x, y) grid_guesses] is whether a loction 
    [(x, y)] has been hit or not, not returning a bool, as
    the other function does. 
    If [(x, y)] was not guessed and is not in [grid_guesses],
    returns [Miss]. *)
let rec get_hit_or_miss (x, y) grid_guesses = 
  match grid_guesses with
  | (x', y', guess) :: t -> 
    if x = x' && y = y' then guess
    else get_hit_or_miss (x, y) t
  | [] -> Miss

(** [get_occupied_or_not (x, y) enemy_ship_pos_list] is [true]
    if [(x, y)] is occupied on the [enemy_ship_pos_list]. *)
let get_occupied_or_not (x, y) enemy_ship_pos_list = 
  enemy_ship_pos_list 
  |> List.fold_left (fun acc (str, int_lst) -> int_lst :: acc) [] 
  |> List.flatten 
  |> List.map (fun (x, y, _) -> (x, y)) 
  |> List.mem (x, y)

(** [get_surrounding_positions (x, y) state] are the surrounding positions
    on the board around the targeted location [(x, y)] in the current [state] 
    after [(x, y)] has been targeted and marked on pos targeted. 

    Requires: [(x, y)] have [x] and [y] in the 0 and 9 inclusive. 

    Requires: Must be called after target for the given AI. *)
let get_surrounding_positions (x, y) state = 
  if get_occupied_or_not (x - 1, y - 1) state.player_1_ship_dict then
    [(x, (y - 1));
     (x, (y + 1));
     ((x - 1), y);
     ((x + 1), y);] 
    |> List.filter (fun (x, y) -> check_bounds (x - 1, y - 1)) 
  else []

(** [print_pairs lst] is a printed representation of pairs in a list. 
    For Debugging Purposes. *)
let rec print_pairs lst = 
  match lst with
  | (x, y) :: t -> 
    ("(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")") 
    |> print_endline; 
    print_pairs t
  | [] -> ()

(** [update_targeted_locations ai_data target_loc] is the new ai with
    updated information about where the AI just targeted included 
    in its [pos_targeted], [pos_remaining] and [surrounding_positions]
    fields.

    Requires: Must apply to ai every single time a location is targeted
    by the AI follow a call of [target].
    This must be used by all Ai difficulties.  *)
let update_targeted_locations ai_data target_loc = 
  let previously_targeted_locs = ai_data.pos_targeted in 
  let new_surround_pos = 
    ai_data.surrounding_positions 
    |> (@) (get_surrounding_positions target_loc ai_data)
    |> List.sort_uniq 
      (fun (x1, y1) (x2, y2) -> 
         if x1 = x2 && y1 = y2 then 0 
         else if x1 > x2 then 1 
         else -1)
    |> List.filter (fun elt -> elt <> target_loc) 
    |> List.filter (fun elt -> not (List.mem elt previously_targeted_locs)) in 
  {ai_data with
   pos_targeted = target_loc :: (ai_data.pos_targeted);
   pos_remaining = 
     List.filter (fun elt -> elt <> target_loc) (ai_data.pos_remaining);
   surrounding_positions = new_surround_pos;}

let initialize_ai player_1 player_2 player_1_pregame player_2_pregame = {
  current_player = player_1;
  next_player = player_2;
  player_1_grid_guesses = [];
  player_1_ship_dict = player_1_pregame;
  player_2_grid_guesses = [];
  player_2_ship_dict = player_2_pregame;
  pos_targeted = [];
  pos_remaining = create_pairs c_ROWS c_COLS;
  surrounding_positions = [];
  guess_phase = true;
  hard_ai = initialize_hard_ai;
  player_1_inv = [];
  player_2_inv = [];
}

let target_ai ai_data = 
  let chosen_target = choose_target ai_data.pos_remaining in 
  let ai_data' = update_targeted_locations ai_data chosen_target in 
  (chosen_target, ai_data')

(** [medium_choose_target pairs_list surrounding_pairs] is 
    the target randomly chosen from [surrounding_pairs]. The target
    is guaranteed not to be in [pairs_list]. 

    Requires : [surrounding_pairs] is not empty!.
    E.G. there must still be some location untargeted by this AI. *)
let medium_choose_target pairs_list surrounding_pairs = 
  if surrounding_pairs = [] then choose_target pairs_list
  else choose_target surrounding_pairs

let target_medium_ai ai_data = 
  let chosen_target = 
    medium_choose_target ai_data.pos_remaining ai_data.surrounding_positions in 
  let ai_data' = update_targeted_locations ai_data chosen_target in 
  (chosen_target, ai_data')

let target_hard_ai state = 
  let ai = state.hard_ai in 
  if get_guess_phase ai 
  then 
    let (chosen_target, new_ai) = random_target ai in 
    let new_state = update_targeted_locations state chosen_target in 
    (chosen_target, {new_state with hard_ai = new_ai})
  else 
    let (chosen_target, new_ai) = smart_target ai in 
    let new_state = update_targeted_locations state chosen_target in 
    (chosen_target, {new_state with hard_ai = new_ai})

(** [update_hard_smart_mode state ai ship_hit ship_sunk] updates the [ai]
    in [state] when the [ai] is in smart mode to account for [ship_hit] and 
    [ship_sunk]. Works only for hard Ai.

    Requires: [ai] is in smart target mode already. 
    Raises: [A target cannot sink a ship, yet not hit a ship] if
    [ship_hit] is [true] but [ship_sunk] is [false] or vice versa. *)
let update_hard_smart_mode state ai ship_hit ship_sunk = 
  if ship_hit && ship_sunk then 
    let random_ai = smart_to_random ai in 
    {state with hard_ai = random_ai}
  else if ship_hit && (not ship_sunk) then state
  else if (not ship_hit) && (not ship_sunk) then 
    let new_smart_ai = update_smart_after_miss ai in 
    {state with hard_ai = new_smart_ai}
  else 
    failwith "A target cannot sink a ship, yet not hit a ship"

let update_hard_ai state ship_hit ship_sunk target_coord = 
  let ai = state.hard_ai in 
  if get_guess_phase ai 
  then begin
    if ship_hit 
    then let smart_ai = 
           (ai |> random_to_smart |> update_smart_ai_after_hit) target_coord in
      {state with hard_ai = smart_ai}
    else state end
  else if all_bounds_or_all_lists ai
  then begin
    let random_ai = smart_to_random ai in 
    {state with hard_ai = random_ai} end
  else 
    update_hard_smart_mode state ai ship_hit ship_sunk
(* if ship_hit && ship_sunk then 
   let random_ai = smart_to_random ai in 
   {state with hard_ai = random_ai}
   else if ship_hit && (not ship_sunk) then state
   else if (not ship_hit) && (not ship_sunk) then 
   let new_smart_ai = update_smart_after_miss ai in 
   {state with hard_ai = new_smart_ai}
   else 
   failwith "A target cannot sink a ship, yet not hit a ship" end *)

let target_insane_ai state = 
  let ai = state.hard_ai in 
  if get_insane_phase ai 
  then 
    let (chosen_target, new_ai) = insane_target ai in 
    let new_state = update_targeted_locations state chosen_target in 
    (chosen_target, {new_state with hard_ai = new_ai})
  else 
    let (chosen_target, new_ai) = smart_target ai in 
    let new_state = update_targeted_locations state chosen_target in 
    (chosen_target, {new_state with hard_ai = new_ai})

(** [update_insane_smart_mode state ai ship_hit ship_sunk] updates the [ai]
    in [state] when the [ai] is in smart mode to account for [ship_hit] and 
    [ship_sunk].  Works only for insane Ai.

    Requires: [ai] is in smart target mode already. 
    Raises: [A target cannot sink a ship, yet not hit a ship] if
    [ship_hit] is [true] but [ship_sunk] is [false] or vice versa. *)
let update_insane_smart_mode state ai ship_hit ship_sunk = 
  if ship_hit && ship_sunk then 
    let insane_ai = smart_to_insane ai in 
    {state with hard_ai = insane_ai}
  else if ship_hit && (not ship_sunk) then state
  else if (not ship_hit) && (not ship_sunk) then 
    let new_insane_ai = update_smart_after_miss ai in 
    {state with hard_ai = new_insane_ai}
  else 
    failwith "A target cannot sink a ship, yet not hit a ship"

let update_insane_ai state ship_hit ship_sunk target_coord = 
  let ai = state.hard_ai in 
  if get_insane_phase ai 
  then begin
    if ship_hit 
    then let smart_ai = 
           (ai |> insane_to_smart |> update_smart_ai_after_hit) target_coord in
      {state with hard_ai = smart_ai}
    else state end
  else if all_bounds_or_all_lists ai
  then begin
    let random_ai = smart_to_random ai in 
    {state with hard_ai = random_ai} end
  else 
    update_insane_smart_mode state ai ship_hit ship_sunk

(* else begin
   if ship_hit && ship_sunk then 
    let insane_ai = smart_to_insane ai in 
    {state with hard_ai = insane_ai}
   else if ship_hit && (not ship_sunk) then state
   else if (not ship_hit) && (not ship_sunk) then 
    let new_insane_ai = update_smart_after_miss ai in 
    {state with hard_ai = new_insane_ai}
   else 
    failwith "A target cannot sink a ship, yet not hit a ship" end *)


(* powerup related code *)

(** [powerup_to_string powerup] converts the powerup_type [powerup] to the 
    appropriate string name of the type. *)
let powerup_to_string powerup =
  match powerup with
  | SquareHit -> "squarehit"
  | ReHit -> "rehit"
  | InstaKill -> "instakill"

(** [string_to_powerup str ] converts [str] to the appropriate
    powerup_type [powerup] to the . 
    Raises: ["Violates preconditions: must be parsed as a proper powerup"] 
    if [str] is not one of [squarehit, rehit or instakill]. *)
let string_to_powerup str = 
  if str = "squarehit" then SquareHit 
  else if str = "rehit" then ReHit
  else if str = "instakill" then InstaKill
  else failwith "Violates preconditions: must be parsed as a proper powerup"

(** [powerups_to_string_list player state] stores all the powerups that a 
    [player] has in [state] in a list. *)
let powerups_to_string_list  player state = 
  match player with
  | Player1 ->
    List.fold_left (fun init powerup -> powerup :: init) [] state.player_1_inv
  | Player2 ->
    List.fold_left (fun init powerup ->  powerup :: init) [] state.player_2_inv

let print_powerups player state = 
  print_endline "These are you power ups:";
  powerups_to_string_list player state |> List.map print_endline |> ignore

let get_player_powerups player state =
  match player with
  | Player1 -> state.player_1_inv
  | Player2 -> state.player_2_inv 

(** [(>>=) f (x, y) player powerup_type m] acts as a monad bind, determing
    if a target at [x] [y] is a [Success] hit or a [Failure] 
    and then applying [f] to successes only to create a new monad of [Success]
    or [Failure].*)
let (>>=) f (x, y) player powerup_type m  =
  match m with
  | Success (state, hit_miss_status, ship_sunk) -> 
    f (x, y) player powerup_type state hit_miss_status ship_sunk 
  | Failure _ as f -> f

(** [(<<>>) m] converts a [Success/Failure] target monad to 
    either a [Usable of state hit sunk] if [Success] or [Unusable] otherwise. *)
let (<<>>) m = 
  match m with 
  | Success (state, hit_miss_status, ship_sunk) -> 
    Usable (state, hit_miss_status, ship_sunk)
  | Failure _ -> Unusable

(** [monad_target (x, y) player powerup_type state hit_miss_status ship_sunk]
    targetts location [x] [y] for the [player] with [powerup_type]
    and updates [hit_miss_status] and [ship_sunk] with the new
    hit or miss and sunk statuses.  *) 
let monad_target (x, y) player powerup_type state hit_miss_status ship_sunk =  
  match target_ship (x, y) player state with
  | Success (state', hit', sunk') ->
    Success (state', (hit' || hit_miss_status), ship_sunk || sunk')
  | Failure _ as f-> f

(** [update_state_powerups (x, y) player powerup_type state hit sunk]
    updates the [player] powerup inventory with [powerup_type] powerup
    and is the new updated state and if the shio was hit and siunk.  *)
let update_state_powerups (x, y) player powerup_type state hit sunk = 
  match player with 
  | Player1 ->  
    let newlist = 
      List.filter (fun pow -> (pow <> powerup_type)) state.player_1_inv in 
    Success ({state with player_1_inv = newlist}, hit, sunk)
  | Player2 ->
    let newlist = 
      List.filter (fun pow -> (pow <> powerup_type)) state.player_2_inv in 
    Success ({state with player_2_inv = newlist}, hit, sunk)

(** [process_square_hits x y player state powerup_type] is the result
    of using an squarehit at [x] [y] for [player] in [state] 
    with [poweryp_type] powerup. It is either [Usable] or [Unsuable]. *)
let process_square_hits x y player state powerup_type = 
  target_ship (x, y) player state
  |> (>>=) monad_target (x + 1, y) player powerup_type
  |> (>>=) monad_target (x, y + 1) player powerup_type
  |> (>>=) monad_target (x + 1, y + 1) player powerup_type
  |> (>>=) update_state_powerups (x, y) player powerup_type
  |> (<<>>)

(** [process_rehit x y player state powerup_type] is the result
    of using an rehit at [x] [y] for [player] in [state] 
    with [poweryp_type] powerup. It is either [Usable] or [Unsuable]. *)
let process_rehit x y player state powerup_type =
  target_ship (x, y) player state
  |> (>>=) update_state_powerups (x, y) player powerup_type
  |> (<<>>) 

(** [get_ship_dict x y player state] is the ship dictionary 
    of [player]'s opponent.  *)
let get_ship_dict x y player state = 
  match player with 
  | Player1 ->
    state.player_2_ship_dict 
  | Player2 -> 
    state.player_1_ship_dict

(** [make_ship_sink x y player state] converts the entire ship with 
    coordinate [x][y] to be completed damaged for [player]. *)
let make_ship_sink x y player state = 
  let ship_dict = get_ship_dict x y player state in 
  Battleship.get_ship_coordinates x y ship_dict

(** [handle_instakill_hit player player_dict coords state powerup] updates 
    [state] after a hit target for instakill for [player]. In particular,
    the ship dictionary, grid guesses and inventory are updated. 

    It will always return [Usable] with ship hit and ship sunk as [true].

    Requires: 
    Do not use [target]'s new state used as [state] for this function. *)
let handle_instakill_hit player player_dict coords state powerup = 
  match player with
  | Player1 -> let newlist = 
                 List.filter (fun pow -> (pow <> powerup)) state.player_1_inv in
    Usable ({state with 
             player_2_ship_dict = 
               player_dict;
             player_1_grid_guesses = 
               coords @ state.player_1_grid_guesses;
             player_1_inv = newlist}, true, true)
  | Player2 -> let newlist = 
                 List.filter (fun pow -> (pow <> powerup)) state.player_2_inv in
    Usable({state with 
            player_1_ship_dict = 
              player_dict;
            player_2_grid_guesses = 
              coords @ state.player_2_grid_guesses;
            player_2_inv = newlist}, true, true)

(** [handle_instakill_miss player state powerup] updates [state]
    after a miss target for instakill for [player]. 

    It will always return [Usable] with ship hit and ship sunk as [false].

    Requires: [target]'s new state used as [state] for this function. *)
let handle_instakill_miss player state powerup = 
  let used_state = 
    match player with
    | Player1 -> 
      let newlist = 
        List.filter (fun pow -> (pow <> powerup)) state.player_1_inv in
      {state with player_1_inv = newlist}
    | Player2 -> 
      let newlist = 
        List.filter (fun pow -> (pow <> powerup)) state.player_2_inv in
      {state with player_2_inv = newlist} in 
  Usable (used_state, false, false)

(** [process_instakill x y player state powerup_type] is the result
    of using an instakill at [x] [y] for [player] in [state] 
    with [poweryp_type] powerup. It is either [Usable] or [Unsuable]. *)
let process_instakill x y player state powerup_type = 
  match target_ship (x, y) player state with 
  | Success (new_state, hit, sunk) -> begin
      if hit then 
        let new_player_dict, ship_coords =
          make_ship_sink x y player state in 
        let hit_ship_coords = 
          List.map (fun (x, y) -> (x, y, Hit)) ship_coords in
        handle_instakill_hit 
          player new_player_dict hit_ship_coords state powerup_type
      else
        handle_instakill_miss player new_state powerup_type 
    end
  | Failure _ -> Unusable 

let update_powerup_state x y player state powerup_type =
  match player with 
  | Player1  -> begin
      if powerup_type = "squarehit" then 
        process_square_hits x y player state powerup_type
      else if powerup_type = "rehit" then 
        process_rehit x y player state powerup_type
      else if powerup_type = "instakill" then 
        process_instakill x y player state powerup_type
      else failwith "powerup must be parsed into three strings" end
  | Player2 -> begin
      if powerup_type = "squarehit" then 
        process_square_hits x y player state powerup_type
      else if powerup_type = "rehit" then 
        process_rehit x y player state powerup_type
      else if powerup_type = "instakill" then 
        process_instakill x y player state powerup_type
      else failwith "powerup must be parsed into three strings" end

(** [add_powerup player state name] adds a powerup [name] to the [player]
    inventory in [state]. 

    Requires: [name] is one of ["instakill"], ["rehit"] or ["squarehit"]. *)
let add_powerup player state name = 
  match player with
  | Player1 ->
    {state with player_1_inv = name :: state.player_1_inv}
  | Player2 ->
    {state with player_2_inv = name :: state.player_2_inv}