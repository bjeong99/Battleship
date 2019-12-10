open Hard_ai

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

(* OLD BROKEN VERSION< APPENDS END OF LIST AT BEGINNING TO REVERSE ORDER
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
      ((digit_string ^ " " ^ h1 ^ c_BOARD_SEP ^ digit_string  ^ " " ^ h2)::acc) 
      (digit - 1)
*)

let combine_boards lst1 lst2 = 
  let x_axis = 
    "    A  B  C  D  E  F  G  H  I  J" 
    ^ c_BOARD_SEP 
    ^ "     A B C D E F G H I J" in
  let empty = "" in 
  x_axis :: empty :: (combine_helper lst1 lst2 [] 1) (* last arg was 10, not 1 before *)

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
  else begin
    if ship_hit && ship_sunk then 
      let random_ai = smart_to_random ai in 
      {state with hard_ai = random_ai}
    else if ship_hit && (not ship_sunk) then state
    else if (not ship_hit) && (not ship_sunk) then 
      let new_smart_ai = update_smart_after_miss ai in 
      {state with hard_ai = new_smart_ai}
    else 
      failwith "A target cannot sink a ship, yet not hit a ship" end

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
  else begin
    if ship_hit && ship_sunk then 
      let insane_ai = smart_to_insane ai in 
      {state with hard_ai = insane_ai}
    else if ship_hit && (not ship_sunk) then state
    else if (not ship_hit) && (not ship_sunk) then 
      let new_insane_ai = update_smart_after_miss ai in 
      {state with hard_ai = new_insane_ai}
    else 
      failwith "A target cannot sink a ship, yet not hit a ship" end

(** [powerup_to_string powerup] converts the powerup_type [powerup] to the 
    appropriate string name of the type. *)
let powerup_to_string powerup =
  match powerup with
  | SquareHit -> "squarehit"
  | ReHit -> "rehit"
  | InstaKill -> "instakill"

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

let update_powerup_state player state powerup_type =
  match player with 
  |Player1 ->
    let newlist = List.filter (fun pow -> (pow <> powerup_type)) state.player_1_inv in 
    {state with player_1_inv = newlist}
  |Player2 ->
    let newlist = List.filter (fun pow -> (pow <> powerup_type)) state.player_2_inv in 
    {state with player_2_inv = newlist}

(* | Player2 ->
   if Battleship.check_cell_occupied (x, y) state.player_1_ship_dict
   then {state with 
      player_1_ship_dict = 
        Battleship.change_to_damage (x, y) state.player_1_ship_dict;
      player_2_grid_guesses = (x, y, Hit) :: state.player_2_grid_guesses}
   else {state with 
      player_2_grid_guesses = (x, y, Miss) :: state.player_2_grid_guesses} *)

let add_powerup player state name = 
  match player with
  | Player1 ->
    {state with player_1_inv = name :: state.player_1_inv}
  | Player2 ->
    {state with player_2_inv = name :: state.player_2_inv}

     (*
     let init_state bs1 bs2 = 
     let empty_grid = empty_board in 
     {
     grid_ship = empty_grid;
     grid_guess = empty_grid;
     ship_list = bs1;
     hit_list = bs2;
     }
   *)
     (*
     let update_player_grid dict =
     make_grid dict empty_board

     let update_guess_grid dict = 
     make_grid dict empty_board

     (** [already_guessed x y board] takes in coordinates [x y] and if already guessed, then true*)
     let already_guessed x y self_guess_dict = 
     not (check_cell_unoccupied (x, y) self_guess_dict)

     let check_enemy_board x y enemy_board = 
     not (check_cell_unoccupied (x, y) enemy_board)

     let update_guess_dict x y guess_dict miss= 
     if miss then 
     let new_dict = insert "M" (x, y) Up guess_dict

     let rec check_sunk lst_of_coord =
     match lst_of_coord with
     |[] -> true
     |(_,_,status) :: t -> if status = Damaged then check_sunk t else false

     let rec check_victory dict = 
     match dict with
     | [] -> true
     | (ship, lst) :: t -> check_sunk lst && check_victory t

   *)

(* (** [init_unknown_list n acc elt] is the list 
   with [elt] in it [n] times following [acc]. 
   Requires: [n] is greater than or equal to [0].*)
   let rec init_unknown_list (n : int) (acc : 'a list) (elt : 'a) : 'a list = 
   if n = 0 then acc
   else init_unknown_list (n - 1) (elt :: acc) elt 

   (** [init_tile_status] is the initial tile status
   of any player, with all tiles of their opponent's board
   initialized to unknown. It represents the initial status
   of the game before any actions have taken place. 
   Requires: The number of rows on the board is equal 
   to the number of columns on the board. *)
   let init_tile_status : tile_status list list = 
   Unknown |> init_unknown_list n_cols [] |> init_unknown_list n_rows [] 

   (** [init_state starting_player following_player] 
   is the state of the game at the beginning. *)
   let init_state (starting_player : string) (following_player : string) = {
   current_player = Player1 starting_player;
   next_player = Player2 following_player;
   player_1 = init_tile_status;
   player_1_ships_found = [];
   player_2 = init_tile_status;
   player_2_ships_found = [];
   }

   (** [update_player s] is the new Battleship game state
   with the player who is to move next, b. *)
   let update_player (s : t) : t = 
   {s with current_player = s.next_player; next_player = s.current_player;}

   (** [change_index lst acc pos] is the new list with 
   element at [pos] position incremented up by 1. 
   Raises: [Failure pos too big] if [pos] is not a 
   position in [lst].
   Requires: [pos] is indexed from 0.*)
   let rec change_index elt lst acc pos = 
   match pos, lst with
   | 0, [] -> failwith "x pos too big"
   | 0, h :: t -> List.rev_append acc ((elt) :: t)
   | n, [] -> failwith "x pos too big"
   | n, h :: t -> change_index elt t (h :: acc) (pos - 1)

   let rec change_matrix_index elt matrix acc x y =
   match y, matrix with
   | 0, [] -> failwith "y row value too big"
   | 0, h :: t -> List.rev_append acc (change_index elt h [] x :: t)
   | n, [] -> failwith "y row value too big"
   | n, h :: t -> change_matrix_index elt t (h :: acc) x (y - 1)

   let insert_array x y arr value = 
   arr.(y).(x) <- value;
   arr

   let update_player_guesses (s : t) (x : int) (y :int) (new_elt : tile_status) : t = 
   match s.current_player with
   | Player1 _ -> 
   {s with player_1 = change_matrix_index new_elt s.player_1 [] (x - 1) (y - 1)}
   | Player2 _ -> 
   {s with player_2 = change_matrix_index new_elt s.player_2 [] (x - 1) (y - 1)}

   let get_player_guess (s : t) (x : int) (y : int) = 
   match s.current_player with
   | Player1 _ -> 
   ((s.player_1 |> List.nth) y |> List.nth) x
   | Player2 _ -> 
   ((s.player_2 |> List.nth) y |> List.nth) x


   let get_ships_sunk (s : t) (player : string) (player_ship_locations) = 



   type victory = 
   | Winner of name
   | Continue

   let get_current_player (s : t) : string = 
   match s.current_player with
   | Player1 s -> s
   | Player2 s -> s

   let get_next_player (s : t) : string = 
   match s.next_player with
   | Player1 s -> s
   | Player2 s -> s

   (** [victory s] is [Winner player]
   where [player] is the name of the player who won 
   iff the game is over in the game state s. 
   otherwise [Continue]. 

   REQUIRES: [victory s] must be applied after every turn cycle to give 
          the champion at the first instance, otherwise it will
          give the wrong champgion. *)
   let update_victory (s : t) : victory = 
   match s.current_player with
   | Player1 name -> 
   if List.length s.player_1_ships_found = c_NUM_SHIPS then true 
   else false
   | Player2 name -> 
   if List.length s.player_2_ships_found = c_NUM_SHIPS then true 
   else false

*)