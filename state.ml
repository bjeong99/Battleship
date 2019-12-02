let c_ROWS = 10
let c_COLS = 10

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
}

let update_player state = 
  {state with 
   current_player = state.next_player;
   next_player = state.current_player;}

let get_current_player state = 
  state.current_player

let get_next_player state = 
  state.next_player

let check_coordinate_guessed (x, y) player state = 
  match player with
  | Player1 -> 
    state.player_1_grid_guesses |> List.map (fun (x, y, guess) -> (x, y)) |> List.mem (x, y)
  | Player2 -> 
    state.player_2_grid_guesses |> List.map (fun (x, y, guess) -> (x, y)) |> List.mem (x, y)

let check_bounds (x, y)  = 
  x >= 0 && x < 10 && y >= 0 && y < 10

let check_coordinate (x, y) player state = 
  check_bounds (x, y) && not (check_coordinate_guessed (x, y) player state)

let target (x, y) player state = 
  match player with
  | Player1 ->
    if Battleship.check_cell_occupied (x, y) state.player_2_ship_dict 
    then {state with 
          player_2_ship_dict = Battleship.change_to_damage (x, y) state.player_2_ship_dict;
          player_1_grid_guesses = (x, y, Hit) :: state.player_1_grid_guesses}
    else {state with player_1_grid_guesses = (x, y, Miss) :: state.player_1_grid_guesses}
  | Player2 ->
    if Battleship.check_cell_occupied (x, y) state.player_1_ship_dict
    then {state with 
          player_1_ship_dict = Battleship.change_to_damage (x, y) state.player_1_ship_dict;
          player_2_grid_guesses = (x, y, Hit) :: state.player_2_grid_guesses}
    else {state with player_2_grid_guesses = (x, y, Miss) :: state.player_2_grid_guesses}

let rec get_hit_status (x, y) dict = 
  match dict with
  | [] -> failwith "(x, y) not a location you targeted"
  | (x', y', status) :: t -> 
    if x = x' && y = y' then status = Hit
    else get_hit_status (x, y) t

let get_player_guess (x, y) player state = 
  match player with
  | Player1 -> 
    get_hit_status (x, y) state.player_1_grid_guesses
  | Player2 ->
    get_hit_status (x, y) state.player_2_grid_guesses

let check_ship_sunk (x, y) player state = 
  match player with
  | Player1 -> 
    if not (get_player_guess (x, y) player state) then false
    else Battleship.coordinate_to_ship_position (x, y) state.player_2_ship_dict
  | Player2 ->
    if not (get_player_guess (x, y) player state) then false
    else Battleship.coordinate_to_ship_position (x, y) state.player_1_ship_dict

type error = 
  | CoordinateVisited
  | OutOfBounds

type action = 
  | Success of t * bool * bool
  | Failure of t * error

let target_ship (x, y) player state = 
  if not (check_bounds (x, y)) then Failure (state, OutOfBounds) 
  else if not (check_coordinate (x, y) player state) then Failure (state, CoordinateVisited)
  else 
    let new_state = target (x, y) player state in 
    let hit_or_miss_status = get_player_guess (x, y) player new_state in 
    let ship_sunk = check_ship_sunk (x, y) player new_state in 
    Success (new_state, hit_or_miss_status, ship_sunk)

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

let combine_boards lst1 lst2 = 
  let x_axis = "    A  B  C  D  E  F  G  H  I  J" ^ c_BOARD_SEP ^ "     A B C D E F G H I J" in
  let empty = "" in 
  let rec combine_helper lst1 lst2 acc digit=  
    match lst1, lst2 with
    | [], [] -> acc
    | [], _ -> failwith "lst2 longer than lst1"
    | _, [] -> failwith "lst1 longer than lst2"
    | h1 :: t1, h2 :: t2 -> 
      let digit_string = 
        match digit with
        | 10 -> string_of_int 10;
        | x -> " " ^ (string_of_int x);
      in
      combine_helper t1 t2 ((digit_string ^ " " ^ h1 ^ c_BOARD_SEP ^ digit_string ^ " " ^ h2) :: acc) (digit - 1)
  in x_axis :: empty :: (combine_helper lst1 lst2 [] 10)

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

(* AI SHIT *)

let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
  in create_rows m n []

let choose_target pairs_list = 
  let () = Random.self_init () in   
  pairs_list 
  |> List.length 
  |> Random.int 
  (*|> (fun l -> l - 1) *)
  |> List.nth pairs_list

let rec get_hit_or_miss (x, y) grid_guesses = 
  match grid_guesses with
  | (x', y', guess) :: t -> 
    if x = x' && y = y' then guess
    else get_hit_or_miss (x, y) t
  | [] -> Miss

let get_occupied_or_not (x, y) enemy_ship_pos_list = 
  enemy_ship_pos_list |> List.fold_left (fun acc (str, int_lst) -> int_lst :: acc) [] |> List.flatten |> List.map (fun (x, y, _) -> (x, y)) |> List.mem (x, y)

let get_surrounding_positions (x, y) state = 
  if get_occupied_or_not (x - 1, y - 1) state.player_1_ship_dict then
    [(x, (y - 1));
     (x, (y + 1));
     ((x - 1), y);
     ((x + 1), y);] 
    |> List.filter (fun (x, y) -> check_bounds (x - 1, y - 1)) 
  else []

let rec print_pairs lst = 
  match lst with
  | (x, y) :: t -> ("(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")") |> print_endline; print_pairs t
  | [] -> ()

let update_targeted_locations ai_data target = 
  let previously_targeted_locs = ai_data.pos_targeted in 
  let new_surround_pos = 
    ai_data.surrounding_positions 
    |> (@) (get_surrounding_positions target ai_data)
    |> List.sort_uniq 
      (fun (x1, y1) (x2, y2) -> 
         if x1 = x2 && y1 = y2 then 0 
         else if x1 > x2 then 1 
         else -1)
    |> List.filter (fun elt -> elt <> target) 
    |> List.filter (fun elt -> not (List.mem elt previously_targeted_locs)) in 
  print_endline "These are the pairs of close locations";
  print_pairs new_surround_pos;
  print_endline "These are the position_targeted";
  print_pairs previously_targeted_locs;
  {
    ai_data with
    pos_targeted = target :: (ai_data.pos_targeted);
    pos_remaining = 
      List.filter (fun elt -> elt <> target) (ai_data.pos_remaining);
    surrounding_positions = new_surround_pos;
  }

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
}

let target_ai ai_data = 
  let chosen_target = choose_target ai_data.pos_remaining in 
  let ai_data' = update_targeted_locations ai_data chosen_target in 
  (chosen_target, ai_data')

let medium_choose_target pairs_list surrounding_pairs = 
  if surrounding_pairs = [] then choose_target pairs_list
  else choose_target surrounding_pairs

let target_medium_ai ai_data = 
  let chosen_target = medium_choose_target ai_data.pos_remaining ai_data.surrounding_positions in 
  let ai_data' = update_targeted_locations ai_data chosen_target in 
  (chosen_target, ai_data')






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