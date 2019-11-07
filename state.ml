let c_ROWS = 10
let c_COLS = 10
let c_NUM_SHIPS = 5
open Battleship
exception AlreadyGuessed of string

(* [state] is the data in the game that has the ability to change
    in the game and must be updated.

    Effectively, this functions as the in game portion
    that follows the pre game set up portion

    We will update two things:
      player one's locations that he has targeted on
      player two
      and player two's locations that he has targeted on
      player one *)


type plyr_target_data =
  tile_status list list

type ships = 
  | AircraftCarrier
  | Battleship
  | Destroyer
  | Submarine
  | PTBoat

(* state needs to store information for each player:
   Specifically, it would be nice if we could store:
    Where player stored his ships : so we can mark it damaged or not



   We need to know where player1 targeted, so that we can display his targets with
       either unknown (untargeted), hit (hit a ship of player2) or miss (miss a ship of player2)

   - data structures 
        We need to update dictionary of player 2, marking hits if player1 hits player 2
        We need an array of where player1 has targeted , marking with either unknown, hit or miss - to print to board

    Need to know the damage status of player1, so that we can display the status
      with either : No ship at location, hit (player 2 hit player1) or NotHit (player1's ship at coordinate safe)

   - data structures 
      We need to take the dictionary of player1, and have it display which coordinate of player1 ships were hit or safe

*)

type player = 
  | Player1
  | Player2

type grid_guess = 
  | Miss
  | Hit

type t = {
  current_player : player;
  next_player : player;
  player_1_grid_guesses : (int * int * grid_guess) list;
  player_1_ship_dict : Battleship.t;
  player_2_grid_guesses : (int * int * grid_guess) list;
  player_2_ship_dict : Battleship.t;
}

let init_state player_1 player_2 player_1_pregame player_2_pregame = {
  current_player = player_1;
  next_player = player_2;
  player_1_grid_guesses = [];
  player_1_ship_dict = player_1_pregame;
  player_2_grid_guesses = [];
  player_2_ship_dict = player_2_pregame;
}

let visited_cell = [Hit; Miss]

let check_valid_coordinate (x, y) player game = 
  match player with
  | Player1 -> 
    game.player_1_grid_guesses |> List.map (fun (x, y, guess) -> (x, y)) |> List.mem (x, y)
  | Player2 -> 
    game.player_2_grid_guesses |> List.map (fun (x, y, guess) -> (x, y)) |> List.mem (x, y)

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

let init_state bs1 bs2 = 
  let empty_grid = empty_board in 
  {
    grid_ship = empty_grid;
    grid_guess = empty_grid;
    ship_list = bs1;
    hit_list = bs2;
  }

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