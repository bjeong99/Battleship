let c_ROWS = 10
let c_COLS = 10
let c_NUM_SHIPS = 5

(* [state] is the data in the game that has the ability to change
    in the game and must be updated.

    Effectively, this functions as the in game portion
    that follows the pre game set up portion

    We will update two things:
      player one's locations that he has targeted on
      player two
      and player two's locations that he has targeted on
      player one *)

type name = string

type player = 
  | Player1 of string
  | Player2 of string

type tile_status = 
  | Unknown
  | Hit
  | Miss

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
    Where player stored his ships : so we can mark it damaged or not*)

type t = {
  current_player : player;
  next_player    : player;
  player_1_ship_placement : Battleship.tile array array;
  player_1 : plyr_target_data;
  player_1_ships_found : ships list;
  player_1_ships_remaining : ships list;
  player_2_ship_placement : Battleship.tile array array;
  player_2 : plyr_target_data;
  player_2_ships_found : ships list;
  player_2_ships_remaining : ships list;
}

let init_enemy_board = 
  Array.make_matrix c_ROWS c_COLS Unknown

(** [init_unknown_list n acc elt] is the list 
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

