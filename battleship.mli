type direction = 
  | Left
  | Right
  | Up
  | Down

type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

type status = 
  | Damaged
  | Undamaged

type player = 
  | Player1
  | Player2

type list_t = (string * ((int * int * status) list) ) list

type t

type error = 
  | BoundsError
  | OccupiedTile
  | OutOfShips
  | NonexistShip

type action = 
  | Success of t
  | Failure of t * error

val starter_ship_list : ship_type list
val empty: t 
val choose_player : bool -> player
val ship_length: ship_type -> int
val string_to_ship : string -> ship_type
val ship_to_string : ship_type -> string
val pretty_print_ship_to_string : ship_type -> string
val string_to_direction : string -> direction
val generate_num_lst : int -> int -> (int -> int -> int) -> int list -> int list
(*
val get_ship_list: t -> string list -> string list
val get_ship_from_coord: int -> int -> t -> string 
*)
val generate_pos_list: int * int -> int -> direction -> (int * int * status) list
val insert : string -> int * int -> direction -> 
  list_t ->
  list_t
val remove : string -> list_t
  -> (string * (int * int * status) list) list
val check_bounds : int * int -> int -> direction -> bool
val check_cell_occupied : int * int -> list_t -> bool
val check_cell_unoccupied : int * int -> list_t -> bool
val check_unoccupied : int * int -> int -> direction -> list_t -> bool
val check_ship_placed : ship_type -> player -> t -> bool
val insert_ship : int * int -> direction -> ship_type -> player -> t -> action
val empty_board : unit -> string array array
val make_grid : list_t -> string array array -> string array array
val string_of_matrix : string array array -> string list
val combine_boards : string list -> string list -> string list
val print_boards : string list -> unit
val print_matrix : string array array -> unit
val print_player_ship_board : t -> player -> unit
val print_dict : list_t -> unit
val string_of_dict : list_t -> string list
val change_damage_list : int * int -> (int * int * status) list -> 
  (int * int * status) list -> (int * int * status) list
val change_to_damage : int * int -> list_t -> list_t
val remaining_ships : player -> t -> int
val remaining_ships_to_place : player -> t -> string list
val check_ship_can_be_removed : string -> player -> t -> bool
val remove_ship : string -> player -> t -> t
val check_all_ships_damaged : list_t -> bool
val check_ship_sunk : (int * int * status) list -> bool
val check_coordinate_in_positions : int * int -> (int * int * status) list -> bool
val coordinate_to_ship_position : int * int -> list_t -> bool
val get_player_dict : player -> t -> list_t
val direction_list : string list
val choose_target : (int * int) list -> int * int
val create_pairs : int -> int -> (int * int) list
val ship_type_to_ship_name : ship_type -> string
val random_ship : player -> t -> (int * int) * string * string
val randomly_laydown_ships : t -> (int * int) * string * string
