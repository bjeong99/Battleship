type direction = 
  | Left
  | Right
  | Up
  | Down
type status = 
  | Damaged
  | Undamaged
type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 
type player
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


val empty: t 
val ship_length: ship_type -> int
(*
val get_ship_list: t -> string list -> string list
val get_ship_from_coord: int -> int -> t -> string 
*)
val generate_pos_list: int * int -> int -> direction -> (int * int * status) list
val string_to_ship: string -> ship_type
(*
val insert: string -> int * int -> direction -> t -> t
*)
val insert_ship: int * int -> direction -> ship_type -> player -> t -> action 
val remove_ship : string -> player -> t -> t
(*
val remove: string -> t -> t
val check_bounds: int * int -> int -> direction -> bool
*)
val check_cell_occupied: int * int -> list_t -> bool
(*
val check_unoccupied: int * int -> int -> direction -> t -> bool
val empty_board: string array array
val make_grid: t -> string array array -> string array array
*)
val change_to_damage : int * int -> list_t -> list_t

val check_all_ships_damaged : list_t -> bool
val coordinate_to_ship_position : int * int -> list_t -> bool

val choose_player: bool -> player
val string_to_direction: string -> direction
val remaining_ships: player -> t -> int
val remaining_ships_to_place: player -> t -> string list
val string_of_matrix : string array array -> string list
val string_of_dict : list_t -> string list
val print_player_ship_board: t -> player -> unit
val print_dict : list_t -> unit
val print_matrix : string array array -> unit
val get_player_dict : player -> t -> list_t

val randomly_laydown_ships : t -> ((int * int) * string * string)
