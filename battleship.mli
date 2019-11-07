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
type t

exception NonexistShip

val empty: t 
val ship_length: ship_type -> int
val get_ship_list: t -> string list -> string list
val get_ship_from_coord: int -> int -> t -> string 
val generate_pos_list: int * int -> int -> direction -> (int * int * status) list
val string_to_ship: string -> ship_type
val insert: string -> int * int -> direction -> t -> t
val remove: string -> t -> t
val check_bounds: int * int -> int -> direction -> bool
val check_cell_occupied: int * int -> t -> bool
val check_unoccupied: int * int -> int -> direction -> t -> bool
val empty_board: string array array
val make_grid: t -> string array array -> string array array