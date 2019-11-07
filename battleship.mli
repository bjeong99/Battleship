type direction = 
  | Left
  | Right
  | Up
  | Down
type status = 
  | Unknown 
  | Hit
  | Miss
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
val generate_post_list ()