type ship_health = 
  | ShipDamaged
  | ShipSafe

type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

type tile = 
  | Empty
  | Ship of ship_type * ship_health

type direction

type player 

type t

type error = 
  | BoundsError
  | OccupiedTile
  | OutOfShips

type action = 
  | Success of t
  | Failure of t * error

val initialize_pregame : unit -> t
val choose_player : bool -> player
val remaining_ships : player -> t -> int
val ship_placement_complete : player -> t -> bool
val remaining_ships_to_place : player -> t -> string list
val string_to_direction : string -> direction
val string_to_ship : string -> ship_type
val insert_ship : (int * int) -> direction -> ship_type -> player -> t -> action
val player_ships : player -> t -> tile array array
val print_player_ship_board : t -> player -> unit



(*
type stype = 
  | Battleship
  | AircraftCarrier
  | Destroyer
  | Cruiser
  | Submarine

type coordinate = int * int

type name = string

type player = Player1 | Player2

type t

val make_pregame_data : name -> name -> t
val get_ship_locations : t -> player -> (stype * coordinate list) list

*)