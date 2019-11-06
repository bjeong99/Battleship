type t

type tile_status = 
  | Unknown
  | Hit
  | Miss

type ship

val init_state : string -> string -> t
val update_player : t -> t

val place_ship : bool -> t -> t
val ships_found : bool -> t -> ship list


val update_player_guesses : t -> int -> int -> t
val get_player_guess : t -> int -> int -> tile_status
val get_current_player : t -> string
val get_next_player : t -> string
val update_victory : t -> bool
