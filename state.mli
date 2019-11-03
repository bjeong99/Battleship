type t

type tile_status = 
  | Unknown
  | Hit
  | Miss

val init_state : string -> string -> t
val update_player : t -> t
val update_player_guesses : t -> int -> int -> t
val get_player_guess : t -> int -> int -> tile_status
val get_current_player : t -> string
val get_next_player : t -> string
val update_victory : t -> bool