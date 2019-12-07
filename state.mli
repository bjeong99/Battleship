type grid_guess
type player
type t

type error = 
  | CoordinateVisited
  | OutOfBounds

type action = 
  | Success of t * bool * bool
  | Failure of t * error

type ship_type

val bool_to_player : bool -> player
val init_state : bool -> bool -> Battleship.list_t -> Battleship.list_t -> t
val update_player : t -> t
val get_current_player : t -> bool
val get_next_player : t -> bool

val target_ship : int * int -> player -> t -> action

(*val ships_found : t -> string list*)

val get_player_guess : int * int -> player -> t -> bool

val check_victory : player -> t -> bool

val string_of_guesses : player -> t -> string list
val string_of_player_dict : player -> t -> string list
val combine_boards : string list -> string list -> string list
val print_boards : string list -> unit
val print_guesses : player -> t -> unit
val print_player_dict : player -> t -> unit

val initialize_ai : bool -> bool -> Battleship.list_t -> Battleship.list_t -> t
val target_ai : t -> ((int * int) * t)

val target_medium_ai : t -> ((int * int) * t)
val target_hard_ai : t -> Hard_ai.point * t
val update_hard_ai : t -> bool -> bool -> Hard_ai.point -> t
val target_insane_ai : t -> Hard_ai.point * t
val update_insane_ai : t -> bool -> bool -> Hard_ai.point -> t

