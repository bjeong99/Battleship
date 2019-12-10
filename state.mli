(* [grid_guess] is represents the whether a coordinate guessed on your
   opponent's grid was a hit or a miss. *)
type grid_guess =
  | Miss
  | Hit
  (* [player] is represents the player. *)
type player =
  | Player1
  | Player2

(* [t] is represents the state of the game *)
type t

(* [error] represents a mistake in targeting, for example,
   an error is [CoordinateVisited] if that location was visited previously,
   and [OutOfBounds] if it was not on the map *)
type error = 
  | CoordinateVisited
  | OutOfBounds

(* [action] represents if a targeting action can be carried out or not,
   and is either a [Success] of a new state, with a ship hit or not,
   and a ship sunk or not,
   or a [failure] with an exception type. *)
type action = 
  | Success of t * bool * bool
  | Failure of t * error

(* [ship_type] represents the type of the ship *)
type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

(** [powerup_type_type] represents the type of powerup. *)
type powerup_type =
  | SquareHit
  | ReHit
  | InstaKill

(* [RI]: Player2 is the only player that can be an AI player. *)
(* [AF]: In any array [arr], [arr.(row).(column)] represents the real world
   point [(column + 1, row + 1)] *)

(** [initialize_ai b1 b2 l1 l2] is the new ai that carries information
    about [b1] which represents player1 and [b2] as player2, and [l1], 
    the locations
    where player1 placed his ships and [l2], the locations where player2 placed
    his ships.  *)
val initialize_ai : bool -> bool -> Battleship.list_t -> Battleship.list_t -> t

(** [init_state b1 b2 l1 l2] is the state of the game,
    with [b1] as the first player and [b2] as the second player,
    and [l1] as the locations where Player1 placed his ships,
    and [l2] as the locations where Player2 placed his ships.  *)
val init_state : bool -> bool -> Battleship.list_t -> Battleship.list_t -> t

(** [print_boards p sl] is the game guesses for player [p] 
    printed to the screen row by row, with the 
    first element (row) of [sl] printed first.  *)
val print_guesses : player -> t -> unit

(** [bool_to_player b] is the Player1 if [b] is [true] 
    else Player2. *)
val bool_to_player : bool -> player

(** [print_player_dict p sl] is the where the ships for player [p] were placed
    printed to the screen row by row, with the 
    first element (row) of [sl] printed first.  *)
val print_player_dict : player -> t -> unit

(** [string_of_player_dict p st] is a list of strings consisting
    of the ship locations placed  by the player. The list is ordered by row,
    so that the first row represents the first element of the list
    and so forth. *)
val string_of_player_dict : player -> t -> string list

(** [string_of_guesses p st] is a list of strings consisting
    of the guesses made by the player. The list is ordered by row,
    so that the first row represents the first element of the list
    and so forth. *)
val string_of_guesses : player -> t -> string list

(** [combine_boards sl1 sl2] is combines the two lists of strings
    via the rule [[a1;b1;c1;...]] [[a2;b2;c2;...]] becomes
    [[a1 ^ a2; b1 ^ b2; c1 ^ c2...]]. 

    Requires : [sl1] and [sl2] are lists of player boards.

    Raises : ["lst2 longer than lst1"] if [sl2] length is greater than length 
    of[sl1]. 
    Raises : ["lst1 longer than lst2"] if [sl1] length is greater than length 
    of[sl2]. *)
val combine_boards : string list -> string list -> string list

(** [print_boards sl] is the game board printed to the screen 
    row by row, with the first element (row) of [sl] printed first.  *)
val print_boards : string list -> unit

(** [target_ai st] is the location that the easy AI targets,
    plus an updated state [st] that has the updates the ai on its targeting
    routine based on the hit/miss of its last target. 

    For easy AI, there is a trivial update to the targeting routine
    which just adds the location targeted to where it has already it. *)
val target_ai : t -> ((int * int) * t)

(** [target_medium_ai st] is the location that the medium AI targets,
    plus an updated state [st] that has the updates the ai on its targeting
    routine based on the hit/miss of its last target. 

    For medium  AI, the targeting route changes based on if there is a hit or 
    a miss. *)
val target_medium_ai : t -> ((int * int) * t)

(** [target_hard_ai st] is the location that the medium AI targets,
    plus an updated state [st] that has the updates the ai on its targeting
    routine based on the hit/miss of its last target. 

    For hard  AI, the targeting route changes based on if there is a hit or 
    a miss. *)
val target_hard_ai : t -> Hard_ai.point * t

(** [target_insane_ai st] is the location that the medium AI targets,
    plus an updated state [st] that has the updates the ai on its targeting
    routine based on the hit/miss of its last target. 

    For hard  AI, the targeting route changes based on if there is a hit or 
    a miss. *)
val target_insane_ai : t -> Hard_ai.point * t

(** [check_victory p st] is [true] if [p] has sunk all the ships
    of his/her opponent. This means that all five ships of the opposing
    player have hits on every location. *)
val check_victory : player -> t -> bool

(** [update_hard_ai st b1 b2 (x, y)] is the updated state with a new ai
    after the AI targets a location and the location [(x, y)] on the
    enemy board has been attacked. The AI is updated based on whether
    that attack at [(x, y)] was a hit or a miss or not, which js [b1]
    and whether a ship was sunk or not [b2] where [b1] is [true] iff
    a ship was hit and [b2] is [true] iff a ship was sunk. 

    Requires: [update_hard_ai] must be called every time after [target] 
    has been used by the ai.

    Raises : ["A target cannot sink a ship, yet not hit a ship"] if
    [ship_hit] is [true] but [ship_sunk] is [false].*)

val update_hard_ai : t -> bool -> bool -> Hard_ai.point -> t

(** [update_player st] is changes [st] so that the player who just moved
    will alternate so that the other player can move next. *)
val update_player : t -> t

(** [update_insane_ai st b1 b2 (x, y)] is the updated state with a new ai
    after the AI targets a location and the location [(x, y)] on the
    enemy board has been attacked. The AI is updated based on whether
    that attack at [(x, y)] was a hit or a miss or not, which js [b1]
    and whether a ship was sunk or not [b2] where [b1] is [true] iff
    a ship was hit and [b2] is [true] iff a ship was sunk. 

    Requires: [update_insane_ai] must be called every time after [target] 
    has been used by the ai.

    Raises : ["A target cannot sink a ship, yet not hit a ship"] if
    [ship_hit] is [true] but [ship_sunk] is [false].*)
val update_insane_ai : t -> bool -> bool -> Hard_ai.point -> t

(** [target_ship (x, y) p st] is the [action] corresponding to 
    [p] targeting location [(x, y)] on the enemy's grid. 
    Requires: [x] and [y] satisify that they are between 0 and 9 inclusive. *)
val target_ship : int * int -> player -> t -> action

(** [add_powerup player state name] adds the [name] of the powerup to the grid
    of [player] and changes state [state]. *)
val add_powerup : player -> t -> string -> t

(** [get_current_player st] is the player who will move on this turn,
    [true] for player1, [false] for player2.  *)
val get_current_player : t -> bool

(** [print_powerups player state] prints all the powerups the [player] owns in
    the specific [state]. *)
val print_powerups : player -> t -> unit

val get_player_powerups : player -> t -> string list

val update_powerup_state : player -> t -> string -> t

(** [get_surrounding_positions (x, y) state] are the surrounding positions
    on the board around the targeted location [(x, y)] in the current [state] 
    after [(x, y)] has been targeted and marked on pos targeted. 

    Requires: [(x, y)] have [x] and [y] in the 0 and 9 inclusive. 

    Requires: Must be called after target for the given AI. *)
val get_surrounding_positions : int * int -> t -> (int * int) list
