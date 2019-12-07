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

(** [strater_ship_list] is a list of ship_types. *)
val starter_ship_list : ship_type list

(** [empty] returns an empty instance of Battleship.t. *)
val empty: t 

(** [choose_player b] returns a player type depending on bool [b]. *)
val choose_player : bool -> player

(** [ship_length ship] returns the length depending on [ship]. *)
val ship_length: ship_type -> int

(** [ship_to_ship ship] returns the ship type corresponding to string [ship]. *)
val string_to_ship : string -> ship_type

(** [ship_to_string ship] returns the string name of ship [ship]. *)
val ship_to_string : ship_type -> string

(** [pretty_print_ship_to_string ship] returns grammatically correct string 
    names of ship [ship]. *)
val pretty_print_ship_to_string : ship_type -> string

(** [string_to_direction str] returns the direction type corresponding to string
    [str]. *)
val string_to_direction : string -> direction

(** [generate_num_lst start length operator acc] returns a list of coordinates of
    the ship placement corresponding to its [start] coordinate and the [length]
     of the ship. *)
val generate_num_lst : int -> int -> (int -> int -> int) -> int list -> int list

(** [generate_pos_list (x, y) length direction] returns a list of positions of
    the ship placement corresponding to its [start] coordinate and the [length]
     of the ship. *)
val generate_pos_list: int * int -> int -> direction -> (int * int * status) list

(** [insert ship (x,y) direction dict] inserts a [ship] into dictionary [dict] 
    depending on the head coordinate [(x, y)] and the direction [direction]. *)
val insert : string -> int * int -> direction -> list_t -> list_t

(** [remove ship dict] removes ship [ship] from dictionary [dict]. *)
val remove : string -> list_t
  -> (string * (int * int * status) list) list

(** [check_bounds (x, y) length direction] returns a bool value if the ship can 
    fit at the head coordinate [(x, y)] and its length [length] and direction 
    [direction]. *)
val check_bounds : int * int -> int -> direction -> bool

(** [check_cell_occupied (x, y) dict] checks if the coordinate [(x, y)] in 
    dictionary [dict] is occupied. *)
val check_cell_occupied : int * int -> list_t -> bool

(** [check_cell_unoccupied (x, y) dict] checks if the coordinate [(x, y)] in 
    dictionary [dict]is unoccupied. *)
val check_cell_unoccupied : int * int -> list_t -> bool

(** [check_unoccupied (x, y) length direction dict] checks if the coordinate 
    list generated by the head [(x, y)], length [length], direction [direction] is 
    unoccupied in dictionary [dict]. *)
val check_unoccupied : int * int -> int -> direction -> list_t -> bool

(** [check_ship_placed ship player game] checks if player's [player] ship [ship]
    has been placed in game [game]. *)
val check_ship_placed : ship_type -> player -> t -> bool

(** [insert_ship (x, y) direction ship player dict] inserts a [ship] into
    dictionary [dict] depending on the head coordinate [(x, y)] and the direction
    [direction] and the player [player].*)
val insert_ship : int * int -> direction -> ship_type -> player -> t -> action

(** [empty_board ()] returns a matrix of water wave emojis. *)
val empty_board : unit -> string array array

(** [make_grid dict grid] makes a grid [grid] based off dictionary [dict]. *)
val make_grid : list_t -> string array array -> string array array

(** [string_of_matrix matrix] returns a string list representation of the matrix
    [matrix]. *)
val string_of_matrix : string array array -> string list

(** [combine_boards lst1 lst2] combines string lists [lst1] and [lst2]. *)
val combine_boards : string list -> string list -> string list

(** [print_boards board_list] prints the string list [board_list] representing 
    each line of the board. *)
val print_boards : string list -> unit

(** [print_matrix matrix] prints the matrix [matrix] that represents the board
    and the axes. *)
val print_matrix : string array array -> unit

(** [print_player_ship_board game player] *)
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
