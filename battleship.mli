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

type powerup_type =
  | SquareHit
  | ReHit
  | InstaKill

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

(** [ship_to_ship ship] returns the ship type corresponding to string [ship].*)
val string_to_ship : string -> ship_type

(** [ship_to_string ship] returns the string name of ship [ship]. *)
val ship_to_string : ship_type -> string

(** [pretty_print_ship_to_string ship] returns grammatically correct string 
    names of ship [ship]. *)
val pretty_print_ship_to_string : ship_type -> string

(** [string_to_direction str] returns the direction 
    type corresponding to string [str]. *)
val string_to_direction : string -> direction

(** [generate_num_lst start length operator acc] 
    returns a list of coordinates of
    the ship placement corresponding to its [start] coordinate and the [length]
     of the ship. *)
val generate_num_lst : 
  int -> int -> (int -> int -> int) -> int list -> int list

(** [generate_pos_list (x, y) length direction] returns a list of positions of
    the ship placement corresponding to its [start] coordinate and the [length]
     of the ship. *)
val generate_pos_list: 
  int * int -> int -> direction -> (int * int * status) list

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
    list generated by the head [(x, y)], length [length], 
    direction [direction] is 
    unoccupied in dictionary [dict]. *)
val check_unoccupied : int * int -> int -> direction -> list_t -> bool

(** [check_ship_placed ship player game] checks if player's [player] ship [ship]
    has been placed in game [game]. *)
val check_ship_placed : ship_type -> player -> t -> bool

(** [insert_ship (x, y) direction ship player dict] inserts a [ship] into
    dictionary [dict] depending on the head coordinate [(x, y)] 
    and the direction
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

(** [print_player_ship_board game player] prints the game board from [game] 
    specific to the [player]. *)
val print_player_ship_board : t -> player -> unit

(** [print_dict dict] prints the dictionary [dict]. *)
val print_dict : list_t -> unit

(** [string_of_dict dict] returns a string list representation of the dictionary
    [dict]. *)
val string_of_dict : list_t -> string list

(** [change_damage_list (x, y) list acc] changes the list string [list] of a 
    board at coordinates [(x, y)] to a damaged status. *)
val change_damage_list : int * int -> (int * int * status) list -> 
  (int * int * status) list -> (int * int * status) list

(** [change_to_damage (x, y) dict] changes the status at coordinate [(x, y)] in
    dict to Damaged. *)
val change_to_damage : int * int -> list_t -> list_t

(** [remaining_ships player game] returns the number of a [player] ships not 
    placed on the board in [game]. *)
val remaining_ships : player -> t -> int

(** [remaining_ships_to_place player game] returns the list of [player] shps
    not placed on the board in [game]. *)
val remaining_ships_to_place : player -> t -> string list

(** [check_ship_can_be_removed ship player game] checks if the [ship] on the 
    board of [player] in [game] can be removed from the board. *)
val check_ship_can_be_removed : string -> player -> t -> bool

(** [remove_ship ship player game] removes a [player]'s [ship] from their board
    in [game]. *) 
val remove_ship : string -> player -> t -> t

(** [check_all_ships_damaged dict] checks in [dict] if all ships have been 
    damaged. *)
val check_all_ships_damaged : list_t -> bool

(** [check_ship_sunk pos_list] checks if the list of coordinates [pos_list] are 
    all Damaged. *)
val check_ship_sunk : (int * int * status) list -> bool

(** [check_coordinate_in_positions (x, y) pos_list] checks if the coordinates 
    [(x, y)] is in the coordinate list [pos_list]. *)
val check_coordinate_in_positions : int * int -> (int * int * status) list 
  -> bool

(** [coordinate_to_ship_position (x, y) dict] checks 
    if the coordinates [(x, y)] is on a ship in dictionary [dict]. *)
val coordinate_to_ship_position : int * int -> list_t -> bool

(** [get_player_dict player game] gets a specific [player]'s dictionary in 
    [game] *)
val get_player_dict : player -> t -> list_t

(** [direction_list] is a string list of directions a ship can be oriented. *)
val direction_list : string list

(** [choose_target pairs_list] chooses a random target in list of coordinates
    [pairs_list]. *)
val choose_target : (int * int) list -> int * int

(** [create_pairs m n] *)
val create_pairs : int -> int -> (int * int) list

(** [ship_type_to_ship_name typ] is the corresponding lower case
    name for the ship type. *)
val ship_type_to_ship_name : ship_type -> string

(** [random_ship player game] will randomly place ONE ship for 
    the [player] and create a new game with 
    a ship randomly placed in a random location
    in a random location. 
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.

    Requires: The player has at least one ship remaining to place on 
    the board.*)
val random_ship : player -> t -> (int * int) * string * string

(** [randomly_laydownn_ships game] is the new game  
    with player2, the AI, having ONE random ship placed in a random
    location, in a random direction. 
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.

    Requires: The player has at least one ship remaining to place on 
    the board.*)
val randomly_laydown_ships : t -> (int * int) * string * string

(** [assign_powerups player game] assigns the [game] powerups
    based on [player]. 

    Requires: Apply on after all the ships for the player have been placed. *)
val assign_powerups : player -> t -> t

(** [print_power_ups p b] is the effect of printing the powerups
    [p] can get form game data [b]. *)
val print_power_ups : player -> t -> unit

(** [powerup_to_string p] the corrsponding string for the powerup [p]. *)
val powerup_to_string : powerup_type -> string

(** [check_coord_in_powerups x y p b] is [true] if [x][y]
    is a location that holds a powerup for player [p] in game data [b]. *)
val check_coord_in_powerups : int -> int -> player -> t -> bool

(** [get_powerup_name x y p b] is the name of the powerup
    at [x][y] for player [p] in game data [b]. *)
val get_powerup_name : int -> int -> player -> t -> string

(** [square_check_bounds (x, y)] is [true] iff 
    the coordinate corepsonding to [x][y] lies on the board
    and also, the square defined by a length unit two side 
    going right adn down on the board is also on the board fully. *)
val square_check_bounds : int*int -> bool

(** [get_ship_coordinates x y l] is the corresponding 
    new list [l] with all positions in [l] corresponding to the ship
    at [x][y] damaged and also the pairs of positions corresponding to 
    the ship given [x][y]. *)
val get_ship_coordinates : int -> int -> list_t -> list_t * ((int * int) list)