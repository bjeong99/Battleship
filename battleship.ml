
let c_ROWS = 10
let c_COLS = 10

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

type player = 
  | Player1
  | Player2

type ship_dictionary = (string * (int * int) list) list

(** Rep Invariant is y is rows, x is columns, rows first, columns second. *)
type t = {
  player_1_ships_remaining : ship_type list;
  player_1_ships : tile array array;
  player_1_ship_dict : ship_dictionary;
  player_2_ships_remaining : ship_type list;
  player_2_ships : tile array array;
  player_2_ship_dict : ship_dictionary;
}

let starter_ship_list = [
  AircraftCarrier ;
  Battleship ;
  Destroyer ;
  Cruiser ;
  Submarine ;
]

let initialize_pregame () = {
  player_1_ships_remaining = starter_ship_list;
  player_1_ships = Array.make_matrix c_ROWS c_COLS Empty;
  player_1_ship_dict = [];
  player_2_ships_remaining = starter_ship_list;
  player_2_ships = Array.make_matrix c_ROWS c_COLS Empty;
  player_2_ship_dict = [];
}

let choose_player player = 
  if player then Player1 else Player2

let rec remove_first_instance_ship lst ship = 
  match lst with
  | [] -> []
  | h :: t -> 
    if h = ship then t else h :: remove_first_instance_ship t ship

let remaining_ships player game = 
  (match player with
   | Player1 ->
     game.player_1_ships_remaining
   | Player2 ->
     game.player_2_ships_remaining)
  |> List.length

let ship_placement_complete player game = 
  remaining_ships player game = 0

let ship_to_string ship = 
  match ship with
  | Battleship  -> "Battleship"
  | AircraftCarrier  -> "Aircraft Carrier"
  | Destroyer -> "Destroyer"
  | Cruiser  -> "Cruiser"
  | Submarine  -> "Submarine"

let string_to_ship ship = 
  let shiplow = String.lowercase_ascii ship in 
  if shiplow = "battleship" then Battleship
  else if shiplow = "aircraftcarrier" then AircraftCarrier
  else if shiplow = "destroyer" then Destroyer
  else if shiplow = "cruiser" then Cruiser
  else if shiplow = "submarine" then Submarine
  else failwith "not legal ship"

let remaining_ships_to_place player game = 
  (match player with
   | Player1 ->
     game.player_1_ships_remaining
   | Player2 ->
     game.player_2_ships_remaining)
  |> List.map ship_to_string

let remove_ship player ship game = 
  match player with
  | Player1 ->
    {game with 
     player_1_ships_remaining = 
       remove_first_instance_ship game.player_1_ships_remaining ship}
  | Player2 ->
    {game with 
     player_2_ships_remaining = 
       remove_first_instance_ship game.player_2_ships_remaining ship}

let rec bulk_add_ships player game pos_list = 
  match pos_list with
  | [] -> ()
  | (x, y, ship) :: t -> begin
      match player with
      | Player1 ->  
        game.player_1_ships.(y).(x) <- Ship (ship, ShipSafe);
        bulk_add_ships player game t
      | Player2 ->
        game.player_2_ships.(y).(x) <- Ship (ship, ShipSafe);
        bulk_add_ships player game t
    end

type direction = 
  | Left
  | Right
  | Up
  | Down

let string_to_direction str = 
  let strlow = String.lowercase_ascii str in 
  if strlow = "left" then Left
  else if strlow = "right" then Right
  else if strlow = "up" then Up
  else if strlow = "down" then Down
  else failwith "str not one of four cases"

let ship_length ship = 
  match ship with
  | Battleship -> 4
  | AircraftCarrier -> 5
  | Destroyer  -> 3
  | Cruiser  -> 3
  | Submarine  -> 2

let rec generate_num_lst start length operator acc = 
  if length = 0 then List.rev acc
  else generate_num_lst ((operator) start 1) (length - 1) operator (start :: acc)

let generate_pos_list (x, y) length direction = 
  match direction with
  | Left -> 
    [] |> generate_num_lst x length (-) |> List.map (fun x -> (x, y)) 
  | Right -> 
    [] |> generate_num_lst x length (+) |> List.map (fun x -> (x, y)) 
  | Down -> 
    [] |> generate_num_lst y length (+) |> List.map (fun y -> (x, y))
  | Up ->  
    [] |> generate_num_lst y length (-) |> List.map (fun y -> (x, y)) 

let check_bounds (x, y) length direction = 
  match direction with
  | Left -> 
    x - length + 1 >= 0 && x < 10 && y >= 0 && y < 10 
  | Right -> 
    x >= 0 && x + length - 1 < 10 && y >= 0 && y < 10 
  | Up ->  
    x >= 0 && x < 10 && y - length + 1 >= 0 && y < 10  
  | Down -> 
    x >= 0 && x < 10 && y >= 0 && y + length - 1 < 10

let check_cell_unoccupied (x, y) player game = 
  match player with
  | Player1 ->
    game.player_1_ships.(y).(x) = Empty
  | Player2 -> 
    game.player_2_ships.(y).(x) = Empty

let check_unoccupied (x, y) length direction player game = 
  (match direction with
   | Left -> 
     [] |> generate_num_lst x length (-) |> List.map (fun x -> (x, y)) 
   | Right -> 
     [] |> generate_num_lst x length (+) |> List.map (fun x -> (x, y)) 
   | Down -> 
     [] |> generate_num_lst y length (+) |> List.map (fun y -> (x, y))
   | Up ->  
     [] |> generate_num_lst y length (-) |> List.map (fun y -> (x, y)))
  |> List.for_all (fun (x, y) -> check_cell_unoccupied (x, y) player game )

let check_ship_placed ship player game = 
  match player with
  | Player1 ->
    List.mem ship game.player_1_ships_remaining 
  | Player2 ->
    List.mem ship game.player_2_ships_remaining 

type error = 
  | BoundsError
  | OccupiedTile
  | OutOfShips 

type action = 
  | Success of t
  | Failure of t * error

let print_int_pair (x, y) = 
  "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"

let rec print_pairs_list lst string_acc= 
  match lst with 
  | [] -> string_acc
  | h :: t -> (print_int_pair h) ^ string_acc

let rec print_ship_pos_dict dict string_acc= 
  match dict with
  | [] -> string_acc
  | (ship, pairs_list) :: t ->
    string_acc ^ "( " ^ ship ^ (print_pairs_list pairs_list "") ^ " )"

let add_to_ship_dictionary ship_name ship_positions player game = 
  match player with
  | Player1 ->
    print_endline (print_ship_pos_dict game.player_1_ship_dict "");
    {game with player_1_ship_dict = (ship_name, ship_positions) :: game.player_1_ship_dict}
  | Player2 ->
    print_endline (print_ship_pos_dict game.player_2_ship_dict "");
    {game with player_2_ship_dict = (ship_name, ship_positions) :: game.player_2_ship_dict}




let insert_ship (x, y) direction ship player game = 
  let length = ship_length ship in 
  if not (check_bounds (x, y) length direction)
  then Failure (game, BoundsError)
  else if not (check_unoccupied (x, y) length direction player game)
  then Failure (game, OccupiedTile)
  else if not (check_ship_placed ship player game) 
  then Failure (game, OutOfShips)
  else 
    let ship_positions = 
      generate_pos_list (x, y) length direction 
    in bulk_add_ships player game (ship_positions |> List.map (fun (x, y) -> (x, y, ship)) );
    Success (game |> remove_ship player ship |> add_to_ship_dictionary (ship_to_string ship) ship_positions player)


let player_ships player game = 
  match player with
  | Player1 ->
    game.player_1_ships
  | Player2 -> 
    game.player_2_ships

let check_hit (x, y) player game = 
  match player with
  | Player1 -> begin
      match game.player_1_ships.(y).(x) with
      | Ship (_, ShipDamaged) -> true
      | _ -> false
    end
  | Player2 -> begin
      match game.player_2_ships.(y).(x) with
      | Ship (_, ShipDamaged) -> true
      | _ -> false
    end

let print_player_ship_board game player =
  let matrix = 
    match player with
    | Player1 -> game.player_1_ships
    | Player2 -> game.player_2_ships
  in
  for i = 0 to (c_ROWS - 1) do
    let s = ref "" in 
    for j = 0 to (c_COLS - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Empty -> "_"
        | Ship (Battleship, ShipSafe) -> "B"
        | Ship (Battleship, ShipDamaged) -> "b"
        | Ship (AircraftCarrier, ShipSafe) -> "A"
        | Ship (AircraftCarrier, ShipDamaged) -> "a"
        | Ship (Destroyer, ShipSafe) -> "D"
        | Ship (Destroyer, ShipDamaged) -> "d"
        | Ship (Cruiser, ShipSafe) -> "C"
        | Ship (Cruiser, ShipDamaged) -> "c"
        | Ship (Submarine, ShipSafe) -> "S"
        | Ship (Submarine, ShipDamaged)-> "s"
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done





(*




let cHIT_CHAR = "X"
let cMISS_CHAR = "O"
let cUNKNOWN_CHAR  = "?"

type hit = 
  | Hit
  | Miss
  | Unknown

type orientation =
  | Vertical
  | Horizontal

(** types are defiend as part of the historical game *)
type stype = 
  | Battleship
  | AircraftCarrier
  | Destroyer
  | Cruiser
  | Submarine

exception OutofBounds
exception OccupiedBox
exception OutofShips
exception InvalidOrientation

(**type [ship] is defined by [name]: string as the name of the ship, 
   [hits]: tuple of int * int, as the first one defining hits received and the 
   second as the length of the ship, and 
   [stype] as stype to define the type of the ship *)
type ship = {
  name: string; 
  hits: int * int; 
  stype: stype; 
  placed: bool}

let ship_length ship = 
  match ship.stype with
  | Battleship -> 4
  | AircraftCarrier -> 5
  | Destroyer -> 3
  | Cruiser -> 3
  | Submarine -> 2

let ship_number ship = 
  match ship.stype with
  | Battleship -> 2
  | AircraftCarrier -> 1
  | Destroyer -> 2
  | Cruiser -> 2
  | Submarine -> 3

(**[init_ships] is the list of ships that the players have at the beginning
   of the game, each player have two battleships, one aircraft carrier, two
   destroyers, two cruisers and one submarine*)
let init_ships =
  let bs1 = {name = "Battleship_1"; hits = (0 , 4); stype = Battleship; 
             placed = false}; in 
  let bs2 = {name = "Battleship_2"; hits = (0 , 4); stype = Battleship; 
             placed = false}; in 
  let ac = {name = "Air_Craft_Carrier"; hits = (0 , 5); stype = AircraftCarrier; 
            placed = false}; in 
  let d1 = {name = "Destroyer_1"; hits = (0 , 3); stype = Destroyer; 
            placed = false}; in 
  let d2 = {name = "Destroyer_2"; hits = (0 , 3); stype = Destroyer; 
            placed = false}; in 
  let c1 = {name = "Cruiser_1"; hits = (0 , 3); stype = Cruiser; 
            placed = false}; in 
  let c2 = {name = "Cruiser_2"; hits = (0 , 3); stype = Cruiser; 
            placed = false}; in 
  let sm1 = {name = "Submarine_1"; hits = (0 , 2); stype = Submarine; 
             placed = false}; in 
  let sm2 = {name = "Submarine_2"; hits = (0 , 2); stype = Submarine; 
             placed = false}; in 
  let sm3 = {name = "Submarine_3"; hits = (0 , 2); stype = Submarine; 
             placed = false}; in

  bs1::bs2::ac::d1::d2::c1::c2::sm1::sm2::sm3::[]

(** [check_placement] checks the list of [init_ships]*)
let rec check_placement init_ships =
  match init_ships with
  |[] -> true
  |h::t -> if h.placed = true then check_placement t else false

type tile = 
  | Occupied of ship
  | Unoccupied

let player_1_matrix = Array.make_matrix n_rows n_cols Unoccupied
let player_1_guesses = Array.make_matrix n_rows n_cols Unknown
let player_2_matrix = Array.make_matrix n_rows n_cols Unoccupied
let player_2_guesses = Array.make_matrix n_rows n_cols Unknown

type player = 
  | Player1
  | Player2

let print_guesses_matrix matrix =
  for i = 1 to (n_rows - 1) do
    let s = ref "" in 
    for j = 1 to (n_cols - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Hit -> cHIT_CHAR
        | Miss -> cMISS_CHAR
        | Unknown -> cUNKNOWN_CHAR  
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done

let mem matrix x y = 
  match matrix.(x).(y) with
  | Unoccupied -> None
  | Occupied s -> Some s

(* fix this *)
let insert_horizontal matrix head_loc ship =
  matrix.(head_loc).(ship_length ship) <- Occupied ship

(* fix this *)
let insert_vertical matrix head_loc ship =
  matrix.(head_loc).(ship_length ship) <- Occupied ship

let get_orientation orientation= 
  match orientation with
  | "h" -> Horizontal
  | "v" -> Vertical
  | _ -> raise InvalidOrientation


(** make function to show what ship is at that location*)
(**where all ships are placed, at each location at in the array shows what ship there is in that location *)

(**TODO: check if occupied or not before placing*)
let insert matrix head_loc orientation ship = 
  match (get_orientation orientation) with
  | Vertical -> insert_vertical matrix head_loc ship
  | Horizontal -> insert_horizontal matrix head_loc ship

let search_guesses matrix x y = 
  matrix.(x).(y)

let modify_guesses matrix x y hit_status = 
  matrix.(x).(y) <- hit_status

let check_bounds x0 x1 y0 y1 = 
  let coordinate_lst = [x0; x1; y0; y1] in 
  coordinate_lst |> List.for_all (fun elt -> elt < n_rows && elt >= 0) &&
  (x0 < x1) && (y0 < y1)

(**only write head and get the legth from ship type *)
let check_hor_vert x0 x1 y0 y1 = 
  (x0 - x1) = 0 || (y0 - y1) = 0

let get_coordinate_length x0 x1 y0 y1 = 
  if (x0 - x1) = 0 then (y1 - y0)
  else (x1 - x0)

let check_correct_length x0 x1 y0 y1 ship = 
  ship_length ship = get_coordinate_length x0 x1 y0 y1

type ship_orientation = 
  | Horizontal of int * int * int
  | Vertical of int * int * int

let det_orientation x0 x1 y0 y1 = 
  if (x0 - x1) = 0 then Horizontal (y0, y1, x0)
  else Vertical (x0, x1, y0)

let rec (--) starting ending = 
  if starting = ending then [ending]
  else starting :: (--) (starting + 1) (ending)

let check_unoccupied matrix orientation = 
  match orientation with
  | Horizontal (x0, x1, y) -> 
    (--) x0 x1 |> List.for_all (fun x -> matrix.(x).(y) = Unoccupied)
  | Vertical (y0, y1, x) -> 
    (--) y0 y1 |> List.for_all (fun y -> matrix.(x).(y) = Unoccupied)

let place_ship ship matrix x0 x1 y0 y1 = 
  match ship with
  | Battleship -> 
  | AircraftCarrier -> 
  | Destroyer -> 
  | Cruiser -> 
  | PTBoat -> 
  | Submarine ->
*)
