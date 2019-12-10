open Emoji

let c_ROWS = 10
let c_COLS = 10

let c_BOARD_SEP = "    "

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

type powerup_type =
  | SquareHit
  | ReHit
  | InstaKill

type status = 
  | Damaged
  | Undamaged

type player = 
  | Player1
  | Player2

type list_t = (string * ((int * int * status) list) ) list

type t = {
  player_1_ships_placed : ship_type list;
  player_1_ships_remaining : ship_type list;
  player_1_ships : (string * ((int * int * status) list) ) list;
  player_2_ships_placed : ship_type list;
  player_2_ships_remaining : ship_type list;
  player_2_ships : (string * ((int * int * status) list) ) list;
  player_1_powerups : (int * int * powerup_type) list;
  player_2_powerups : (int * int * powerup_type) list;
}

type error = 
  | BoundsError
  | OccupiedTile
  | OutOfShips
  | NonexistShip

type action = 
  | Success of t
  | Failure of t * error


let starter_ship_list = [
  AircraftCarrier ;
  Battleship ;
  Destroyer ;
  Cruiser ;
  Submarine ;
]

let powerup_list = [
  SquareHit ;
  ReHit ;
  InstaKill ;
]

let empty = {
  player_1_ships_placed = [];
  player_1_ships_remaining = starter_ship_list;
  player_1_ships = [];
  player_2_ships_placed = [];
  player_2_ships_remaining = starter_ship_list;
  player_2_ships = [];
  player_1_powerups = [];
  player_2_powerups = [];
}

let choose_player b = 
  if b then Player1 else Player2

let ship_length ship = 
  match ship with
  | AircraftCarrier -> 5
  | Battleship -> 4
  | Destroyer  -> 3
  | Cruiser  -> 3
  | Submarine  -> 2

let string_to_powerup powerup =
  let powerlow = String.lowercase_ascii powerup in 
  if powerlow = "squarehit" then SquareHit
  else if powerlow = "rehit" then ReHit
  else if powerlow = "instakill" then InstaKill
  else failwith "not legal powerup"

let powerup_to_string powerup =
  match powerup with
  | SquareHit -> "squarehit"
  | ReHit -> "rehit"
  | InstaKill -> "instakill"

let string_to_ship ship = 
  let shiplow = String.lowercase_ascii ship in 
  if shiplow = "battleship" then Battleship
  else if shiplow = "aircraftcarrier" then AircraftCarrier
  else if shiplow = "destroyer" then Destroyer
  else if shiplow = "cruiser" then Cruiser
  else if shiplow = "submarine" then Submarine
  else failwith "not legal ship"

let ship_to_string ship = 
  match ship with
  | AircraftCarrier -> "aircraftcarrier"
  | Battleship -> "battleship"
  | Destroyer  -> "destroyer"
  | Cruiser  -> "cruiser"
  | Submarine  -> "submarine"

let pretty_print_ship_to_string ship = 
  match ship with
  | AircraftCarrier -> "Aircraft Carrier"
  | Battleship -> "Battleship"
  | Destroyer  -> "Destroyer"
  | Cruiser  -> "Cruiser"
  | Submarine  -> "Submarine"


let string_to_direction str = 
  let strlow = String.lowercase_ascii str in 
  if strlow = "left" then Left
  else if strlow = "right" then Right
  else if strlow = "up" then Up
  else if strlow = "down" then Down
  else failwith "str not one of four cases"

(*
let rec get_ship_list dict acc_key= 
  match dict with 
  | [] -> acc_key
  | (ship, lst) :: t -> get_ship_list t (ship :: acc_key)


let rec get_coord x y lst = 
  match lst with 
  | [] -> false
  | (a, b, _) :: t -> if a = x && b = y then true
    else get_coord x y t

let rec get_ship_from_coord x y dict = 
  match dict with 
  | [] -> raise NonexistShip 
  | (k, lst) :: t -> if (get_coord x y lst) then k else get_ship_from_coord x y t
*)

let rec generate_num_lst start length operator acc = 
  if length = 0 then List.rev acc
  else generate_num_lst ((operator) start 1) (length - 1) operator (start :: acc)

let generate_pos_list (x, y) length direction = 
  match direction with
  | Left -> 
    [] |> generate_num_lst x length (-) |> List.map (fun x -> (x, y, Undamaged)) 
  | Right -> 
    [] |> generate_num_lst x length (+) |> List.map (fun x -> (x, y, Undamaged)) 
  | Down -> 
    [] |> generate_num_lst y length (+) |> List.map (fun y -> (x, y, Undamaged))
  | Up ->  
    [] |> generate_num_lst y length (-) |> List.map (fun y -> (x, y, Undamaged)) 

let insert ship (x, y) direction dict = 
  let ship_positions = generate_pos_list (x, y) 
      (ship_length (string_to_ship ship)) direction in   
  (ship, ship_positions) :: dict

let remove ship dict = 
  List.filter (fun (ship_elt, _ ) -> ship <> ship_elt) dict

let insert_pup powerup (x,y) dict =
  let pup_positions = generate_pos_list (x,y) 1 Right in 
  let pp = pup_positions |> List.map (fun (x,y,z) -> (x,y)) in 
  (powerup, pp) :: dict

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

let square_check_bounds (x,y) =
  check_bounds (x,y) 2 Down &&
  check_bounds (x,y) 2 Right  

let check_cell_occupied (x, y) dict = 
  List.fold_left (fun init (key, values_list) -> values_list @ init) [] dict |> 
  List.map (fun (x,y,z) -> (x,y)) |> List.mem (x, y)

let check_cell_unoccupied (x, y) dict = 
  not (check_cell_occupied (x, y) dict)

let check_unoccupied (x, y) length direction dict = 
  (match direction with
   | Left -> 
     [] |> generate_num_lst x length (-) |> List.map (fun x -> (x, y)) 
   | Right -> 
     [] |> generate_num_lst x length (+) |> List.map (fun x -> (x, y)) 
   | Down -> 
     [] |> generate_num_lst y length (+) |> List.map (fun y -> (x, y))
   | Up ->  
     [] |> generate_num_lst y length (-) |> List.map (fun y -> (x, y)))
  |> List.for_all (fun (x, y) -> check_cell_unoccupied (x, y) dict)

let check_ship_placed ship player game = 
  match player with
  | Player1 ->
    List.mem ship game.player_1_ships_remaining 
  | Player2 ->
    List.mem ship game.player_2_ships_remaining 

let insert_ship (x, y) direction ship player dict = 
  match player with
  | Player1 -> 
    if check_bounds (x, y) (ship_length ship) direction 
    then begin
      if check_ship_placed ship player dict then 
        begin if check_unoccupied (x, y) (ship_length ship) direction dict.player_1_ships
          then Success 
              {dict with
               player_1_ships_placed = ship :: dict.player_1_ships_placed;
               player_1_ships = insert (ship_to_string ship) (x, y) direction dict.player_1_ships;
               player_1_ships_remaining = List.filter (fun elt -> elt <> ship) dict.player_1_ships_remaining} 
          else Failure (dict, OccupiedTile)
        end
      else Failure (dict, OutOfShips)
    end
    else Failure (dict, BoundsError)
  | Player2 ->
    if check_bounds (x, y) (ship_length ship) direction 
    then 
      begin
        if check_ship_placed ship player dict then 
          begin if check_unoccupied (x, y) (ship_length ship) direction dict.player_2_ships
            then Success 
                {dict with 
                 player_2_ships_placed = ship :: dict.player_2_ships_placed;
                 player_2_ships = insert (ship_to_string ship) (x, y) direction dict.player_2_ships;
                 player_2_ships_remaining = List.filter (fun elt -> elt <> ship) dict.player_2_ships_remaining} 
            else Failure (dict, OccupiedTile)
          end
        else Failure (dict, OutOfShips)
      end
    else Failure (dict, BoundsError)

let empty_board () = 
  Array.make_matrix 10 10 Emoji.water_wave

let rec make_grid dict grid = 
  (* TODO  add powerups*)
  match dict with
  | [] -> grid
  | (string, list_of_ints) :: t ->
    let first_letter = String.get (String.uppercase_ascii string) 0 |> Char.escaped in
    List.map (fun (x, y, z) -> grid.(y).(x) <- 
                 (if z = Undamaged then (first_letter ^ " ") else Emoji.fire)) 
      list_of_ints |> ignore;
    make_grid t grid

(* OLD VERSION: NUMBERS DISPLAYED IN OPPOSITE ORDER
   let rec make_grid dict grid = 
   (* TODO  add powerups*)
   match dict with
   | [] -> grid
   | (string, list_of_ints) :: t ->
    let first_letter = String.get (String.uppercase_ascii string) 0 |> Char.escaped in
    List.map (fun (x, y, z) -> grid.(y).(x) <- 
                 (if z = Undamaged then (first_letter ^ " ") else Emoji.fire)) 
      list_of_ints |> ignore;
    make_grid t grid

*)

let string_of_matrix matrix = 
  let init_list = ref [] in 
  for i = 0 to (c_ROWS - 1) do
    let s = ref "" in 
    for j = 0 to (c_COLS - 1) do 
      s := !s ^ " " ^ matrix.(i).(j) 
    done;
    init_list := !init_list @ [!s];
  done;
  !init_list

(* OLDER INCORRECT VERSION: PUTS ROWS AT BOTTOM ON TOP
   let string_of_matrix matrix = 
   let init_list = ref [] in 
   for i = 0 to (c_ROWS - 1) do
    let s = ref "" in 
    for j = 0 to (c_COLS - 1) do 
      s := !s ^ " " ^ matrix.(i).(j) 
    done;
    init_list := !s :: !init_list;
   done;
   !init_list
*)

let combine_boards lst1 lst2 = 
  let rec combine_helper lst1 lst2 acc =  
    match lst1, lst2 with
    | [], [] -> acc
    | [], _ -> failwith "lst2 longer than lst1"
    | _, [] -> failwith "lst1 longer than lst2"
    | h1 :: t1, h2 :: t2 -> 
      combine_helper t1 t2 ((h1 ^ c_BOARD_SEP ^ h2) :: acc)
  in combine_helper lst1 lst2 []

let rec print_boards board_list = 
  match board_list with
  | [] -> ()
  | h :: t -> 
    print_endline h; 
    print_boards t

let print_matrix matrix = 
  let axis_x = ref "    A  B  C  D  E  F  G  H  I  J\n" in 
  print_endline !axis_x;
  for i = 0 to (c_ROWS - 1) do
    let s = 
      begin
        match i with 
        | 9 -> ref ((string_of_int (i + 1)) ^ " ");
        | _ -> ref (" " ^ (string_of_int (i + 1)) ^ " ");
      end
    in
    for j = 0 to (c_COLS - 1) do 
      s := !s ^ " " ^ matrix.(i).(j)
    done;
    print_endline !s;
  done

let print_player_ship_board game player =
  match player with
  | Player1 ->
    let grid = make_grid game.player_1_ships (empty_board ()) in 
    print_matrix grid
  | Player2 -> 
    let grid = make_grid game.player_2_ships (empty_board ()) in 
    print_matrix grid

let print_dict dict = 
  make_grid dict (empty_board ()) |> print_matrix

let string_of_dict dict = 
  make_grid dict (empty_board ()) |> string_of_matrix

let rec change_damage_list (x, y) list acc =
  match list with
  | [] -> acc
  | (a, b, status) :: t -> 
    if a = x && b = y then change_damage_list (x, y) t ((a, b, Damaged) :: acc)
    else change_damage_list (x, y) t ((a, b, status) :: acc)


let change_to_damage (x, y) dict = 
  let rec change_to_damage_helper (x, y) dict acc = 
    match dict with
    | [] -> acc
    | (ship, lst) :: t ->
      change_to_damage_helper (x, y) t ((ship, change_damage_list (x, y) lst []) :: acc)
  in change_to_damage_helper (x, y) dict []


let remaining_ships player game = 
  match player with
  | Player1 -> List.length game.player_1_ships_remaining
  | Player2 -> List.length game.player_2_ships_remaining

let remaining_ships_to_place player game = 
  (match player with
   | Player1 ->
     game.player_1_ships_remaining
   | Player2 ->
     game.player_2_ships_remaining)
  |> List.map pretty_print_ship_to_string


let check_ship_can_be_removed ship player game = 
  (match player with
   | Player1 ->
     game.player_1_ships_placed
   | Player2 ->
     game.player_2_ships_placed)
  |> List.map ship_to_string
  |> List.mem ship

let remove_ship_helper ship player game = 
  match player with
  | Player1 -> {
      game with
      player_1_ships = remove ship game.player_1_ships;
      player_1_ships_placed = 
        game.player_1_ships_placed |> List.filter (fun elt -> elt <> string_to_ship ship);
      player_1_ships_remaining =
        (string_to_ship ship) :: game.player_1_ships_remaining}
  | Player2 -> {
      game with
      player_2_ships = remove ship game.player_2_ships;
      player_2_ships_placed = 
        game.player_2_ships_placed |> List.filter (fun elt -> elt <> string_to_ship ship);
      player_2_ships_remaining =
        (string_to_ship ship) :: game.player_2_ships_remaining}

let remove_ship ship player game = 
  if not (check_ship_can_be_removed ship player game) then game
  else 
    let () = print_endline ship in
    remove_ship_helper ship player game

let check_all_ships_damaged dict = 
  dict 
  |> List.fold_left (fun init (name, pos_list) -> pos_list :: init) [] 
  |> List.flatten 
  |> List.for_all (fun (x, y, status) -> status = Damaged)

let check_ship_sunk pos_list = 
  pos_list |> List.for_all (fun (x, y, status) -> status = Damaged)

let rec check_coordinate_in_positions (x, y) pos_list = 
  match pos_list with
  | [] -> false
  | (x', y', status) :: t ->
    if x' = x && y' = y then true else check_coordinate_in_positions (x, y) t

let rec coordinate_to_ship_position (x, y) dict = 
  match dict with
  | [] -> failwith "(x, y) coordinate was not on a ship"
  | (name, pos_list) :: t ->
    if check_coordinate_in_positions (x, y) pos_list 
    then check_ship_sunk pos_list
    else coordinate_to_ship_position (x, y) t

let get_player_dict player game = 
  match player with
  | Player1 -> game.player_1_ships
  | Player2 -> game.player_2_ships

let direction_list = ["up"; "down"; "left"; "right"]

let choose_target pairs_list = 
  let () = Random.self_init () in 
  pairs_list 
  |> List.length 
  |> Random.int 
  |> List.nth pairs_list

let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
  in create_rows m n []

(** [ship_type_to_ship_name ship] convents [ship] to
    its standardized string name, which is the same
    as the command a player enters into the terminal. *)
let ship_type_to_ship_name ship = 
  match ship with
  | AircraftCarrier -> "aircraftcarrier"
  | Battleship -> "battleship"
  | Destroyer  -> "destroyer"
  | Cruiser  -> "cruiser"
  | Submarine  -> "submarine"

(** [randomly_laydown_ships_helper player game] is the new game
    with ONE randomly chosen ship placed in a random location in 
    a random direction for the indicated [player].
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.

    Requires: The player has at least one ship remaining to place on 
    the board. *)
let randomly_laydown_ships_helper player game = 
  match player with
  | Player1 ->
    let () = Random.self_init () in 
    let remaining_ships_num = List.length game.player_1_ships_remaining in 
    let random_ship = List.nth (game.player_1_ships_remaining |> List.map ship_type_to_ship_name) (Random.int remaining_ships_num) in 
    let random_direction = List.nth direction_list (Random.int (List.length direction_list)) in 
    let pairs_list = create_pairs c_ROWS c_COLS in 
    let random_position = choose_target pairs_list in 
    (random_position, random_direction, random_ship)
  | Player2 -> 
    let () = Random.self_init () in 
    let remaining_ships_num = List.length game.player_2_ships_remaining in 
    let random_ship = List.nth (game.player_2_ships_remaining |> List.map ship_type_to_ship_name) (Random.int remaining_ships_num) in 
    let random_direction = List.nth direction_list (Random.int (List.length direction_list)) in 
    let pairs_list = create_pairs c_ROWS c_COLS in 
    let random_position = choose_target pairs_list in 
    (random_position, random_direction, random_ship)

(** [random_ship player game] will randomly place ONE ship for 
    the [player] and create a new game with 
    a ship randomly placed in a random location
    in a random location. 
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.

    Requires: The player has at least one ship remaining to place on 
    the board.*)
let random_ship player game = 
  randomly_laydown_ships_helper player game

(** [randomly_laydownn_ships game] is the new game  
    with player2, the AI, having ONE random ship placed in a random
    location, in a random direction. 
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.

    Requires: The player has at least one ship remaining to place on 
    the board.*)
let randomly_laydown_ships game = 
  randomly_laydown_ships_helper Player2 game

let string_of_pair (x, y) = 
  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let print_pairs_list lst = 
  List.map string_of_pair lst |> List.map print_endline |> ignore

(** [get_unvisited_loc dict] is all unvisited coordianteds based
    on [dict]. *)
let get_unvisited_loc dict = 
  let coord_list = 
    dict |> List.map (fun (s, lst) -> lst) 
    |> List.flatten |> List.map (fun (x, y, ship) -> (x, y)) in
  let full_list = create_pairs 10 10 in 
  full_list 
  |> List.map (fun (x, y) -> (x - 1), (y - 1))
  |> List.filter (fun (x, y) -> not (List.mem (x, y) coord_list))


(** [get_three_random_helper unvisited counter acc] is three random
    coordinates from [unvisited]. *)
let rec get_three_random_helper unvisited counter acc = 
  if counter = 0 then acc 
  else 
    let length = List.length unvisited in 
    let point = List.nth unvisited (Random.int length) in 
    let new_list = List.filter (fun (x, y) -> (x, y) <> point) unvisited in 
    get_three_random_helper new_list (counter - 1) (point :: acc)

(** [assign_three_random_emojis dict] assigns three emojis to
    the a random set of three coordinates. *)
let assign_three_random_emojis dict = 
  let unvisited = get_unvisited_loc dict in 
  let three_random = get_three_random_helper unvisited 3 [] in 
  List.map2 (fun (x, y) power -> (x, y, power)) three_random powerup_list

(** [assign_powerups player game] assigns the [game] powerups
    based on [player]. 

    Requires: Apply on after all the ships for the player have been placed. *)
let assign_powerups player game = 
  match player with 
  | Player1 -> 
    {game with 
     player_1_powerups = assign_three_random_emojis game.player_1_ships}
  | Player2 ->
    {game with 
     player_2_powerups = assign_three_random_emojis game.player_2_ships}

let empty_powerup () = 
  Array.make_matrix 10 10 Emoji.water_wave

let rec make_powerup_grid grid dict  = 
  match dict with
  | [] -> grid
  | (x, y, powerup) :: t ->
    if powerup = SquareHit
    then let () = (grid.(y).(x) <- Emoji.collision) in make_powerup_grid grid t 
    else if powerup = ReHit
    then let () =  (grid.(y).(x) <- Emoji.gem_stone)in  make_powerup_grid grid t 
    else if powerup = InstaKill
    then let () = (grid.(y).(x) <- Emoji.skull) in make_powerup_grid grid t 
    else failwith "Cannot get"

let string_of_matrix_2 matrix = 
  let init_list = ref [] in 
  for i = 0 to (c_ROWS - 1) do
    let s = ref "" in 
    for j = 0 to (c_COLS - 1) do 
      s := !s ^ " " ^ matrix.(i).(j) 
    done;
    init_list :=  !init_list @ [!s];
  done;
  !init_list

(** [print_power_ups player game] prints the powerups of the player
    to screen. 
    Used for dbugging purposes. *)
let print_power_ups player game = 
  match player with 
  | Player1 -> 
    game.player_1_powerups 
    |> make_powerup_grid (empty_powerup ())
    |> string_of_matrix_2
    |> List.map print_endline
    |> ignore
  | Player2 ->
    game.player_2_powerups 
    |> make_powerup_grid (empty_powerup ())
    |> string_of_matrix_2
    |> List.map print_endline
    |> ignore

let check_coord_in_powerups x y player battleship = 
  match player with 
  | Player1 ->
    battleship.player_1_powerups 
    |> List.map (fun (x, y, name) -> (x, y))
    |> List.mem (x, y) 
  | Player2 ->
    battleship.player_2_powerups 
    |> List.map (fun (x, y, name) -> (x, y))
    |> List.mem (x, y) 



let rec get_powerup_name_helper x y powerup_dict =
  match powerup_dict with 
  | [] -> failwith "Coul not find powerup"
  | (x', y', powerup) :: t ->
    if x' = x && y' = y then powerup_to_string powerup 
    else get_powerup_name_helper x y t

let get_powerup_name x y player battleship = 
  match player with 
  | Player1 ->
    battleship.player_1_powerups 
    |> get_powerup_name_helper x y
  | Player2 ->
    battleship.player_2_powerups 
    |> get_powerup_name_helper x y

let rec point_to_ship_name x y player_list = 
  match player_list with 
  | [] -> None
  | (ship_name, pos_lst) :: t ->
    let coord_lst = List.map (fun (x, y, status) -> (x, y)) pos_lst in 
    if List.mem (x, y) coord_lst then Some ship_name
    else point_to_ship_name x y t

let rec change_ship_dict_to_damaged ship_name dict acc = 
  match dict with 
  | [] -> (List.rev acc , [])
  | (name, coord_lst) :: t ->
    if name = ship_name then 
      let new_coord_lst = List.map (fun (x, y, _) -> (x, y, Damaged)) coord_lst in 
      (acc @ ((name, new_coord_lst) :: t), 
       (List.map (fun (x, y, _) -> (x, y)) new_coord_lst)
      )
    else 
      change_ship_dict_to_damaged ship_name t ((name, coord_lst) :: acc)

let get_ship_coordinates x y dict = 
  let ship_name = point_to_ship_name x y dict in 
  match ship_name with
  | Some n -> change_ship_dict_to_damaged n dict []
  | None -> (dict, [])

