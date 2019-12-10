(* open Emoji

   let c_ROWS = 10
   let c_COLS = 10

   let c_BOARD_SEP = "    "

   (* [direction] represents the four different orientations a ship's 
   head can be placed in *)
   type direction = 
   | Left
   | Right
   | Up
   | Down

   (* [ship_type] represents the type that a ship can be. *)
   type ship_type = 
   | Battleship 
   | AircraftCarrier 
   | Destroyer 
   | Cruiser 
   | Submarine 

   (* [status] represents if a some coordinate a ship lies on has 
   been hit or not hit *)
   type status = 
   | Damaged
   | Undamaged 

   (* [player] represents the either player1, or player2. Player2
   is the only player that can be an AI *)
   type player = 
   | Player1
   | Player2

   (* [list_t] represents the coordinates where each ship has been placed *)
   type list_t = (string * ((int * int * status) list) ) list

   type t = {
   player_1_ships_placed : ship_type list;
   player_1_ships_remaining : ship_type list;
   player_1_ships : (string * ((int * int * status) list) ) list;
   player_2_ships_placed : ship_type list;
   player_2_ships_remaining : ship_type list;
   player_2_ships : (string * ((int * int * status) list) ) list;
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

   let empty = {
   player_1_ships_placed = [];
   player_1_ships_remaining = starter_ship_list;
   player_1_ships = [];
   player_2_ships_placed = [];
   player_2_ships_remaining = starter_ship_list;
   player_2_ships = [];
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
   | AircraftCarrier -> "AircraftCarrier"
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
   Array.make_matrix 10 10 Emoji.white_medium_square

   let rec make_grid dict grid = 
   match dict with
   | [] -> grid
   | (string, list_of_ints) :: t ->
    let first_letter = String.get (String.uppercase_ascii string) 0 |> Char.escaped in
    List.map (fun (x, y, z) -> grid.(y).(x) <- 
                 (if z = Undamaged then first_letter else Emoji.fire)) 
      list_of_ints |> ignore;
    make_grid t grid

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
   let axis_x = ref "    A B C D E F G H I J\n" in 
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

   let print_matrix_2 matrix = 
   for i = 0 to (c_ROWS - 1) do
    let s = ref "" in 
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
   |> List.map ship_to_string

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
   else remove_ship_helper ship player game


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
   (*|> (fun l -> l - 1) *)
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
    in the way, or going off the board. *)
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
    in the way, or going off the board.*)
   let random_ship player game = 
   randomly_laydown_ships_helper player game

   (** [randomly_laydownn_ships game] is the new game  
    with player2, the AI, having ONE random ship placed in a random
    location, in a random direction. 
    THERE IS NO GUARANTEE THAT THE SHIP CAN BE PLACED
    AT THAT LOCATION, for example due to another ship
    in the way, or going off the board.*)
   let randomly_laydown_ships game = 
   randomly_laydown_ships_helper Player2 game

   (*
   let () = Random.self_init () in 
   let remaining_ships_num = List.length game.player_2_ships_remaining in 
   let random_ship = List.nth (game.player_2_ships_remaining |> List.map ship_type_to_ship_name) (Random.int remaining_ships_num) in 
   let random_direction = List.nth direction_list (Random.int (List.length direction_list)) in 
   let pairs_list = create_pairs c_ROWS c_COLS in 
   let random_position = choose_target pairs_list in 
   (random_position, random_direction, random_ship)
 *)


*)
