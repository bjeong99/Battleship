type direction = 
  | Left
  | Right
  | Up
  | Down
type status = 
  | Damaged
  | Undamaged

type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

type t = (string * ((int * int * status) list) ) list

exception NonexistShip
let empty = []

let ship_length ship = 
  match ship with
  | Battleship -> 4
  | AircraftCarrier -> 5
  | Destroyer  -> 3
  | Cruiser  -> 3
  | Submarine  -> 2

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

let string_to_ship ship = 
  let shiplow = String.lowercase_ascii ship in 
  if shiplow = "battleship" then Battleship
  else if shiplow = "aircraftcarrier" then AircraftCarrier
  else if shiplow = "destroyer" then Destroyer
  else if shiplow = "cruiser" then Cruiser
  else if shiplow = "submarine" then Submarine
  else failwith "not legal ship"


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
  |> List.for_all (fun (x, y) -> check_cell_occupied (x, y) dict)


let empty_board = 
  Array.make_matrix 10 10 "_"

let rec make_grid dict grid = 
  match dict with
  | [] -> grid
  | (string, list_of_ints) :: t ->
    let first_letter = Str.first_chars (String.uppercase_ascii string) 1 in
    List.map (fun (x, y, z) -> grid.(y).(x) <- 
                 (if z = Undamaged then first_letter else String.lowercase_ascii
                      first_letter)) list_of_ints |> ignore;
    make_grid t grid

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





