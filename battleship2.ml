type direction = 
  | Left
  | Right
  | Up
  | Down

type t = (string * ((int * int) list) ) list

let empty = []

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


type ship_type = 
  | Battleship 
  | AircraftCarrier 
  | Destroyer 
  | Cruiser 
  | Submarine 

let ship_length ship = 
  match ship with
  | Battleship -> 4
  | AircraftCarrier -> 5
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


let insert ship (x, y) direction dict = 
  let ship_positions = generate_pos_list (x, y) (ship_length (string_to_ship ship)) direction in   
  (ship, ship_positions) :: dict

let remove ship dict = 
  List.filter (fun (ship_elt, _ ) -> ship <> ship_elt) dict

type player = Player1 | Player2

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

let check_cell_unoccupied (x, y) dict = 
  List.fold_left (fun init (key, values_list) -> values_list :: init) [] dict |> List.mem (x, y)

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


let empty_board = 
  Array.make_matrix 10 10 "_"

let rec make_grid dict grid = 
  match dict with
  | [] -> grid
  | (string, list_of_ints) :: t ->
    let first_letter = Str.first_chars (String.uppercase_ascii string) 1 in
    List.map (fun (x, y) -> grid.(y).(x) <- first_letter) list_of_ints |> ignore;
    make_grid t grid





