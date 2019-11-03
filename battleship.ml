let n_rows = 10
let n_cols = 10

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

