type point = int * int

type t = {
  guess_phase : bool;
  target_horizontal : bool;
  current_location : point option;
  hit_coord : point option;
  horizontal_points : point list;
  vertical_points : point list;
  remaining_coords : point list;
  locations_targeted : point list;
}

let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
  in create_rows m n []

let initialize_hard_ai = {
  guess_phase = true;
  target_horizontal = true;
  current_location = None;
  hit_coord = None;
  horizontal_points = [];
  vertical_points = [];
  remaining_coords = create_pairs 10 10;
  locations_targeted = [];
}

(** [get_guess phase ai] is the targeting phase the [ai] is in, either
    the random or smart phase. *)
let get_guess_phase ai = 
  ai.guess_phase

(** [choose_random_target remaining_coords] is a random
    coordinate pair from [remaining_coords]. 

    Requires: [remaining_coords] has at least one (1) element. 
              [Remaining_coords] has a unique list of coordinates. *)
let choose_random_target remaining_coords = 
  let num_remaining = List.length remaining_coords in 
  let random_index = Random.int num_remaining in 
  List.nth remaining_coords random_index

let random_target ai = 
  let random_coord = choose_random_target (ai.remaining_coords) in 
  ({ai with
    current_location = Some random_coord;
    remaining_coords = 
      List.filter (fun c -> c <> random_coord) ai.remaining_coords;
    locations_targeted = 
      random_coord :: ai.locations_targeted;}, random_coord)

let filter_coords ai (x, y)= 
  x >= 0 && x <= 9 && y >= 0 && y <= 9 &&
  not (List.mem (x, y) ai.locations_targeted) 

let find_neighboring_horiz (x, y) ai = 
  let horizontal = [(x - 1, y); (x + 1, y)] in 
  List.filter (filter_coords ai) horizontal 

let find_neighboring_vert (x, y) ai = 
  let vertical  = [(x, y - 1); (x, y + 1)] in 
  List.filter (filter_coords ai) vertical

(** Requires: is a valid board coordinate, x from 0 to 9, y from 0 to 9 
              ALSO, ships must be length 2 or more. No length 1 ships. *)
let find_neighboring_coords (x, y) ai = 
  (find_neighboring_horiz (x, y) ai, find_neighboring_vert (x, y) ai)

let random_to_smart ai hit_coord = 
  let (horiz, vert) = find_neighboring_coords hit_coord ai in
  { ai with 
    guess_phase = false;
    hit_coord = Some hit_coord;
    horizontal_points = horiz;
    vertical_points = vert; } 

let horizontal_target ai = 
  let horizontal_points = ai.horizontal_points in 
  let random_index = Random.int (List.length horizontal_points) in 
  List.nth horizontal_points random_index

let vertical_target ai = 
  let vertical_points = ai.vertical_points in 
  let random_index = Random.int (List.length vertical_points) in 
  List.nth vertical_points random_index

let smart_choose_target ai = 
  if ai.target_horizontal then horizontal_target ai
  else vertical_target ai

let smart_target ai = 
  let new_target = smart_choose_target ai in 
  ({ai with
    current_location = Some new_target;
    remaining_coords = 
      List.filter (fun c -> c <> new_target) ai.remaining_coords;
    locations_targeted = 
      new_target :: ai.locations_targeted;}, new_target)

let change_to_vertical ai = 
  if ai.horizontal_points = [] 
  then {ai with target_horizontal = false}
  else ai

let add_horizontal_neighbors ai = 
  let current_location = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.current_location in 
  let horiz_neighbors = find_neighboring_horiz current_location ai in 
  {ai with horizontal_points = ai.horizontal_points @ horiz_neighbors}

let add_vertical_neighbors ai = 
  let current_location = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.current_location in 
  let vert_neighbors = find_neighboring_vert current_location ai in 
  {ai with vertical_points = ai.vertical_points @ vert_neighbors}

let smart_update_neighbors ai coord = 
  if ai.horizontal_points = [] 
  then ai |> change_to_vertical |> add_vertical_neighbors
  else ai |> add_horizontal_neighbors

let smart_to_random ai ship_sunk = 
  if ship_sunk 
  then 
    {ai with 
     guess_phase = true; 
     target_horizontal = true;
     hit_coord = None;
     horizontal_points = [];
     vertical_points = [];}
  else ai

