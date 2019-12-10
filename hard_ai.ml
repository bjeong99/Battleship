let c_COL = 10
let c_ROW = 10

type point = int * int

type target_direction = 
  | Left
  | Right
  | Top
  | Bottom

(* CITATION

   The algorithm idea and strategy comes
   from http://datagenetics.com/blog/december32011/index.html 

   We fully ackwoledge the direct influence of the strategy on
   our code. *)

type t = {
  guess_phase : bool;

  insane : bool;

  (*
  target_horizontal : bool;
  current_location : point option;
  hit_coord : point option;
  horizontal_points : point list;
  vertical_points : point list;
  *)

  remaining_coords : point list;
  locations_targeted : point list;


  hit_left_bound : bool;
  hit_right_bound : bool;
  hit_top_bound : bool;
  hit_bottom_bound : bool;


  left_list : point list;
  right_list : point list;
  top_list : point list;
  bottom_list : point list;
}

(* algorithm 

   Once there us a hit, store the hit point in hit coord
   Also, store neighboring points of the hit point in horiz and vert column
   Actually create four lists: left, right, up, down
   In order, with points closest to the hit point, go left, then right, then up, then down
   if you 
*)

(** [get_guess phase ai] is the targeting phase the [ai] is in, either
    the random or smart phase. *)

let all_lists_empty ai = 
  ai.left_list = [] &&
  ai.right_list = [] &&
  ai.top_list = [] &&
  ai.bottom_list = []

let all_bounds_true ai = 
  ai.hit_bottom_bound &&
  ai.hit_left_bound &&
  ai.hit_top_bound &&
  ai.hit_right_bound

let all_bounds_or_all_lists ai = 
  all_lists_empty ai || all_bounds_true ai

let get_guess_phase ai = 
  ai.guess_phase

let random_to_smart ai = {
  ai with guess_phase = false;
}

let change_left_bound ai = {
  ai with
  hit_left_bound = true;
}

let change_right_bound ai = {
  ai with 
  hit_right_bound = true;
}

let change_top_bound ai = {
  ai with 
  hit_top_bound = true;
}

let change_bottom_bound ai = {
  ai with 
  hit_bottom_bound = true;
}

let update_smart_after_miss ai = 
  if not ai.hit_left_bound then 
    change_left_bound ai 
  else if not ai.hit_right_bound then 
    change_right_bound ai |> change_left_bound
  else if not ai.hit_top_bound then 
    change_top_bound ai 
    |> change_right_bound 
    |> change_left_bound
  else if not ai.hit_bottom_bound then 
    change_bottom_bound ai 
    |> change_top_bound 
    |> change_right_bound 
    |> change_left_bound  
  else failwith "There are no other directions a miss hit could occur in"

let rec generate_up hit_already (x, y) acc = 
  if y < 1 then List.rev acc 
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_up hit_already (x, y - 1) ((x, y) :: acc)

let rec generate_down hit_already (x, y) acc = 
  if y > 10 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_down hit_already (x, y + 1) ((x, y) :: acc)

let rec generate_right hit_already (x, y) acc = 
  if x > 10 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_right hit_already (x + 1, y) ((x, y) :: acc)

let rec generate_left hit_already (x, y) acc = 
  if x < 1 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_left hit_already (x - 1, y) ((x, y) :: acc)

let generate_lists ai (x, y) = 
  let hit_already = ai.locations_targeted in 
  { ai with
    left_list = generate_left hit_already (x - 1, y) [];
    right_list = generate_right hit_already (x + 1, y) [];
    top_list = generate_up hit_already (x, y - 1) [];
    bottom_list = generate_down hit_already (x, y + 1) [];}

let update_smart_ai_after_hit ai (x, y) =
  generate_lists ai (x, y)

let reset_bounds_and_lists ai = {
  ai with 
  guess_phase = true;
  hit_left_bound = false;
  hit_right_bound = false;
  hit_top_bound = false;
  hit_bottom_bound = false;
  left_list = [];
  right_list = [];
  top_list = [];
  bottom_list = []; 
}

let smart_to_random ai = 
  reset_bounds_and_lists ai

let smart_target ai =  
  if ai.left_list <> [] && (not ai.hit_left_bound) then 
    let target = List.hd ai.left_list in 
    (target, 
     {ai with locations_targeted = target :: ai.locations_targeted;
              remaining_coords = 
                List.filter (fun c -> c <> target) ai.remaining_coords;
              left_list = 
                List.filter (fun c -> c <> target) ai.left_list;})
  else if ai.right_list <> [] && (not ai.hit_right_bound) then 
    let target = List.hd ai.right_list in 
    (target,
     {ai with locations_targeted = target :: ai.locations_targeted;
              remaining_coords = 
                List.filter (fun c -> c <> target) ai.remaining_coords;
              right_list = 
                List.filter (fun c -> c <> target) ai.right_list;})
  else if ai.top_list <> [] && (not ai.hit_top_bound) then 
    let target =  List.hd ai.top_list in 
    (target, 
     {ai with locations_targeted = target :: ai.locations_targeted;
              remaining_coords = 
                List.filter (fun c -> c <> target) ai.remaining_coords;
              top_list = 
                List.filter (fun c -> c <> target) ai.top_list;})
  else if ai.bottom_list <> [] && (not ai.hit_bottom_bound) then 
    let target = List.hd ai.bottom_list in 
    (target, 
     {ai with locations_targeted = target :: ai.locations_targeted;
              remaining_coords = 
                List.filter (fun c -> c <> target) ai.remaining_coords;
              bottom_list = 
                List.filter (fun c -> c <> target) ai.bottom_list;})
  else failwith "It is impossible to not be able to sink a ship in this scheme"

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
  (random_coord, 
   {ai with
    remaining_coords = 
      List.filter (fun c -> c <> random_coord) ai.remaining_coords;
    locations_targeted = 
      random_coord :: ai.locations_targeted;})

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
  remaining_coords = create_pairs c_COL c_ROW;
  locations_targeted = [];

  insane = true;

  hit_left_bound = false;
  hit_right_bound  = false;
  hit_top_bound  = false;
  hit_bottom_bound  = false;


  left_list = [];
  right_list = [];
  top_list = [];
  bottom_list = [];
}

(* Insane AI targeting *)

let print_int_matrix matrix = 
  matrix 
  |> Array.map 
    (fun array -> 
       Array.fold_left 
         (fun acc i -> 
            acc ^ "  " ^ string_of_int i) "" array) 
  |> Array.map print_endline
  |> ignore

type targeted =   
  | Targeted
  | Untargeted

let empty_target_array () = 
  Array.make_matrix c_COL c_ROW Untargeted

let rec update_target_array target_arr locs_targeted = 
  match locs_targeted with
  | [] -> target_arr
  | (x, y) :: t -> 
    target_arr.(x - 1).(y - 1) <- Targeted; 
    update_target_array target_arr t 

let empty_counts_array () = 
  Array.make_matrix c_COL c_ROW 0

let incr_arr x y arr = 
  arr.(x - 1).(y - 1) <- arr.(x - 1).(y - 1) + 1

let rec incr_x_range x_0 x_1 y arr = 
  if x_0 = x_1 then incr_arr x_0 y arr
  else let () = incr_arr x_0 y arr in incr_x_range (x_0 + 1) x_1 y arr

let rec incr_y_range x y_0 y_1 arr = 
  if y_0 = y_1 then incr_arr x y_0 arr
  else let () = incr_arr x y_0 arr in incr_y_range x (y_0 + 1) y_1 arr

let rec iter_vert_ship x y_0 y_1 target_arr = 
  if y_0 = y_1 
  then begin
    if target_arr.(x - 1).(y_0 - 1) = Untargeted 
    then true
    else false
  end
  else begin
    if target_arr.(x - 1).(y_0 - 1) = Untargeted 
    then iter_vert_ship x (y_0 + 1) y_1 target_arr
    else false
  end

let rec iter_horiz_ship x_0 x_1 y target_arr = 
  if x_0 = x_1 
  then begin
    if target_arr.(x_0 - 1).(y - 1) = Untargeted 
    then true
    else false
  end
  else begin
    if target_arr.(x_0 - 1).(y - 1) = Untargeted 
    then iter_horiz_ship (x_0 + 1) x_1 y target_arr
    else false
  end

let rec iter_vert_col col start_y ship_length target_arr counts_arr =
  if start_y + ship_length - 1 > c_ROW then counts_arr
  else begin
    if iter_vert_ship col start_y (start_y + ship_length - 1) target_arr 
    then 
      let () = incr_y_range col start_y (start_y + ship_length - 1) counts_arr in 
      iter_vert_col col (start_y + 1) ship_length target_arr counts_arr
    else iter_vert_col col (start_y + 1) ship_length target_arr counts_arr 
  end

let rec iter_horiz_row row start_x ship_length target_arr counts_arr =
  if start_x + ship_length  - 1 > c_COL then counts_arr
  else begin
    if iter_horiz_ship start_x (start_x + ship_length - 1) row target_arr 
    then 
      let () = incr_x_range start_x (start_x + ship_length - 1) row counts_arr in 
      iter_horiz_row row (start_x + 1) ship_length target_arr counts_arr
    else iter_horiz_row row (start_x + 1) ship_length target_arr counts_arr 
  end

let rec iter_col col ship_length target_arr counts_arr = 
  if col = c_COL then iter_vert_col col 1 ship_length target_arr counts_arr
  else 
    iter_vert_col col 1 ship_length target_arr counts_arr
    |> iter_col (col + 1) ship_length target_arr 

let rec iter_row row ship_length target_arr counts_arr = 
  if row = c_ROW then iter_horiz_row row 1 ship_length target_arr counts_arr
  else 
    iter_horiz_row row 1 ship_length target_arr counts_arr
    |> iter_row (row + 1) ship_length target_arr 

let iter_ship ship_length target_arr counts_arr = 
  iter_col 1 ship_length target_arr counts_arr
  |> iter_row 1 ship_length target_arr

let c_SHIP_LENGTHS = 
  [2; 3; 3; 4; 5]

let iter_counts target_arr = 
  List.fold_left 
    (fun init_counts_arr len -> iter_ship len target_arr init_counts_arr) 
    (empty_counts_array ())
    c_SHIP_LENGTHS

let make_upper_right () = 
  (ref 1, ref 1)

let make_matrix_start () matrix =   
  ref matrix.(0).(0)

let get_max_index_matrix matrix = 
  let x, y = make_upper_right () in 
  let value = make_matrix_start () matrix in 
  for i = 1 to c_COL do 
    for j = 1 to c_ROW do 
      if matrix.(i - 1).(j - 1) > !value 
      then (value := matrix.(i - 1).(j - 1); x := i; y := j)
    done
  done; 
  assert (!x > 0 && !x <= 10 && !y > 0 && !y <= 10);
  (*assert (!value <> 0);*)
  (!x, !y)

let get_random locs_remaining = 
  let length = List.length locs_remaining in 
  let random = Random.int length in 
  List.nth locs_remaining random

let repeat_choose_random locs_targeted locs_remaining chosen_target = 
  if List.mem chosen_target locs_targeted
  then get_random locs_remaining 
  else chosen_target

let prob_target locs_targeted locs_remaining = 
  locs_targeted 
  |> update_target_array (empty_target_array ())
  |> iter_counts 
  |> get_max_index_matrix
  |> repeat_choose_random locs_targeted locs_remaining

let string_point (x, y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
let print_points lst = List.map (fun p -> print_endline (string_point p)) lst

let insane_target ai = 
  let chosen_target = prob_target ai.locations_targeted ai.remaining_coords in 
  (chosen_target, 
   {ai with
    remaining_coords = 
      List.filter (fun c -> c <> chosen_target) ai.remaining_coords;
    locations_targeted = 
      chosen_target :: ai.locations_targeted;})

let insane_to_smart ai = {
  ai with insane = false;
}

let smart_to_insane ai = 
  ai
  |> reset_bounds_and_lists 
  |> (fun ai -> {ai with insane = true})

let get_insane_phase ai = 
  ai.insane




(*
let get_target_horizontal ai = 
  ai.target_horizontal

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
  let hit_coord = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.hit_coord in 
  let current_location = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.current_location in 
  let horiz_neighbors = (find_neighboring_horiz current_location ai) @ (find_neighboring_horiz hit_coord ai) |> List.sort_uniq compare in 
  {ai with horizontal_points = ai.horizontal_points @ horiz_neighbors}

let add_vertical_neighbors ai = 
  let hit_coord = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.hit_coord in 
  let current_location = 
    (function | Some c -> c | None -> failwith "no coordinate") 
      ai.current_location in 
  let vert_neighbors = (find_neighboring_vert current_location ai) @ (find_neighboring_vert hit_coord ai) |> List.sort_uniq compare in 
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
*)


(*
An algorithm to switch from horizontal to vertical targeting

suppose you make a hit with a ship next to the ship you hit

hit at 1, 2 first, guess horizontally
ss
xs
s
s
s
Hit coord (1, 2)
Horiz: (2, 2)
Next guess naturally is 2, 2

Then 
ss
xx
s
s
s 

Hit coord (1, 2)
Horiz: (3, 2)
(no sink, another hit)

Next guess naturally is 3, 2

Then 
ss
xxo
s
s
s
Hit coord (1, 2)
Horiz: 
Vert: (1, 1), (1, 3)
Which is a miss (because there are no other horizontal coordinates neighbors left - left edge is boundary, right was a miss
we then switch to vertical targeting from the original coordinate, (1, 2))

For which we do
xs
xxo
s
s
s
Hit coord (1, 2)
Horiz: 
Vert: (1, 3)
Which is a hit and not sunk

and then since the upper edge is blocked, we go down
xs
xxo
x
s
s
Hit coord (1, 2)
Horiz: 
Vert: (1, 4)
and then 
xs
xxo
x
x
s
Hit coord (1, 2)
Horiz: 
Vert: (1, 5)
and fianlly
xs
xxo
x
x
x
Hit coord (1, 2)
Horiz: 
Vert: (1, 6)
(hit and sunk, for which we end the smart phase and go back to the original random phase)





*)