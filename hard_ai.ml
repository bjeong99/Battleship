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

(** [all_lists_empty ai] is [true] iff 
    all directional targeting lists for smart
    are empty. *)
let all_lists_empty ai = 
  ai.left_list = [] &&
  ai.right_list = [] &&
  ai.top_list = [] &&
  ai.bottom_list = []

(** [all_bounds_true a] is [true] iff
    all direction bounds for smart ai are [true]. *)
let all_bounds_true ai = 
  ai.hit_bottom_bound &&
  ai.hit_left_bound &&
  ai.hit_top_bound &&
  ai.hit_right_bound

(** [all_bounds_or_all_lists a] is [true] iff
    all direction bounds for smart ai are [true]
    or all lists are empty. *)
let all_bounds_or_all_lists ai = 
  all_lists_empty ai || all_bounds_true ai

let get_guess_phase ai = 
  ai.guess_phase

let random_to_smart ai = {
  ai with guess_phase = false;
}

(** [change_left_bound ai] changes the left bound to [true]. *)
let change_left_bound ai = {
  ai with
  hit_left_bound = true;
}

(** [change_right_bound ai] changes the right bound to [true]. *)
let change_right_bound ai = {
  ai with 
  hit_right_bound = true;
}

(** [change_top_bound ai] changes the top bound to [true]. *)
let change_top_bound ai = {
  ai with 
  hit_top_bound = true;
}

(** [change_bottom_bound ai] changes the bottom bound to [true]. *)
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

(** [generate_up hit_already (x, y) acc] generates a list of points
    upwards from [(x, y)] as long as no points above [x][y] are [hit_already].*)
let rec generate_up hit_already (x, y) acc = 
  if y < 1 then List.rev acc 
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_up hit_already (x, y - 1) ((x, y) :: acc)

(** [generate_down hit_already (x, y) acc] generates a list of points
    down from [(x, y)] as long as no points above [x][y] are [hit_already].*)
let rec generate_down hit_already (x, y) acc = 
  if y > 10 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_down hit_already (x, y + 1) ((x, y) :: acc)

(** [generate_right hit_already (x, y) acc] generates a list of points
    right from [(x, y)] as long as no points above [x][y] are [hit_already].*)
let rec generate_right hit_already (x, y) acc = 
  if x > 10 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_right hit_already (x + 1, y) ((x, y) :: acc)

(** [generate_left hit_already (x, y) acc] generates a list of points
    left from [(x, y)] as long as no points above [x][y] are [hit_already].*)
let rec generate_left hit_already (x, y) acc = 
  if x < 1 then List.rev acc
  else if List.mem (x, y) hit_already then List.rev acc 
  else generate_left hit_already (x - 1, y) ((x, y) :: acc)

(** [generate_left ai (x, y)] generates a list of points
    for all the directions around [(x, y)]. *)
let generate_lists ai (x, y) = 
  let hit_already = ai.locations_targeted in 
  { ai with
    left_list = generate_left hit_already (x - 1, y) [];
    right_list = generate_right hit_already (x + 1, y) [];
    top_list = generate_up hit_already (x, y - 1) [];
    bottom_list = generate_down hit_already (x, y + 1) [];}

let update_smart_ai_after_hit ai (x, y) =
  generate_lists ai (x, y)

(** [reset_bounds_and_lists ai] sets all bounds ro [false]
    all lists to empty and sets [guess_phase] to [true]. *)
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

(** [smart_target_left ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Chooses in the leftward direction. 
    Requires: can only use when a ship has been hit and not sunk yet.*)
let smart_target_left ai = 
  let target = List.hd ai.left_list in 
  (target, 
   {ai with locations_targeted = target :: ai.locations_targeted;
            remaining_coords = 
              List.filter (fun c -> c <> target) ai.remaining_coords;
            left_list = 
              List.filter (fun c -> c <> target) ai.left_list;})

(** [smart_target_right ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Chooses in the righward direction. 
    Requires: can only use when a ship has been hit and not sunk yet.*)
let smart_target_right ai = 
  let target = List.hd ai.right_list in 
  (target,
   {ai with locations_targeted = target :: ai.locations_targeted;
            remaining_coords = 
              List.filter (fun c -> c <> target) ai.remaining_coords;
            right_list = 
              List.filter (fun c -> c <> target) ai.right_list;})

(** [smart_target_up ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Chooses in the upward direction. 
    Requires: can only use when a ship has been hit and not sunk yet.*)
let smart_target_up ai = 
  let target =  List.hd ai.top_list in 
  (target, 
   {ai with locations_targeted = target :: ai.locations_targeted;
            remaining_coords = 
              List.filter (fun c -> c <> target) ai.remaining_coords;
            top_list = 
              List.filter (fun c -> c <> target) ai.top_list;})

(** [smart_target_down ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Chooses in the downward direction. 
    Requires: can only use when a ship has been hit and not sunk yet.*)
let smart_target_down ai = 
  let target = List.hd ai.bottom_list in 
  (target, 
   {ai with locations_targeted = target :: ai.locations_targeted;
            remaining_coords = 
              List.filter (fun c -> c <> target) ai.remaining_coords;
            bottom_list = 
              List.filter (fun c -> c <> target) ai.bottom_list;})

let smart_target ai =  
  if ai.left_list <> [] && (not ai.hit_left_bound) then 
    smart_target_left ai
  else if ai.right_list <> [] && (not ai.hit_right_bound) then 
    smart_target_right ai
  else if ai.top_list <> [] && (not ai.hit_top_bound) then
    smart_target_up ai
  else if ai.bottom_list <> [] && (not ai.hit_bottom_bound) then 
    smart_target_down ai
  else failwith "It is impossible to not be able to sink a ship in this scheme"
(* let target = List.hd ai.left_list in 
   (target, 
   {ai with locations_targeted = target :: ai.locations_targeted;
          remaining_coords = 
            List.filter (fun c -> c <> target) ai.remaining_coords;
          left_list = 
            List.filter (fun c -> c <> target) ai.left_list;}) *)
(* else if ai.right_list <> [] && (not ai.hit_right_bound) then 
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
   else failwith "It is impossible to not be able to sink a ship in this scheme" *)

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

(**  [create_pairs m n] generates all pairs from (1, 1)
     to (m, n) inclusive of [m] and [n]. *)
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

(* [targeted] represents if a lcoation is targeted or not] *)
type targeted =   
  | Targeted
  | Untargeted

(** [empty_target_array ()] is an array of untargeted locations. *)
let empty_target_array () = 
  Array.make_matrix c_COL c_ROW Untargeted

(** [update_target_array target_arr locs_targeted] 
    updates [target_arr] with [locs_targted]. *)
let rec update_target_array target_arr locs_targeted = 
  match locs_targeted with
  | [] -> target_arr
  | (x, y) :: t -> 
    target_arr.(x - 1).(y - 1) <- Targeted; 
    update_target_array target_arr t 

(** [empty_counts_array ()] is an array with 0 everywhere 
    representing counts.  *)
let empty_counts_array () = 
  Array.make_matrix c_COL c_ROW 0

(** [incr_arr x y arr] changes [arr] at [x][y]
    by 1.*)
let incr_arr x y arr = 
  arr.(x - 1).(y - 1) <- arr.(x - 1).(y - 1) + 1

(** [incr_x_range x_0 x_1 y arr] changes [arr] at all [x][y]
    by 1 along from [x_0] to [x_1].*)
let rec incr_x_range x_0 x_1 y arr = 
  if x_0 = x_1 then incr_arr x_0 y arr
  else let () = incr_arr x_0 y arr in incr_x_range (x_0 + 1) x_1 y arr

(** [incr_y_range  x y_0 y_1 arr] changes [arr] at all [x][y]
    by 1 along from [y_0] to [y_1].*)
let rec incr_y_range x y_0 y_1 arr = 
  if y_0 = y_1 then incr_arr x y_0 arr
  else let () = incr_arr x y_0 arr in incr_y_range x (y_0 + 1) y_1 arr

(** [iter_vert_ship x y_0 y_1 target_arr] inserts one ships down a column
    defined by [x] from [y_0] to [y_1] and augments counts.
     on [target_arr] each time a ship can be placed at [x][y_0] *)
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

(** [iter_horiz_ship x_0 x_1 y target_arr] inserts one ships along a row
    defined by [y] from [x_0] to [x_1] and augments counts.
     on [target_arr] each time a ship can be placed at [x_0][y] *)
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

(** [iter_vert_col col start_y ship_length target_arr counts_arr] 
    inserts as many ships ships down a column
    defined by [col] from [start_y] to [start_y - 1 + ship_length] 
    and augments counts.
     on [target_arr] adn [counts_arr] *)
let rec iter_vert_col col start_y ship_length target_arr counts_arr =
  if start_y + ship_length - 1 > c_ROW then counts_arr
  else begin
    if iter_vert_ship col start_y (start_y + ship_length - 1) target_arr 
    then 
      let () = 
        incr_y_range col start_y (start_y + ship_length - 1) counts_arr in 
      iter_vert_col col (start_y + 1) ship_length target_arr counts_arr
    else iter_vert_col col (start_y + 1) ship_length target_arr counts_arr 
  end

(** [iter_horiz_row col start_x ship_length target_arr counts_arr] 
    inserts as many ships ships down a column
    defined by [row] from [start_x] to [start_x - 1 + ship_length] 
    and augments counts.
     on [target_arr] adn [counts_arr] *)
let rec iter_horiz_row row start_x ship_length target_arr counts_arr =
  if start_x + ship_length  - 1 > c_COL then counts_arr
  else begin
    if iter_horiz_ship start_x (start_x + ship_length - 1) row target_arr 
    then 
      let () = 
        incr_x_range start_x (start_x + ship_length - 1) row counts_arr in 
      iter_horiz_row row (start_x + 1) ship_length target_arr counts_arr
    else iter_horiz_row row (start_x + 1) ship_length target_arr counts_arr 
  end

(** [iter_col col ship_length target_arr counts_arr] iterates down 
    all columsn starting at [col] with [ship_length] sized
    ships to augment  [target_arr counts_arr]. *)
let rec iter_col col ship_length target_arr counts_arr = 
  if col = c_COL then iter_vert_col col 1 ship_length target_arr counts_arr
  else 
    iter_vert_col col 1 ship_length target_arr counts_arr
    |> iter_col (col + 1) ship_length target_arr 

(** [iter_row row ship_length target_arr counts_arr] iterates down 
    all columsn starting at [row] with [ship_length] sized
    ships to augment  [target_arr counts_arr]. *)
let rec iter_row row ship_length target_arr counts_arr = 
  if row = c_ROW then iter_horiz_row row 1 ship_length target_arr counts_arr
  else 
    iter_horiz_row row 1 ship_length target_arr counts_arr
    |> iter_row (row + 1) ship_length target_arr 

(** [iter_ship] iterates down all columsn adn rows for a [ship_length] to
    augment [target_arr] and [counts_arr] *)
let iter_ship ship_length target_arr counts_arr = 
  iter_col 1 ship_length target_arr counts_arr
  |> iter_row 1 ship_length target_arr

(** [c_SHIP_LENGTHS] are the lengths of all ships in the game. *) 
let c_SHIP_LENGTHS = 
  [2; 3; 3; 4; 5]

(** [iter_counts target_arr] will augment the counts array
    based opn [target_arr] for all ships, columns and rows. *)
let iter_counts target_arr = 
  List.fold_left 
    (fun init_counts_arr len -> iter_ship len target_arr init_counts_arr) 
    (empty_counts_array ())
    c_SHIP_LENGTHS

(** [make_upper_right ()] sets a ref at the upper right coordinate. *)
let make_upper_right () = 
  (ref 1, ref 1)

(** [ make_matrix_start () matrix] sets a ref
    with calue at upper left. *)
let make_matrix_start () matrix =   
  ref matrix.(0).(0)

(** [get_max_index_matrix matrix ] is the index of the max value
    in counts array [matrix].  *)
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
  (!x, !y)

(** [get_random locs_remaining] gets a random element from
    [locs-remaining]. 
    Raises: [index out of bounds error] if [locs-remaining] is length 0. *)
let get_random locs_remaining = 
  let length = List.length locs_remaining in 
  let random = Random.int length in 
  List.nth locs_remaining random

(**  [repeat_choose_random locs_targeted locs_remaining chosen_target] 
     will choose a randomn targete from [locs_targeted] 
     and [locs_remaining] if [chosen_target] was visigted before,
     otherwise [chosen_target]. *)
let repeat_choose_random locs_targeted locs_remaining chosen_target = 
  if List.mem chosen_target locs_targeted
  then get_random locs_remaining 
  else chosen_target

(** [prob_target locs_targeted locs_remaining]chooses the target with
    highest count in [locs_remaining] not in [locs_targeted] according
    to the algorithm to place ships every location. *)
let prob_target locs_targeted locs_remaining = 
  locs_targeted 
  |> update_target_array (empty_target_array ())
  |> iter_counts 
  |> get_max_index_matrix
  |> repeat_choose_random locs_targeted locs_remaining

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
