open OUnit2
open State
open Battleship
open Command
open Hard_ai

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [make_command_test name str expected] is the OUnit
    test named [name] that asserts the equality of [string]
    and [parse expected]. *)
let make_command_test name str expected = 
  name >:: (fun _ ->
      assert_equal expected (parse str)
    )

(** [make_difficulty_test name str expected] is the OUnit
    test named [name] that asserts the equality of [string]
    and [parse_difficulty expected]. *)
let make_difficulty_test name str expected = 
  name >:: (fun _ ->
      assert_equal expected (parse_difficulty str)
    )

let str_list_to_str lst = 
  List.fold_left (fun init s -> init ^ " " ^ s) "" lst

let make_battleship_test 
    name
    battleship
    num_ships_remain1
    num_ships_remain2
    ships_remain1
    ships_remain2
    all_ships_sunk1
    all_ships_sunk2
    coordinates_ships1
    bool_coords1
    coordinates_ships2
    bool_coords2 = 
  name >:: (fun _ ->
      assert_equal ~printer: string_of_int num_ships_remain1 (remaining_ships (choose_player true) battleship);
      assert_equal ~printer: string_of_int num_ships_remain2 (remaining_ships (choose_player false) battleship);
      assert_equal ~printer: str_list_to_str ~cmp: cmp_set_like_lists ships_remain1 (remaining_ships_to_place (choose_player true) battleship);
      assert_equal ~printer: str_list_to_str ~cmp: cmp_set_like_lists ships_remain2 (remaining_ships_to_place (choose_player false) battleship);
      assert_equal ~printer: string_of_bool all_ships_sunk1 (battleship |> get_player_dict (choose_player true) |> check_all_ships_damaged);
      assert_equal ~printer: string_of_bool all_ships_sunk2 (battleship |> get_player_dict (choose_player false) |> check_all_ships_damaged);
      assert_equal ~printer: string_of_bool bool_coords1 
        (List.for_all (fun (x, y) -> check_coordinate_in_positions (x, y) (battleship |> get_player_dict (choose_player true) |> List.map (fun (_, t) -> t) |> List.flatten)) coordinates_ships1);
      assert_equal ~printer: string_of_bool bool_coords2 
        (List.for_all (fun (x, y) -> check_coordinate_in_positions (x, y) (battleship |> get_player_dict (choose_player false) |> List.map (fun (_, t) -> t) |> List.flatten)) coordinates_ships2)
    )

let make_random_battleship_test 
    name
    battleship
    num_ships_remain1
    num_ships_remain2
    ships_remain1
    ships_remain2
    all_ships_sunk1
    all_ships_sunk2 = 
  name >:: (fun _ ->
      assert_equal ~printer: string_of_int num_ships_remain1 (remaining_ships (choose_player true) battleship);
      assert_equal ~printer: string_of_int num_ships_remain2 (remaining_ships (choose_player false) battleship);
      assert_equal ~printer: str_list_to_str ~cmp: cmp_set_like_lists ships_remain1 (remaining_ships_to_place (choose_player true) battleship);
      assert_equal ~printer: str_list_to_str ~cmp: cmp_set_like_lists ships_remain2 (remaining_ships_to_place (choose_player false) battleship);
      assert_equal ~printer: string_of_bool all_ships_sunk1 (battleship |> get_player_dict (choose_player true) |> check_all_ships_damaged);
      assert_equal ~printer: string_of_bool all_ships_sunk2 (battleship |> get_player_dict (choose_player false) |> check_all_ships_damaged)
    ) 

let rec repeated_random_insertion player battleship  = 
  if remaining_ships (choose_player player) battleship = 0 then battleship
  else
    try 
      (battleship 
       |> random_ship (choose_player player)
       |> (fun ((x, y), direction, ship) -> 
           insert_ship 
             (x, y) 
             (string_to_direction direction) 
             (string_to_ship ship) 
             (choose_player player) 
             battleship)
       |> (fun result -> 
           match result with 
           | Success b -> b 
           | Failure _ -> failwith "failure"))
      |> repeated_random_insertion player
    with 
    | _ -> repeated_random_insertion player battleship 

let rec repeated_ai_random_insertion battleship  = 
  if remaining_ships (choose_player false) battleship = 0 then battleship
  else
    try 
      (battleship 
       |> randomly_laydown_ships 
       |> (fun ((x, y), direction, ship) -> 
           insert_ship 
             (x, y) 
             (string_to_direction direction) 
             (string_to_ship ship) 
             (choose_player false) 
             battleship)
       |> (fun result -> 
           match result with 
           | Success b -> b 
           | Failure _ -> failwith "failure"))
      |> repeated_ai_random_insertion 
    with 
    | _ -> repeated_ai_random_insertion battleship 


let state_tests = [

]


let battleship_tests = [
  make_battleship_test
    "empty"
    Battleship.empty
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true; (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert one ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    4
    5
    ["Aircraft Carrier"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true; (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert two ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    3
    5
    ["Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true;
  make_battleship_test
    "insert THREE ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")) 
    2
    5
    ["Destroyer"; "Cruiser";]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true; (* because there are no ships placed currently, so vacuously, all are damaged *) (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert four ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")) 
    1
    5
    ["Destroyer";]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true; (* because there are no ships placed currently, so vacuously, all are damaged *) (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert FIve ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")) 
    0
    5
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true; (* because there are no ships placed currently, so vacuously, all are damaged *) (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert one ship p2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    4
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Submarine"]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4)]
    true; (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert two ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    3
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; ]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9)]
    true;
  make_battleship_test
    "insert THREE ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    2
    []
    ["Battleship"; "Destroyer"; ]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1);]
    true;(* because there are no ships placed currently, so vacuously, all are damaged *) (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert four ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    1
    []
    [ "Destroyer"; ]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8)]
    true; (* because there are no ships placed currently, so vacuously, all are damaged *) (* because there are no ships placed currently, so vacuously, all are damaged *)
  make_battleship_test
    "insert FIve ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    0
    []
    []
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "delete after 10 inserts on player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "battleship" (choose_player true))
    1
    0
    ["Battleship"]
    []
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "reinsert battleship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "battleship" (choose_player true)
     |> insert_ship (1, 6) Down Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure"))
    0
    0
    []
    []
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(1, 6); (1, 7); (1, 8); (1, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "remove from player 2 a cruiser"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "cruiser" (choose_player false))
    0
    1
    []
    ["Cruiser"]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "remove from player 1 a destroyer after player 2 remove cruiser"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (fun result -> match result with | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "destroyer" (choose_player true))
    1
    1
    ["Destroyer"]
    ["Cruiser"]
    false 
    false (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); ]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    [(8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "single remove empty"
    (Battleship.empty 
     |> remove_ship "destroyer" (choose_player false))
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true;
  make_battleship_test
    "double remove empty"
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "destroyer" (choose_player true))
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true;
  make_battleship_test
    "double same player 1 empty"
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "destroyer" (choose_player true))
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true;
  make_battleship_test
    "double same player 2 empty"
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "destroyer" (choose_player false))
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true (* because there are no ships placed currently, so vacuously, all are damaged *)
    []
    true;


  (* randomized testing *)
  make_random_battleship_test 
    "insert player1 and player2 randomly"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_random_insertion false
     |> remove_ship "cruiser" (choose_player false))
    0
    1
    []
    ["Cruiser"]
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_random_insertion false
     |> remove_ship "cruiser" (choose_player false)
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_random_insertion false
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "battleship" (choose_player false)
     |> repeated_random_insertion false
     |> remove_ship "destroyer" (choose_player false)
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "battleship" (choose_player false)
     |> repeated_random_insertion false
     |> repeated_random_insertion false
     |> remove_ship "destroyer" (choose_player false)
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> repeated_random_insertion false
     |> repeated_random_insertion false
     |> remove_ship "destroyer" (choose_player false)
     |> repeated_random_insertion false)
    1
    0
    ["Cruiser"]
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> repeated_random_insertion false)
    1
    0
    ["Cruiser"]
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> repeated_random_insertion false
     |> repeated_random_insertion true)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> repeated_random_insertion false
     |> repeated_random_insertion true)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion false
     |> repeated_random_insertion false
     |> remove_ship "destroyer" (choose_player false)
     |> repeated_random_insertion false)
    2
    0
    ["Cruiser"; "Aircraft Carrier"]
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion false)
    2
    0
    ["Cruiser"; "Aircraft Carrier"]
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion true
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion true
     |> repeated_random_insertion true
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion true
     |> repeated_random_insertion false)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly and then remove and insert"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> remove_ship "cruiser" (choose_player true)
     |> remove_ship "aircraftcarrier" (choose_player true)
     |> repeated_random_insertion true
     |> repeated_random_insertion false
     |> repeated_random_insertion true)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly, with player 2 as AI"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_ai_random_insertion)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly, with player 2 as AI"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_ai_random_insertion
     |> remove_ship "cruiser" (choose_player false)
     |> repeated_ai_random_insertion)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "insert player1 and player2 randomly, with player 2 as AI"
    (Battleship.empty 
     |> repeated_random_insertion true
     |> repeated_ai_random_insertion
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "battleship" (choose_player false)
     |> remove_ship "cruiser" (choose_player false)
     |> repeated_ai_random_insertion
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "battleship" (choose_player false)
     |> repeated_ai_random_insertion)
    0
    0
    []
    []
    false
    false;



  (* bounds checking for insertion and removal *)
]

let command_tests = [
  make_command_test 
    "random string"
    "hello"
    InvalidCommand;
  make_command_test 
    "random string with spaces at front"
    "  hello"
    InvalidCommand;
  make_command_test 
    "random string with spaces at back"
    "hello   "
    InvalidCommand;
  make_command_test 
    "random string with spaces at front and back"
    "  hello  "
    InvalidCommand;
  make_command_test 
    "random string with multiplke words spaces at front and back"
    " sasd hello  asd sf"
    InvalidCommand;
  make_command_test 
    "multiple random words"
    " target lolz loz ship"
    InvalidCommand;
  make_command_test 
    "invalid target"
    " target target target target"
    InvalidCommand;
  make_command_test 
    ""
    "  "
    InvalidCommand;
  make_command_test 
    " "
    "  "
    InvalidCommand;


  make_command_test 
    "quit simple"
    "quit"
    Quit;
  make_command_test 
    "quit caps"
    "QuiT"
    Quit;
  make_command_test 
    "quit weird caps"
    "qUit"
    Quit;
  make_command_test 
    "quit with spaces"
    "  qUit  "
    Quit;
  make_command_test 
    "quit with spaces"
    "qUit  "
    Quit;
  make_command_test 
    "quit with spaces"
    "  qUit"
    Quit;
  make_command_test 
    "quit mispelled"
    "  QUite"
    InvalidCommand;


  make_command_test 
    "random simple"
    "random"
    Random;
  make_command_test 
    "random caps"
    "Random"
    Random;
  make_command_test 
    "random caps"
    "Random  1 4 cruiser"
    InvalidCommand;
  make_command_test 
    "random caps"
    "Random remove cruiser"
    InvalidCommand;
  make_command_test 
    "random caps"
    "quit random"
    InvalidCommand;
  make_command_test 
    "random weird caps"
    "RanDOm"
    Random;
  make_command_test 
    "random with spaces"
    "  ranDOM  "
    Random;
  make_command_test 
    "random with spaces"
    "RANDOm "
    Random;
  make_command_test 
    "random with spaces"
    "  rAndOm"
    Random;
  make_command_test 
    "random misspelled"
    "  RrAndOm"
    InvalidCommand;

  make_command_test 
    "finish simple"
    "finish"
    FinishPlacement;
  make_command_test 
    "finish caps"
    "Finish"
    FinishPlacement;
  make_command_test 
    "finish weird caps"
    "FinisH"
    FinishPlacement;
  make_command_test 
    "finish with spaces"
    "  fiNISH  "
    FinishPlacement;
  make_command_test 
    "finish with spaces"
    "FinISh "
    FinishPlacement;
  make_command_test 
    "finish with spaces"
    "  finISH"
    FinishPlacement;
  make_command_test 
    "finish  mispelleds"
    "  fiNNISH"
    InvalidCommand;
  make_command_test 
    "finish remove quit"
    "  fiNISH remove quit"
    InvalidCommand;
  make_command_test 
    "finish remove quit"
    "  clears"
    InvalidCommand;


  make_command_test 
    "remove"
    "  remove"
    InvalidCommand;
  make_command_test 
    "finish remove quit"
    "  remove Clear"
    InvalidCommand;
  make_command_test 
    "remove"
    "  removedestroyer"
    InvalidCommand;
  make_command_test 
    "remove"
    "  RemoveDestroyer"
    InvalidCommand;
  make_command_test 
    "remove something"
    "  remove something"
    InvalidCommand;
  make_command_test 
    "remove something"
    "  remove Destroyed"
    InvalidCommand;
  make_command_test 
    "remove something"
    "  remove destroyer cruiser battleshIp  "
    InvalidCommand;
  make_command_test 
    "remove something ship"
    "  remove battleship"
    (Remove ("battleship"));
  make_command_test 
    "remove something aircraft"
    "  remove aircraftcarrier"
    (Remove ("aircraftcarrier"));
  make_command_test 
    "remove something aircraft space"
    "  remove aircraft carrier"
    InvalidCommand;
  make_command_test 
    "remove something aircraft space"
    "  remove aircraftcarrier  "
    (Remove ("aircraftcarrier"));
  make_command_test 
    "remove something weird caps"
    "  remOve airCraftCarrier"
    (Remove ("aircraftcarrier"));
  make_command_test 
    "remove targets"
    "remove targets"
    InvalidCommand;
  make_command_test 
    "yes remove targets"
    "yes remove targets"
    InvalidCommand;
  make_command_test 
    "yes no remove"
    "yes no remove"
    InvalidCommand;

  make_command_test 
    "yes"
    "  YEs"
    (YesNo(true));
  make_command_test 
    "yes"
    "yes      "
    (YesNo(true));
  make_command_test 
    "yes"
    "Yyes s     "
    InvalidCommand;
  make_command_test 
    "yes"
    "yes targets     "
    InvalidCommand;
  make_command_test 
    "yes"
    "1 1    "
    InvalidCommand;
  make_command_test 
    "no"
    "  No      "
    (YesNo(false));
  make_command_test 
    "no"
    "  NO      "
    (YesNo(false));
  make_command_test 
    "no"
    "  NOP      "
    InvalidCommand;
  make_command_test 
    "yes"
    "yes targets  no   "
    InvalidCommand;
  make_command_test 
    "yes"
    "1 1  no  "
    InvalidCommand;

  make_command_test 
    "target1"
    "target11  "
    InvalidCommand;
  make_command_test 
    "target1"
    "target3 6 "
    InvalidCommand;
  make_command_test 
    "target1"
    "target3 0 "
    InvalidCommand;
  make_command_test 
    "target1"
    "target 3 0 "
    InvalidCommand;
  make_command_test 
    "target1"
    "target 0 0 "
    InvalidCommand;
  make_command_test 
    "target1"
    "target 0 1 "
    InvalidCommand;
  make_command_test 
    "target1"
    "target sda asda a  "
    InvalidCommand;
  make_command_test 
    "target2"
    "  target sda asda "
    InvalidCommand;
  make_command_test 
    "target3"
    "  remove target "
    InvalidCommand;
  make_command_test 
    "target4"
    "  1 1 target "
    InvalidCommand;
  make_command_test 
    "target5"
    "  yes target "
    InvalidCommand;
  make_command_test 
    "target6"
    "  targets "
    InvalidCommand;
  make_command_test 
    "target7"
    "  target battleship "
    InvalidCommand;
  make_command_test 
    "target8"
    "  target Cruiser 1 1 "
    InvalidCommand;
  make_command_test 
    "target9"
    "  target 20 20 "
    InvalidCommand;
  make_command_test 
    "target10"
    "  target 0 20 "
    InvalidCommand;
  make_command_test 
    "target11"
    "  target 1899 1 "
    InvalidCommand;
  make_command_test 
    "target12"
    "  target 0 9 "
    InvalidCommand;
  make_command_test 
    "target13"
    "  target 1 0 "
    InvalidCommand;
  make_command_test 
    "target13"
    "  target 7 0 "
    InvalidCommand;
  make_command_test 
    "target14"
    "  target 3 3 "
    (Target (2, 2));
  make_command_test 
    "target15"
    "  target 0 0  "
    InvalidCommand;
  make_command_test 
    "target16"
    "    target    9     9"
    (Target (8, 8));
  make_command_test 
    "target17"
    "    target    1     4"
    (Target (0, 3));
  make_command_test 
    "target18"
    "target 4         7     "
    (Target (3, 6));
  make_command_test 
    "target19"
    "target 2         1     "
    (Target (1, 0));
  make_command_test 
    "target20"
    "target 4         10     "
    (Target (3, 9));
  make_command_test 
    "target21"
    "target 10         10     "
    (Target (9, 9));
  make_command_test 
    "target22"
    "target 10         4     "
    (Target (9,3));
  make_command_test 
    "target23"
    "target 10         A     "
    InvalidCommand;
  make_command_test 
    "target24"
    "target F         A     "
    InvalidCommand;
  make_command_test 
    "target25"
    "target G         9     "
    (Target (6, 8));
  make_command_test 
    "target26"
    "target 3         H    "
    InvalidCommand;
  make_command_test 
    "target26"
    "target I4  "
    InvalidCommand;
  make_command_test 
    "target26"
    "  targetJ4  "
    InvalidCommand;

  make_command_test 
    "valid"
    "target 3         H    "
    InvalidCommand;
  make_command_test 
    "valid"
    "Valdi valid         H 1 3 cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid 1 3cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid1 3 cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid13cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "target valid 1 3 cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid 0 0  cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid 0 3  cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid 10 0  cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "valid 10 10  cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "10 10 left cruiser   "
    (Valid (9, 9, "left", "cruiser"));
  make_command_test 
    "valid"
    "J 10 left cruiser   "
    (Valid (9, 9, "left", "cruiser"));
  make_command_test 
    "valid"
    "J J left cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "3 J left cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "J 10 left bAttleship   "
    (Valid (9, 9, "left", "battleship"));
  make_command_test 
    "valid"
    "3 10 right bAttleship   "
    (Valid (2, 9, "right", "battleship"));
  make_command_test 
    "valid"
    "1 1 left bAttleship   "
    (Valid (0, 0, "left", "battleship"));
  make_command_test 
    "valid"
    "1 1 up bAttleship   "
    (Valid (0, 0, "up", "battleship"));
  make_command_test 
    "valid"
    "3 10 right bAttleship   "
    (Valid (2, 9, "right", "battleship"));
  make_command_test 
    "valid"
    "310 right bAttleship   "
    InvalidCommand;
  make_command_test 
    "valid"
    "3 8 right bAttleships   "
    InvalidCommand;
  make_command_test 
    "valid"
    "3 8 down subMARINE   "
    (Valid (2, 7, "down", "submarine"));
  make_command_test 
    "valid"
    "3 8 right bAttleships cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "battleship 1 1 downs  "
    InvalidCommand;
  make_command_test 
    "valid"
    "3 8 10 11 right bAttleships cruiser   "
    InvalidCommand;
  make_command_test 
    "valid"
    "target 3 8 10 7 right bAttleships cruiser   "
    InvalidCommand;

]

let suite =
  "test suite for A7"  >::: List.flatten [
    battleship_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite