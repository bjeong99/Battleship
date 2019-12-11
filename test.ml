open OUnit2
open State
open Battleship
open Command
open Hard_ai
open Emoji

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

(** [str_list_to_str lst] concatenates all the elements of [lst]
    together in the same order as [lst] from left ro right, with
    a space as a delimiter. *)
let str_list_to_str lst = 
  List.fold_left (fun init s -> init ^ " " ^ s) "" lst

let int_list_to_str lst = 
  List.fold_left (fun init s -> init ^ "  " ^ (string_of_int (fst s)) ^ ", " ^ 
                                (string_of_int (snd s))) "" lst
(**[make_battleship_test 
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
    bool_coords2] 
    is the OUnit2 test with name [name] that asserts the equality 
    of 
    [num_ships_remain1] and the number of remaining ships player 1 has 
    [num_ships_remain2] and the number of remaining ships player 2 has 
    [ships_remain1] and 
      the  remaining ships player 1 has 
    [ships_remain2] and 
      the remaining ships player 2 has 
    [all_ships_sunk1] and
      if all the ships in battleship are sunk for player 1
    [all_ships_sunk2] and
      if all the ships in battleship are sunk for player 2
    [bool_coords1] and if all coordinates in [coordinates_ships1] are in 
      batteship
    and [bool_coords2] and if all coordinates in [coordinates_ships2]
      are in battleship *)
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
      assert_equal 
        ~printer: string_of_int 
        num_ships_remain1 
        (remaining_ships (choose_player true) battleship);
      assert_equal 
        ~printer: string_of_int 
        num_ships_remain2 
        (remaining_ships (choose_player false) battleship);
      assert_equal 
        ~printer: str_list_to_str 
        ~cmp: cmp_set_like_lists 
        ships_remain1 
        (remaining_ships_to_place (choose_player true) battleship);
      assert_equal 
        ~printer: str_list_to_str 
        ~cmp: cmp_set_like_lists 
        ships_remain2 
        (remaining_ships_to_place (choose_player false) battleship);
      assert_equal 
        ~printer:
          string_of_bool all_ships_sunk1 
        (battleship 
         |> get_player_dict (choose_player true) 
         |> check_all_ships_damaged);
      assert_equal 
        ~printer: string_of_bool 
        all_ships_sunk2 
        (battleship 
         |> get_player_dict (choose_player false)
         |> check_all_ships_damaged);
      assert_equal 
        ~printer: string_of_bool bool_coords1 
        (List.for_all 
           (fun (x, y) -> 
              check_coordinate_in_positions (x, y) 
                (battleship 
                 |> get_player_dict (choose_player true)
                 |> List.map (fun (_, t) -> t) 
                 |> List.flatten))
           coordinates_ships1);
      assert_equal
        ~printer: string_of_bool bool_coords2 
        (List.for_all 
           (fun (x, y) -> 
              check_coordinate_in_positions (x, y) 
                (battleship 
                 |> get_player_dict (choose_player false) 
                 |> List.map (fun (_, t) -> t) 
                 |> List.flatten))
           coordinates_ships2)
    )

(**[make_battleship_test 
    name 
    battleship
    num_ships_remain1
    num_ships_remain2
    ships_remain1
    ships_remain2
    all_ships_sunk1
    all_ships_sunk2] 
    is the OUnit2 test with name [name] that asserts the equality 
    of 
    [num_ships_remain1] and the number of remaining ships player 1 has 
    [num_ships_remain2] and the number of remaining ships player 2 has 
    [ships_remain1] and 
      the  remaining ships player 1 has 
    [ships_remain2] and 
      the remaining ships player 2 has 
    [all_ships_sunk1] and
      if all the ships in battleship are sunk for player 1
    [all_ships_sunk2] and
      if all the ships in battleship are sunk for player 2*)
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
      assert_equal
        ~printer: string_of_int 
        num_ships_remain1 (remaining_ships (choose_player true) battleship);
      assert_equal 
        ~printer: string_of_int 
        num_ships_remain2 (remaining_ships (choose_player false) battleship);
      assert_equal 
        ~printer: str_list_to_str 
        ~cmp: cmp_set_like_lists 
        ships_remain1 
        (remaining_ships_to_place (choose_player true) battleship);
      assert_equal 
        ~printer: str_list_to_str 
        ~cmp: cmp_set_like_lists 
        ships_remain2 
        (remaining_ships_to_place (choose_player false) battleship);
      assert_equal 
        ~printer: string_of_bool 
        all_ships_sunk1 
        (battleship 
         |> get_player_dict (choose_player true) 
         |> check_all_ships_damaged);
      assert_equal 
        ~printer: string_of_bool 
        all_ships_sunk2 
        (battleship 
         |> get_player_dict (choose_player false) 
         |> check_all_ships_damaged)
    ) 

(** [repeated_random_insertion player battleship] repeated adds
    ships for the [player] into [battleship]  randomly until
    all ships are added.  *)
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

(** [repeated_ai_random_insertion battleship] repeated adds
    ships for the ai (player2) into [battleship]  randomly until
    all ships are added.  *)
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

(** [printer_battleship_error (t, error)] is the string representation
    of [t, error] for the printer in [assert_equals]
    when dealing with [Battleship Failure] variants. *)
let printer_battleship_error (t, error) = 
  match error with 
  | BoundsError -> "bounds error"
  | OccupiedTile -> "occupied tile"
  | OutOfShips -> "out of ships"
  | NonexistShip -> "non exist ship : should never be raised"

(** [make_battleship_bounds_test name battleship_failure  battleship error_type]
    is the Ounit2 test with name [name] that asserts the equality of
    [error_type] and [battleship] with the [battleship_failure] action
    that must be a failure.. *)
let make_battleship_bounds_test 
    name
    battleship_failure 
    battleship
    error_type = 
  name >:: (fun _ ->
      assert_equal ~printer: printer_battleship_error
        (battleship, error_type)
        ((function | Success _ -> failwith "error" | Failure (t, e) -> (t, e)) 
           battleship_failure)
    )

(** [make_battleship_damage_test
    name
    player
    battleship
    damage_coords_lst 
    battleship_status_lst 
    all_sunk] is the OUnit2 test named [name] that asserts the equality 
    of the [player]'s [battleship] dictionary with locations changed
    by [damage_coords_lst] with [battleship_status_lst]. 
    It also asserts the equality of [all_sunk] and whether
    all the ships are damaged in the updated battleship for [player]. *) 
let make_battleship_damage_test
    name
    player
    battleship
    damage_coords_lst 
    battleship_status_lst 
    all_sunk = 
  name >:: (fun _ ->
      let final_damage_list = 
        List.fold_left 
          (fun bship coord -> change_to_damage coord bship) 
          (battleship |> get_player_dict (choose_player player))
          (damage_coords_lst) in 
      let cleaned_final_list = 
        final_damage_list
        |> List.map (fun (_, t) -> t)
        |> List.flatten in
      assert_equal 
        ~cmp: cmp_set_like_lists
        battleship_status_lst cleaned_final_list;
      assert_equal
        ~printer: string_of_bool
        all_sunk (check_all_ships_damaged final_damage_list);
    )
(** [get_option_value action] returns a tuple that is an easier representation 
    of State.action. The first two boolean values represent if a ship has been 
    hit and if the ship has been sunk, and the third bool value represents 
    whether the action is a Sucess or a Failure. *)
let get_option_value (action: State.action) = 
  match action with 
  | Success (_, b1, b2) -> (b1, b2, true)
  | Failure (_, error) -> (false, false, false)

(** [make_state_player_test name state player_bool player_win] is an OUnit2 test
    named [name] that compares the various player functions in State with the 
    expected [player_bool] and [player_win] values. It also compares the 
    [target_action] of the targeting function based on the targeted coordinates
    [x] and[y]. *)
let make_state_player_test
    name
    state 
    player_bool 
    player_win 
    target_action
    (x, y)= 
  name >:: (fun _ ->
      let updated_state = match target_ship (x, y) (bool_to_player player_bool) 
                                  state with 
      | Success (t, _ , _) -> t 
      | Failure (t, _) -> failwith "failure" in 
      assert_equal 
        ~printer: string_of_bool player_bool (get_current_player state);
      assert_equal 
        ~printer: string_of_bool player_win 
        (check_victory (bool_to_player player_bool) updated_state);
      assert_equal target_action 
        (get_option_value (target_ship (x, y) (bool_to_player player_bool) 
                             state));
    )


(** [make_state_test 
    name  
    state
    expected_string]
    is an ounit2 test named [name] that compares
    the [state] with the [expected_string] output
    for [player].  
    It also asserts the equality of where the [player] has
    guessed in [state]. *)
let make_state_test 
    name  
    state
    expected_placed_ships1
    expected_guesses1
    expected_placed_ships2
    expected_guesses2 = 
  name >:: (fun _ ->
      assert_equal 
        ~printer: 
          (fun lst -> List.fold_left (fun init s -> init ^ "Start" ^ s) "" lst)
        expected_placed_ships1
        (string_of_player_dict (bool_to_player true) state);
      assert_equal 
        ~printer: 
          (fun lst -> List.fold_left (fun init s -> init ^ "Start" ^ s) "" lst)
        expected_guesses1
        (string_of_guesses (bool_to_player true) state);
      assert_equal 
        ~printer: 
          (fun lst -> List.fold_left (fun init s -> init ^ "Start" ^ s) "" lst)
        expected_placed_ships2
        (string_of_player_dict (bool_to_player false) state);
      assert_equal 
        ~printer: 
          (fun lst -> List.fold_left (fun init s -> init ^ "Start" ^ s) "" lst)
        expected_guesses2
        (string_of_guesses (bool_to_player false) state);
    )

let num_tiles = 100

let board_pairs = create_pairs 10 10 

(** [repeatedly_garget test_result point_acc n targeting_function state] 
    repeatedly targets in a given [state] with a specific [targeting_function] [n]
    number of times and collects the targeted coordinates in [points_acc]. *)
let rec repeatedly_target test_result points_acc n targeting_function state = 
  if n = 0 then 
    test_result && 
    cmp_set_like_lists points_acc points_acc &&
    List.for_all (fun p -> List.mem p board_pairs) points_acc &&
    List.length (points_acc) = num_tiles 
  else 
    let (x, y), new_state = targeting_function state in 
    let new_result = x >= 1 && x <= 10 && y >= 1 && y <= 10 in
    repeatedly_target 
      (new_result && test_result) 
      ((x, y) :: points_acc) 
      (n - 1) targeting_function new_state

(** [make_state_ai_target_test name state targeting_function] 
    is a OUnit test named [name] that asserts the equality of 
    [targeting_function state] is always in board bounds
    running over 100 trials. *)
let make_state_ai_target_test 
    name 
    state
    targeting_function = 
  let test_result = 
    repeatedly_target true [] num_tiles targeting_function state in
  name >:: (fun _ ->
      assert_bool 
        "You targeted a location off the board" 
        test_result
    )

(** [make_state_ai_surrounding name state x y surrounding_pairs_lst] is a 
    OUnit test named [name] that asserts the quality of the 
    get_surrounding_positions function that is used by the medium and hard AI. It 
    enforces the quality of the AI in a given [state] that it can find the next
    positions to target once hitting a ship at cordinates [x] and [y]. *)
let make_state_ai_surrounding
    name
    state
    x 
    y 
    surrounding_pairs_lst= 
  name >:: (fun _ -> 
      assert_equal 
        ~printer: int_list_to_str
        ~cmp: cmp_set_like_lists
        surrounding_pairs_lst
        (get_surrounding_positions (x, y) state);
    )

(** [ocean_row] is a string that represents a row in an empty board. *)
let ocean_row = 
  " " ^ water_wave ^ " " ^ water_wave ^ " " ^ water_wave ^ " " ^ water_wave ^ 
  " " ^ water_wave ^ " " ^ water_wave ^ " " ^ water_wave ^ " " ^ water_wave ^ 
  " " ^ water_wave ^ " " ^ water_wave 

(** [ocean] is a list of [ocean_row]s that represents an empty board. *)
let ocean = 
  [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row;
   ocean_row; ocean_row; ocean_row; ocean_row; ocean_row]

(** [question_row] is a string that represents a row in an empty guess board. *)
let question_row = 
  " " ^ "?" ^ " " ^ "?" ^ " " ^ "?" ^ " " ^ "?" ^ " " ^ "?" ^ " " ^ 
  "?" ^ " " ^ "?" ^ " " ^ "?" ^ " " ^ "?" ^ " " ^ "?" 

(** [no_guesses] is a list of [question_row]s that represents an emtpy guess
    board. *)
let no_guesses = 
  [question_row; question_row; question_row; question_row; question_row;
   question_row; question_row; question_row; question_row; question_row]

(** [empty_player1_dict] returns an empty Battleship.list_t for player 1. *)
let empty_player1_dict = 
  Battleship.empty |> get_player_dict (choose_player true)

(** [empty_player2_dict] returns an empty Battleship.list_T for player 2.*)
let empty_player2_dict = 
  Battleship.empty |> get_player_dict (choose_player false)

(** [one_ship_in_dict] returns a Battleship.list_t that contains only one ship.*)
let one_ship_in_dict= 
  let insert_bs = insert_ship (5, 5) Right Destroyer Player1 Battleship.empty
  in 
  match insert_bs with 
  | Success x -> get_player_dict (choose_player true) x
  | _ -> failwith ""

(** [initial_state] returns an initial state with everything empty for player 1
    and player 2.*)
let initial_state = init_state true false empty_player1_dict empty_player2_dict

(** [one_ship_target_repeat] returns a state in which the one ship had been 
    repeatedly targeted except for one part of the ship. *)
let one_ship_target_repeat n (x,y) player = 
  let init_state = init_state true false one_ship_in_dict one_ship_in_dict in
  let rec loop_to_target n (x, y) updated_state = 
    match n with 
    | 0 -> updated_state
    | _ -> 
      match target_ship (x, y) player updated_state with 
      | Success (t, _, _) -> loop_to_target (n-1) (x + 1, y) t
      | Failure (t, _) -> failwith "failure"
  in 
  loop_to_target n (x,y) init_state

let state_tests = [
  make_state_player_test
    "init"
    initial_state
    true
    true
    (false, false, true)
    (5,5);

  make_state_player_test
    "test update_player"
    (update_player initial_state)
    false
    true
    (false, false, true)
    (5,5);

  make_state_player_test
    "test with non-empty"
    (init_state true false one_ship_in_dict one_ship_in_dict)
    true
    false
    (true, false, true)
    (5,5);

  make_state_player_test
    "test with non_empty and one hit"
    (one_ship_target_repeat 1 (6,5) Player1)
    true
    false
    (true, false,true)
    (5,5);

  make_state_player_test
    "test with non_empty and all sunk"
    (one_ship_target_repeat 2 (6,5) Player1)
    true
    true
    (true, true, true)
    (5,5);

  make_state_test 
    "empty state"
    (State.init_state true false 
       empty_player1_dict empty_player2_dict
    )
    ocean
    no_guesses
    ocean
    no_guesses;

  make_state_test
    "one ship"
    (State.init_state true false one_ship_in_dict empty_player2_dict)
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses
    ocean
    no_guesses;

  make_state_test
    "one ship for both players"
    (State.init_state true false one_ship_in_dict one_ship_in_dict)
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses;

  make_state_test
    "one ship hit once"
    (one_ship_target_repeat 1 (5,5) Player1)
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    [question_row; question_row; question_row; question_row; question_row; 
     " ? ? ? ? ? X ? ? ? ?"; question_row; question_row; question_row; 
     question_row]
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 🔥 D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses;

  make_state_test
    "one ship hit all"
    (one_ship_target_repeat 4 (5,5) Player1)
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    [question_row; question_row; question_row; question_row; question_row; 
     " ? ? ? ? ? X X X O ?"; question_row; question_row; question_row; 
     question_row]
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 🔥 🔥 🔥 🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses;

  make_state_test
    "one ship hit all player2"
    (one_ship_target_repeat 4 (5,5) Player2)
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 🔥 🔥 🔥 🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    no_guesses
    [ocean_row; ocean_row; ocean_row; ocean_row; ocean_row; 
     " 🌊 🌊 🌊 🌊 🌊 D  D  D  🌊 🌊"; ocean_row; ocean_row; ocean_row; 
     ocean_row]
    [question_row; question_row; question_row; question_row; question_row; 
     " ? ? ? ? ? X X X O ?"; question_row; question_row; question_row; 
     question_row];    

  make_state_ai_target_test
    "empty easy AI target"
    (State.initialize_ai true false 
       empty_player1_dict empty_player2_dict
    )
    target_ai;

  make_state_ai_surrounding
    "empty medium AI target"
    (State.initialize_ai true false 
       empty_player1_dict empty_player2_dict)
    5
    5
    [];

  make_state_ai_surrounding
    "one ship medium/hard AI target hit"
    (State.initialize_ai true false 
       one_ship_in_dict empty_player2_dict)
    6
    6
    [(6, 5);(6, 7);(5, 6);(7, 6)];

  make_state_ai_surrounding
    "one ship medium/hard AI target miss"
    (State.initialize_ai true false 
       one_ship_in_dict empty_player2_dict)
    10
    10
    [];

  make_state_ai_target_test
    "empty medium AI target"
    (State.initialize_ai true false 
       (Battleship.empty |> get_player_dict (choose_player true))
       (Battleship.empty |> get_player_dict (choose_player false))
    )
    target_medium_ai;
  make_state_ai_target_test
    "empty hard AI target"
    (State.initialize_ai true false 
       (Battleship.empty |> get_player_dict (choose_player true))
       (Battleship.empty |> get_player_dict (choose_player false))
    )
    target_hard_ai;
  (*Modified INsane so that a oocation can always be targeted *)
  make_state_ai_target_test
    "empty insane AI target"
    (State.initialize_ai true false 
       (Battleship.empty |> get_player_dict (choose_player true))
       (Battleship.empty |> get_player_dict (choose_player false))
    )
    target_insane_ai;
]

let battleship_tests = [
  make_battleship_damage_test
    "multiple inserts and samages and one with different players"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (5, 8) Left Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 4) Right Cruiser (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(9, 2); (9, 3);(9, 4); (9, 5); (9, 6)]
    [(5, 8, Undamaged); (4, 8, Undamaged); (3, 8, Undamaged);
     (9, 2, Damaged); (9, 3, Damaged);
     (9, 4, Damaged); (9, 5, Damaged);
     (9, 6, Damaged)]
    false;
  make_battleship_damage_test
    "multiple inserts and samages and one with different players"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (5, 8) Left Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    [(6, 3); (6, 2); (6, 1); (6, 0); (9, 4); (9, 6)]
    [(6, 3, Damaged); (6, 2, Damaged);
     (6, 1, Damaged); (6, 0, Damaged);]
    true;
  make_battleship_damage_test
    "multiple inserts and samages and one ships sunk player 1"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    [(6, 3); (6, 2); (6, 1); (6, 0); (9, 4); (9, 6)]
    [(6, 3, Damaged); (6, 2, Damaged);
     (6, 1, Damaged); (6, 0, Damaged);
     (9, 2, Undamaged); (9, 3, Undamaged);
     (9, 4, Damaged); (9, 5, Undamaged);
     (9, 6, Damaged)]
    false;
  make_battleship_damage_test
    "multiple inserts and samages and one ships sunk player 2"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    [(6, 3); (6, 2); (6, 1); (6, 0); (9, 4); (9, 6)]
    [(6, 3, Damaged); (6, 2, Damaged);
     (6, 1, Damaged); (6, 0, Damaged);
     (9, 2, Undamaged); (9, 3, Undamaged);
     (9, 4, Damaged); (9, 5, Undamaged);
     (9, 6, Damaged)]
    false;
  make_battleship_damage_test
    "multiple inserts and samages but no ship sunk player 1"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    [(6, 3); (6, 0); (9, 4); (9, 6)]
    [(6, 3, Damaged); (6, 2, Undamaged);
     (6, 1, Undamaged); (6, 0, Damaged);
     (9, 2, Undamaged); (9, 3, Undamaged);
     (9, 4, Damaged); (9, 5, Undamaged);
     (9, 6, Damaged)]
    false;
  make_battleship_damage_test
    "multiple inserts and samages but no ship sunk player 2"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (9, 2) Down AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    [(6, 3); (6, 0); (9, 4); (9, 6)]
    [(6, 3, Damaged); (6, 2, Undamaged);
     (6, 1, Undamaged); (6, 0, Damaged);
     (9, 2, Undamaged); (9, 3, Undamaged);
     (9, 4, Damaged); (9, 5, Undamaged);
     (9, 6, Damaged)]
    false;
  make_battleship_damage_test
    "insert player 1 and damage part of ship"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(6, 3); (6, 0)]
    [(6, 3, Damaged); (6, 2, Undamaged);
     (6, 1, Undamaged); (6, 0, Damaged)]
    false;
  make_battleship_damage_test
    "insert player 2 and damage part of ship"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(6, 3); (6, 1); (6, 0)]
    [(6, 3, Damaged); (6, 2, Undamaged);
     (6, 1, Damaged); (6, 0, Damaged)]
    false;
  make_battleship_damage_test
    "insert player 2 and damage entire ship"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(6, 3); (6, 2); (6, 1); (6, 0)]
    [(6, 3, Damaged); (6, 2, Damaged);
     (6, 1, Damaged); (6, 0, Damaged)]
    true;
  make_battleship_damage_test
    "insert player 1 and damage entire ship"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(6, 3); (6, 2); (6, 1); (6, 0)]
    [(6, 3, Damaged); (6, 2, Damaged);
     (6, 1, Damaged); (6, 0, Damaged)]
    true;
  make_battleship_damage_test
    "insert player 1"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(2, 3); (4, 4)]
    [(6, 3, Undamaged); (6, 2, Undamaged);
     (6, 1, Undamaged); (6, 0, Undamaged)]
    false;
  make_battleship_damage_test
    "insert player 2"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(2, 3); (4, 4)]
    [(6, 3, Undamaged); (6, 2, Undamaged);
     (6, 1, Undamaged); (6, 0, Undamaged)]
    false;
  make_battleship_damage_test
    "opposite player"
    true
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(2, 3); (4, 4)]
    [] (* player 1 no ships placed *)
    true;(*vacuously true *)
  make_battleship_damage_test
    "opposite player"
    false
    (Battleship.empty
     |> insert_ship (6, 3) Up Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    [(2, 3); (4, 4)]
    [] (* player 2 no ships placed *)
    true;(*vacuously true *)
  make_battleship_damage_test
    "empty player 1"
    true
    Battleship.empty 
    [(2, 3); (4, 4)]
    []
    true;(*vacuously true *)
  make_battleship_damage_test
    "empty player 2"
    false
    Battleship.empty 
    [(2, 3); (4, 4)]
    []
    true; (*vacuously true *)
  make_battleship_test
    "empty"
    Battleship.empty
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true 
    true 
    []
    true 
    []
    true; 
  make_battleship_test
    "insert one ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    4
    5
    ["Aircraft Carrier"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true 
    [(4, 9); (5, 9); (6, 9); (7, 9)]
    true 
    []
    true; 
  make_battleship_test
    "insert two ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    3
    5
    ["Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7);]
    true 
    []
    true;
  make_battleship_test
    "insert THREE ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    2
    5
    ["Destroyer"; "Cruiser";]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); 
     (2, 5); (2, 6); (2, 7);(0, 1); (0, 0)]
    true 
    []
    true;  
  make_battleship_test
    "insert four ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    1
    5
    ["Destroyer";]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5);
     (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6)]
    true 
    []
    true;  
  make_battleship_test
    "insert FIve ship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    0
    5
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    false 
    true 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); 
     (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6);
     (9, 1); (9, 2); (9, 3)]
    true 
    []
    true;  
  make_battleship_test
    "insert one ship p2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    4
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Submarine"]
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); 
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4)]
    true; 
  make_battleship_test
    "insert two ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    3
    []
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; ]
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); 
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9)]
    true;
  make_battleship_test
    "insert THREE ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    2
    []
    ["Battleship"; "Destroyer"; ]
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); 
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); 
     (0, 2); (0, 1);]
    true; 
  make_battleship_test
    "insert four ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    1
    []
    [ "Destroyer"; ]
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); 
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); 
     (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8)]
    true;  
  make_battleship_test
    "insert FIve ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    0
    []
    []
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); (2, 6); 
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3);
     (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "delete after 10 inserts on player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "battleship" (choose_player true))
    1
    0
    ["Battleship"]
    []
    false 
    false 
    [(2, 3); (2, 4); (2, 5); (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); 
     (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true 
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); 
     (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "reinsert battleship"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "battleship" (choose_player true)
     |> insert_ship (1, 6) Down Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    0
    0
    []
    []
    false 
    false (* because there are no ships placed currently, 
             so vacuously, all are damaged *)
    [(1, 6); (1, 7); (1, 8); (1, 9); (2, 3); (2, 4); (2, 5); (2, 6);
     (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently, 
            so vacuously, all are damaged *)
    [(2, 4); (3, 4); (4, 4); (8, 9); (9, 9); (0, 5); (0, 4); (0, 3); 
     (0, 2); (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "remove from player 2 a cruiser"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "cruiser" (choose_player false))
    0
    1
    []
    ["Cruiser"]
    false 
    false (* because there are no ships placed currently, 
             so vacuously, all are damaged *)
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5); 
     (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); 
     (5, 6); (9, 1); (9, 2); (9, 3)]
    true (* because there are no ships placed currently,
            so vacuously, all are damaged *)
    [(8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); 
     (0, 1); (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "remove from player 1 a destroyer after player 2 remove cruiser"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function| Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function| Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "destroyer" (choose_player true))
    1
    1
    ["Destroyer"]
    ["Cruiser"]
    false 
    false 
    [(4, 9); (5, 9); (6, 9); (7, 9); (2, 3); (2, 4); (2, 5);
     (2, 6); (2, 7);(0, 1); (0, 0); (7, 6); (6, 6); (5, 6); ]
    true 
    [(8, 9); (9, 9); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); 
     (3, 8); (2, 8); (1, 8); (0, 8); (7, 7); (7, 8); (7, 9)]
    true;
  make_battleship_test
    "single remove empty"
    (Battleship.empty 
     |> remove_ship "destroyer" (choose_player false))
    5
    5
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    ["Aircraft Carrier"; "Battleship"; "Destroyer"; "Cruiser"; "Submarine"]
    true 
    true 
    []
    true 
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
    true 
    true 
    []
    true 
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
    true 
    true 
    []
    true 
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
    true 
    true 
    []
    true 
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
    "switch order"
    (Battleship.empty 
     |> repeated_random_insertion false
     |> repeated_random_insertion true)
    0
    0
    []
    []
    false
    false;
  make_random_battleship_test 
    "switch order"
    (Battleship.empty 
     |> repeated_random_insertion false
     |> remove_ship "submarine" (choose_player true)
     |> repeated_random_insertion true)
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

  (* RANDOM INSERTION, REMOVAl and Random *)
  make_random_battleship_test 
    "player 1insert, random then delete"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
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
  make_random_battleship_test 
    "player 2insert, random then delete"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
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
  make_random_battleship_test 
    "player 1 insert, delte, random then delete"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "battleship" (choose_player true)
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
  make_random_battleship_test 
    "player 2 insert, delte, random then delete"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "battleship" (choose_player false)
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
  make_random_battleship_test 
    "player 1 delete, insert random "
    (Battleship.empty 
     |>remove_ship "cruiser" (choose_player true)
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
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
  make_random_battleship_test 
    "player 2 delete, insert random "
    (Battleship.empty 
     |>remove_ship "cruiser" (choose_player false)
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
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
  make_random_battleship_test 
    "mix player 1 and player 2 "
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player false)
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> repeated_random_insertion true
     |> repeated_ai_random_insertion
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
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
  make_random_battleship_test 
    "mix player 1 and player 2 more"
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player false)
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> insert_ship (9, 9) Up Destroyer (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> remove_ship "battleship" (choose_player true)
     |> repeated_random_insertion true
     |> remove_ship "battleship" (choose_player false)
     |> repeated_ai_random_insertion
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
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
  make_random_battleship_test 
    "mix player 1 and player 2 more"
    (Battleship.empty 
     |> insert_ship (0, 4) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "battleship" (choose_player false)
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> insert_ship (9, 9) Up Destroyer (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> remove_ship "battleship" (choose_player true)
     |> insert_ship (0, 4) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> repeated_random_insertion true
     |> remove_ship "battleship" (choose_player false)
     |> repeated_ai_random_insertion
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
     |> remove_ship "submarine" (choose_player false)
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
  make_random_battleship_test 
    "mix player 1 and player 2 more"
    (Battleship.empty 
     |> remove_ship "cruiser" (choose_player false)
     |> insert_ship (4, 9) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> insert_ship (9, 9) Up Destroyer (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> remove_ship "destroyer" (choose_player false)
     |> remove_ship "battleship" (choose_player true)
     |> repeated_random_insertion true
     |> remove_ship "battleship" (choose_player false)
     |> repeated_ai_random_insertion
     |> remove_ship "submarine" (choose_player true)
     |> remove_ship "submarine" (choose_player true)
     |> remove_ship "submarine" (choose_player true)
     |> repeated_ai_random_insertion
     |> remove_ship "cruiser" (choose_player false)
     |> remove_ship "battleship" (choose_player false)
     |> repeated_ai_random_insertion)
    1
    0
    ["Submarine"]
    []
    false
    false;
  (* bounds checking for insertion and removal *)
  make_battleship_bounds_test
    "insert player 1 on a location off the board"
    (Battleship.empty 
     |> insert_ship (9, 9) Right Cruiser (choose_player true) ) 
    Battleship.empty 
    BoundsError;
  make_battleship_bounds_test
    "insert player 1 on a location off the board"
    (Battleship.empty 
     |> insert_ship (9, 9) Down Cruiser (choose_player true) )
    Battleship.empty  
    BoundsError;
  make_battleship_bounds_test
    "insert player 1 on a location off the board"
    (Battleship.empty 
     |> insert_ship (0, 0) Left Cruiser (choose_player true) ) 
    Battleship.empty 
    BoundsError;
  make_battleship_bounds_test
    "insert player 1 on a location off the board"
    (Battleship.empty 
     |> insert_ship (0, 0) Up Cruiser (choose_player true) ) 
    Battleship.empty 
    BoundsError;
  make_battleship_bounds_test
    "insert player 2 on a location off the board"
    (Battleship.empty 
     |> insert_ship (8, 4) Right Cruiser (choose_player false) ) 
    Battleship.empty 
    BoundsError;
  make_battleship_bounds_test
    "insert player 2 on a location off the board"
    (Battleship.empty 
     |> insert_ship (3, 6) Down AircraftCarrier (choose_player false) )
    Battleship.empty  
    BoundsError;
  make_battleship_bounds_test
    "insert player 2 on a location off the board"
    (Battleship.empty 
     |> insert_ship (1, 7) Left Battleship (choose_player false) ) 
    Battleship.empty 
    BoundsError;
  make_battleship_bounds_test
    "insert player 2 on a location off the board"
    (Battleship.empty 
     |> insert_ship (9, 0) Up Submarine (choose_player false) ) 
    Battleship.empty 
    BoundsError;

  make_battleship_bounds_test
    "legal then illegal player 2"
    (Battleship.empty 
     |> insert_ship (9, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (5, 8) Down AircraftCarrier (choose_player false)) 
    (Battleship.empty 
     |> insert_ship (9, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    BoundsError;
  make_battleship_bounds_test
    "legal then illegal player 1"
    (Battleship.empty 
     |> insert_ship (9, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (5, 8) Down AircraftCarrier (choose_player true)) 
    (Battleship.empty 
     |> insert_ship (9, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    BoundsError;
  make_battleship_bounds_test
    "insert one ship p2 and error on p2 big"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 5) Right Cruiser (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    BoundsError;
  make_battleship_bounds_test
    "insert one ship p1 and error on p1 big"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 5) Right Cruiser (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    BoundsError;

  make_battleship_bounds_test
    "insert four ship big error"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (1, 2) Left Destroyer (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    BoundsError;  

  make_battleship_bounds_test
    "insert four ship big error Player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (1, 2) Left Destroyer (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    BoundsError;  

  make_battleship_bounds_test
    "insert five ship remove error"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "Cruiser" (choose_player true)
     |> insert_ship (9, 6) Right Cruiser (choose_player true)) 
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "Cruiser" (choose_player true)) 
    BoundsError; 

  make_battleship_bounds_test
    "insert five ship remove error Player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "Cruiser" (choose_player false)
     |> insert_ship (9, 6) Right Cruiser (choose_player false)) 
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> remove_ship "Cruiser" (choose_player false)) 
    BoundsError; 

  make_battleship_bounds_test
    "out of ships "
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 9) Up Cruiser (choose_player false)) 
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player false)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    OutOfShips; 

  make_battleship_bounds_test
    "out of ships "
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Down AircraftCarrier (choose_player true)) 
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")) 
    OutOfShips; 

  make_battleship_bounds_test
    "insert FIve ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 9) Up Destroyer (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    OutOfShips;
  make_battleship_bounds_test
    "insert FIve ship player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 9) Up Destroyer (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (2, 3) Down AircraftCarrier (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (0, 1) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 6) Left Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (9, 1) Down Destroyer (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (2, 4) Right Cruiser (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (8, 9) Right Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (0, 5) Up AircraftCarrier (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (3, 8) Left Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure")
     |> insert_ship (7, 7) Down Destroyer (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    OutOfShips;

  make_battleship_bounds_test
    "same loc same ship player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Right Battleship (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OutOfShips;
  make_battleship_bounds_test
    "same loc same ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Right Battleship (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OutOfShips;
  make_battleship_bounds_test
    "same loc diff ship player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Right Cruiser (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "same loc diff ship player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Right Cruiser (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "same loc diff ship diff direction player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player true) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "same loc diff ship diff direction player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "opp player no mistake"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up AircraftCarrier (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "opp player no mistake p 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up AircraftCarrier (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "loc overlap on Aircraft carrier and cruiser player 1"
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (6, 7) Left AircraftCarrier (choose_player true))
    (Battleship.empty 
     |> insert_ship (4, 9) Right Battleship (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Cruiser (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") )
    OccupiedTile;
  make_battleship_bounds_test
    "loc overlap on submarine and destroyer player 2"
    (Battleship.empty 
     |> insert_ship (4, 9) Up Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (3, 8) Right Destroyer (choose_player false))
    (Battleship.empty 
     |> insert_ship (4, 9) Up Submarine (choose_player false) 
     |> (function | Success b -> b | Failure _ -> failwith "failure") 
     |> insert_ship (4, 9) Up Submarine (choose_player true)
     |> (function | Success b -> b | Failure _ -> failwith "failure"))
    OccupiedTile;
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