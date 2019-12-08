open OUnit2
open State
open Battleship
open Command
open Hard_ai

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

let state_tests = [

]


let battleship_tests = [

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