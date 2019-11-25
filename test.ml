open OUnit2
open State
open Battleship
open Command

let state_tests = [
]


let battleship_tests = [

]

let command_tests = [

]

let suite =
  "test suite for A7"  >::: List.flatten [
    battleship_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite