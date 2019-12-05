open OUnit2
open State
open Battleship
open Command

(*
For running quick tests
1, 1, down, aircraftcarrier
2,2 ,down, battleship
3,3, down, cruiser
4, 4, down, destroyer
5, 5, down, submarine
 *)

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