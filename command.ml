(* general command constants *)
let c_TARGET = "target"
let c_REMOVE = "remove"
let c_FINISH = "finish"
let c_RANDOM = "random"
let c_QUIT = "quit"

(* affirmative constants *)
let c_YES = "yes"
let c_NO = "no"

(* difficulty constants *)
let c_EASY = "easy"
let c_MEDIUM = "medium"
let c_HARD = "hard"
let c_INSANE = "insane"

let c_DIRECTION_LIST = [
  "up";
  "down";
  "left";
  "right";
]

let c_SHIPS_LIST = [
  "battleship";
  "aircraftcarrier";
  "cruiser";
  "destroyer";
  "submarine";
]

(** [string_to_char acc s] is a list of chars 
    that correspond to the string [s] in the same ordering as [s]. 
    For example, [hello] is [['h'; 'e'; 'l'; 'l'; 'o']]. *)
let rec string_to_char acc s = 
  match s.[0] with
  | c -> string_to_char (c :: acc) (String.sub s 1 (String.length s - 1)) 
  | exception (Invalid_argument _) -> List.rev acc


(* START CITATION:
   https://stackoverflow.com/questions/49184057/
   does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c 
   I cite ochatron for the .. solution which I had no idea about, 
   specifically for the code in is_alpha and is_digit. 
   All the work below is fully attributed to Ochatron. *)

(** [is_alpha c] is [true] is [c] is an English character, upper or lower
    cased. [false] otherwise. *)
let is_alpha c =
  match c with
  | 'a' .. 'z' 
  | 'A' .. 'Z' -> true 
  | _ -> false

(** [is_difit c] is [true] is [c] is an digit. [false] otherwise. *)
let is_digit c = 
  match c with
  | '0' .. '9' -> true 
  | _ -> false

(* END CITATION *)

(** [process_number n] is [true] iff the [n] is comprised only of digits. r*)
let process_number n = 
  n |> string_to_char [] |> List.for_all is_digit

(** [char_to_coord n] translates a digit or an alphabetical lenght one string
    to a digit. 
    Specifically, A or a is 1, B or b is 2, etc...
    Requires: [n] is a string with length 1. *)
let char_to_coord n = 
  match n with 
  | "A"| "a" | "1" -> "1"
  | "B"| "b" | "2" -> "2"
  | "C"| "c" | "3" -> "3"
  | "D"| "d" | "4" -> "4"
  | "E"| "e" | "5" -> "5"
  | "F"| "f" | "6" -> "6"
  | "G"| "g" | "7" -> "7"
  | "H"| "h" | "8" -> "8"
  | "I"| "i" | "9" -> "9"
  | "J"| "j" | "10" -> "10"
  | _ -> "not a digit"

let process_direction d = 
  List.mem d c_DIRECTION_LIST

let process_ship_name s =
  List.mem s c_SHIPS_LIST

let process_string s = 
  string_to_char [] s |> List.for_all is_alpha

type command = 
  | Quit
  | InvalidCommand
  | Valid of int * int * string * string
  | Target of int * int
  | YesNo of bool
  | Remove of string
  | FinishPlacement
  | Random

let parse_cleaned str_lst = 
  match str_lst with
  | x :: y :: direction :: ship :: [] ->
    let x_digit = char_to_coord x in 
    if process_number x_digit && 
       process_number y && 
       process_string direction && 
       process_string ship &&
       process_direction direction &&
       process_ship_name ship 
    then Valid (int_of_string x_digit - 1, int_of_string y - 1, direction, ship)
    else InvalidCommand
  | target :: x :: y :: [] ->
    let x_digit = char_to_coord x in
    if process_number x_digit && 
       process_number y &&
       target = c_TARGET
    then Target (int_of_string x_digit - 1, int_of_string y - 1)
    else InvalidCommand
  | remove :: ship :: [] ->
    if remove = c_REMOVE &&
       process_string ship &&
       process_ship_name ship 
    then Remove (ship)
    else InvalidCommand
  | single_word_command :: [] ->
    if single_word_command = c_FINISH 
    then FinishPlacement
    else if single_word_command = c_RANDOM 
    then Random
    else InvalidCommand
  | _ -> InvalidCommand


let parse_elements str_lst = 
  match str_lst with
  | x :: y :: direction :: ship :: [] ->
    let x_digit = char_to_coord x in 
    if process_number x_digit && 
       process_number y && 
       process_string direction && 
       process_string ship &&
       process_direction direction &&
       process_ship_name ship 
    then Valid (int_of_string x_digit - 1, int_of_string y - 1, direction, ship)
    else InvalidCommand
  | target :: x :: y :: [] ->
    let x_digit = char_to_coord x in
    if process_number x_digit && 
       process_number y &&
       target = c_TARGET
    then Target (int_of_string x_digit - 1, int_of_string y - 1)
    else InvalidCommand
  | remove :: ship :: [] ->
    if remove = c_REMOVE &&
       process_string ship &&
       process_ship_name ship 
    then Remove (ship)
    else InvalidCommand
  | single_word_command :: [] ->
    if single_word_command = c_FINISH 
    then FinishPlacement
    else if single_word_command = c_RANDOM 
    then Random
    else InvalidCommand
  | _ -> InvalidCommand



let parse_quit str = 
  if str = c_QUIT then Quit 
  else 
    str 
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun elt -> elt <> "")
    |> parse_elements

let parse_affirmative str = 
  if str = c_YES then YesNo true
  else if str = c_NO then YesNo false
  else parse_quit str

let parse str = 
  str 
  |> String.lowercase_ascii
  |> String.trim
  |> parse_affirmative



type difficulty = 
  | Easy
  | Medium
  | InvalidDifficulty

let check_difficulty str = 
  if str  = c_EASY then Easy
  else if str = c_MEDIUM then Medium
  else InvalidDifficulty

let parse_difficulty str = 
  str 
  |> String.lowercase_ascii
  |> String.trim
  |> check_difficulty


