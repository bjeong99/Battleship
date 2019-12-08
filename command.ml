(* general command constants *)
let c_TARGET = "target"
let c_REMOVE = "remove"
let c_FINISH = "finish"
let c_RANDOM = "random"
let c_QUIT = "quit"
let c_USE = "use"
let c_POWERUPS = "powerups"

(* affirmative constants *)
let c_YES = "yes"
let c_NO = "no"

(* difficulty constants *)
let c_EASY = "easy"
let c_MEDIUM = "medium"
let c_HARD = "hard"
let c_INSANE = "insane"

(* delimiters for user to apply *)
let c_COMMA = ','
let c_SPACE = ' '
let c_COLON = ':'

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

let c_POWERUPS_LIST = [
  "squarehit" ;
  "rehit" ;
  "instakill" ;
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

let process_powerup p =
  List.mem p c_POWERUPS_LIST

(** [process_direction d] is [true] iff [d] is in [c_DIRECTION_LIST]. *)
let process_direction d = 
  List.mem d c_DIRECTION_LIST

(** [process_ship_name s] is [true] iff [s] is in [c_SHIPS_LIST]. *)
let process_ship_name s =
  List.mem s c_SHIPS_LIST

(** [process_string s] is [true] iff 
    [s] is composed of only alphabetical elements.*)
let process_string s = 
  string_to_char [] s |> List.for_all is_alpha

(* [command] represents what a player could type in during
   the laying down ships phase, as well as the targeting phase 

   Quit : represents a command to end the game. 
   InvalidCommand : a malformed command not part of the game. 
   Valid x y direction ship : a command to lay down a ship at coordinate
    (x, y) in direction [direction] with ship [ship].
    Does not guarantee that the ship can actually be laid down at 
    that location (x, y).

   Target (x, y) : represents a player targeting a location (x, y)
    on the enemy board. Does not guarantee that (x, y) can actually
    be targeted.

   YesNo b : represents affirmative if [b] is [true], [false] otherwise. 
   Remove s : represents removing a ship [s] from the board. Does not guarantee
    that the ship [s] can actually be removed. 

   FinishPlacement : represents a command to finish the laying down ships
    phase. Does not guarantee that the command will be carried out. 

   Random : represents a command to finish laying down the remainder of the
    ships for that player automatically, using computer randomly generated
    coordinates. 

   NOTE: A valid command refers not to [Valid] but to any command that is not
    [Invalid]*)
type command = 
  | Quit
  | InvalidCommand
  | Valid of int * int * string * string
  | Target of int * int
  | YesNo of bool
  | Remove of string
  | FinishPlacement
  | Random
  | Use of int * int * string
  | Powerups

(** [parse_lay_down_ship x y direction ship] is
    [Valid (x - 1, y - 1, direction ship] if
    [x] and [y] are numbers and
    [direction] is a direction type and 
    [ship] is a ship type
    else [InvalidCommand]. *)
let parse_lay_down_ship x y direction ship = 
  let x_digit = char_to_coord x in 
  if process_number x_digit && 
     process_number y && 
     process_string direction && 
     process_string ship &&
     process_direction direction &&
     process_ship_name ship 
  then Valid (int_of_string x_digit - 1, int_of_string y - 1, direction, ship)
  else InvalidCommand

let parse_use x y use powerup =
  let x_digit = char_to_coord x in 
  if process_number x_digit &&
     process_number y &&
     use = c_USE &&
     process_powerup powerup
  then Use (int_of_string x_digit - 1, int_of_string y - 1, powerup)
  else InvalidCommand

(** [parse_target x y target] is 
    [Target (x - 1, y - 1)] if
    [x] and [y] are valid numbers
    else [InvalidCommand]. *)
let parse_target x y target = 
  let x_digit = char_to_coord x in
  if process_number x_digit && 
     process_number y &&
     target = c_TARGET
  then Target (int_of_string x_digit - 1, int_of_string y - 1)
  else InvalidCommand

(** [parse_remove remove ship] is [Remove ship] if
    [remove] is the remove string and [ship] is a valid ship type
    else [InvalidCommand]. *)
let parse_remove remove ship = 
  if remove = c_REMOVE &&
     process_string ship &&
     process_ship_name ship 
  then Remove (ship)
  else InvalidCommand

(** [parse_single_word word] is 
    [FinishPlacement] if [word] is the finish string,
    [Random] if [word] is the random string,
    [YesNo true] if [word] is the yes string,
    [YesNo false] if [word] is the no string,
    [Quit] if [word] is the quit string
    else [InvalidCommand]. *)
let parse_single_word word = 
  print_endline "this is word";
  print_endline word;
  print_endline c_RANDOM;
  if word = c_FINISH then FinishPlacement
  else if word = c_RANDOM then Random
  else if word = c_YES then YesNo true
  else if word = c_NO then YesNo false
  else if word = c_QUIT then Quit
  else if word = c_POWERUPS then Powerups
  else let () = print_endline "Invalid"; in InvalidCommand

(** [cleaned_to_command str_lst] will parse [str_lst] into
    a valid command or an [InvalidCommand] *)
let cleaned_to_command str_lst = 
  match str_lst with
  | x :: y :: direction :: ship :: [] ->
    parse_lay_down_ship x y direction ship
  | target :: x :: y :: [] ->
    parse_target x y target
  | remove :: ship :: [] ->
    parse_remove remove ship
  | use :: p :: at :: x :: y :: [] ->
    parse_use x y use p
  | word :: [] ->
    parse_single_word word
  | _ -> InvalidCommand

(** [clean_str str] is [str] 
    with leading and trailing spaces removed, and
    turned lowercase and 
    divided on spaces into a list of substrings and
    with all empty strings in that list removed. *)
let clean_str str =   
  str
  |> String.lowercase_ascii
  |> String.trim
  |> String.split_on_char c_SPACE
  |> List.map String.trim
  |> List.filter (fun elt -> elt <> "")

(** [parse str] is the [command] [str] corresponds to. *)
let parse str = 
  str |> clean_str |> cleaned_to_command

(** [difficulty] represents the difficulty of the AI, which is always player 2. 
    [Easy] is an AI that always guesses randomly. 
    [Medium] is an AI that guesses randomly until it finds a ship,
    then targets around that ship. 
    [Hard] is an improved [Medium] that minimizes the number of shots
      by choosing a direction to target. 
    [Insane] is an algorithm by so and so... 
    [InvalidDifficulty] is an invalid command for a difficulty. *)
type difficulty = 
  | Easy
  | Medium
  | Hard
  | Insane
  | InvalidDifficulty

(** [check_difficulty str] is what [difficulty] [str] 
    corresponds to.  *)
let check_difficulty str = 
  if str = c_EASY then Easy
  else if str = c_MEDIUM then Medium
  else if str = c_HARD then Hard
  else InvalidDifficulty

(** [parse_difficulty str] is the [difficulty] corresponding to [str]. *)
let parse_difficulty str = 
  str 
  |> String.lowercase_ascii
  |> String.trim
  |> check_difficulty