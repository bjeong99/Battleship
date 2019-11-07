type command = 
  | Quit
  | InvalidCommand
  | Valid of int * int * string * string
  | Target of int * int
  (*| Ingame of int * int * string * string*)

let c_TARGET = "target"

let rec string_to_char acc s = 
  match s.[0] with
  | c -> string_to_char (c :: acc) (String.sub s 1 (String.length s - 1)) 
  | exception (Invalid_argument _) -> List.rev acc

let rec char_list_to_str char_lst acc = 
  match char_lst with
  | [] -> acc
  | h :: t -> char_list_to_str t (acc ^ Char.escaped h)

let rec beginnings_equal sub s = 
  match sub, s with
  | [], [] -> true
  | [], _ -> true
  | h1 :: t1, h2 :: t2 -> 
    if h1 = h2 then beginnings_equal t1 t2
    else false
  | _ -> false

(*
(** [beginning_contains s1 s2] is [true] iff [s2] begins
    exactly with [s1]. *)
let beginning_contains game_phase s = 
  let sub_chars = 
    match game_phase with
    | Setup -> string_to_char "pregame" in 
  | Gameplay 
let str_chars = string_to_char s [] in 
if beginnings_equal sub_chars str_chars then 



  let check_game_status str
*)

(*https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c 
  I cite ochatron for the .. solution which I had no idea about*)
let is_alpha c =
  match c with
  | 'a' .. 'z' 
  | 'A' .. 'Z' -> true 
  | _ -> false

let is_digit c = 
  match c with
  | '0' .. '9' -> true 
  | _ -> false


let process_number n = 
  n |> string_to_char [] |> List.for_all is_digit
(* if String.length n <> 1 then false
   else if n.[0] |> is_digit then true
   else false
*)

let direction_list = [
  "up";
  "down";
  "left";
  "right";
]

let ships_list = [
  "battleship";
  "aircraftcarrier";
  "cruiser";
  "destroyer";
  "submarine";
]

let process_direction d = 
  List.mem d direction_list

let process_ship_name s =
  List.mem s ships_list

let process_string s = 
  string_to_char [] s |> List.for_all is_alpha


(** the command I want to parse is
    x, y, direction, ship*)


let parse_elements str_lst = 
  match str_lst with
  | x :: y :: direction :: ship :: [] ->
    if process_number x && 
       process_number y && 
       process_string direction && 
       process_string ship &&
       process_direction direction &&
       process_ship_name ship 
    then Valid (int_of_string x - 1, int_of_string y - 1, direction, ship)
    else InvalidCommand
  | c_TARGET :: x :: y :: [] ->
    if process_number x && 
       process_number y 
    then Target (int_of_string x - 1, int_of_string y - 1)
    else InvalidCommand
  | _ -> InvalidCommand

let c_QUIT = "quit"

let parse_quit str = 
  if str = c_QUIT then Quit 
  else 
    str 
    |> String.split_on_char ','
    |> List.map String.trim
    |> parse_elements

let parse str = 
  str 
  |> String.lowercase_ascii
  |> String.trim
  |> parse_quit

(*
let parse game_state player str = 
  let cleaned = str 
                |> String.lowercase_ascii
                |> String.trim
                |> string_to_char []
                |> List.filter (fun c -> c <> ' ') in
  match cleaned with
  | [] -> InvalidCommand
  | h :: [] -> InvalidCommand
  | h1 :: h2 :: [] ->
    if is_digit h1 && is_digit h2 
    then Ingame (int_of_char h1, int_of_char h2) 
    else InvalidCommand
  | _ -> InvalidCommand
*)






(*
  type object_phrase = string list

  type command = 
  | Fire of object_phrase
  | Place of object_phrase
  | Remove of object_phrase
  | Inventory
  | Remaining of object_phrase
  | Finished
  | Quit

  exception Empty

  exception Malformed

  let parse str =
  (** Split string by spaces and takes out empty strings *)
  let words = String.split_on_char ' ' str in
  let listWords = List.filter (fun str -> str <> "") words in
  match listWords with
  | [] -> raise Empty
  | h :: t -> if h = "fire" && t <> [] then Fire t
    else if h = "place" && t <> [] then Place t
    else if h = "remove" && t <> [] then Remove t
    else if h = "inventory" && t == [] then Inventory 
    else if h = "remaining" && t <> [] then Remaining t
    else if h = "finished" && t == [] then Finished
    else if h = "quit" && t == [] then Quit
    else raise Malformed
*)