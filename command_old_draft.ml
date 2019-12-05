(*
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
*)

(* let parse str = 
   str 
   |> String.lowercase_ascii
   |> String.trim
   |> String.split_on_char ','
   |> List.map String.trim
   |> List.filter (fun elt -> elt <> "")
   |> parse_cleaned
*)



(*
type ship_type = 
  | Battleship
  | AircraftCarrier
  | Destroyer
  | Cruiser
  | Submarine

type command = 
  | Pregame of int * int * ship_type
  | Ingame of int * int
  | Quit
*)

(*
  (**
   Parsing of player commands.
 *)

  (** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
  - If the player command is ["fire 3 5"], then the object phrase is 
      [["3"; "5"]].
  - If the player command is ["go 3     5"], then the object phrase is
      again [["3"; "5"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
  type object_phrase = string list

  (** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
  type command = 
  | Fire of object_phrase
  | Place of object_phrase
  | Remove of object_phrase
  | Inventory
  | Remaining of object_phrase
  | Finished
  | Quit

  (** Raised when an empty command is parsed. *)
  exception Empty

  (** Raised when a malformed command is encountered. *)
  exception Malformed

  (** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
  - [parse "    fire   3   5   "] is [Fire ["3"; "5"]]
  - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb does not exist,
    or if the verb requiring an object phrase is empty or if a the very without
    an object phrase has an object phrase. *)
  val parse : string -> command
  *)
