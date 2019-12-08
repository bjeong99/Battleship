(**[command] represents what a player could type in during
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

(** [c_TARGET] is a string of the "target" command. *)
val c_TARGET : string

(** [c_REMOVE] is a string of the "remove" command. *)
val c_REMOVE : string

(** [c_FINISH] is a string of the "finish" command. *)
val c_FINISH : string

(** [c_RANDOM] is a string of the "random" command. *)
val c_RANDOM : string

(** [c_QUIT] is a string of the "quit" command. *)
val c_QUIT : string

(** [c_YES] is a string of the "yes" command. *)
val c_YES : string

(** [c_NO] is a string of the "no" command. *)
val c_NO : string

(** [c_EASY] is a string of the "easy" command. *)
val c_EASY : string

(** [c_MEDIUM] is a string of the "medium" command. *)
val c_MEDIUM : string

(** [c_HARD] is a string of the "hard" command. *)
val c_HARD : string

(** [c_INSANE] is a string of the "insane" command. *)
val c_INSANE : string

(** [c_COMMA] is a char of a comma. *)
val c_COMMA : char

(** [c_SPACE] is a char of a space. *)
val c_SPACE : char

(** [c_COLON] is a char of a colon. *)
val c_COLON : char

(** [c_DIRECTION_LIST] is a list of possible command directions. *)
val c_DIRECTION_LIST : string list

(** [c_SHIPS_LIST] is a list of possible command ships. *)
val c_SHIPS_LIST : string list

(** [string_to_char acc s] is a list of chars 
    that correspond to the string [s] in the same ordering as [s]. 
    For example, [hello] is [['h'; 'e'; 'l'; 'l'; 'o']]. *)
val string_to_char : char list -> string -> char list

(** [is_alpha c] is [true] is [c] is an English character, upper or lower
    cased. [false] otherwise. *)
val is_alpha : char -> bool

(** [is_difit c] is [true] is [c] is an digit. [false] otherwise. *)
val is_digit : char -> bool

(** [process_number n] is [true] iff the [n] is comprised only of digits. r*)
val process_number : string -> bool 

(** [char_to_coord n] translates a digit or an alphabetical lenght one string
    to a digit. 
    Specifically, A or a is 1, B or b is 2, etc...
    Requires: [n] is a string with length 1. *)
val char_to_coord : string -> string

(** [process_direction d] is [true] iff [d] is in [c_DIRECTION_LIST]. *)
val process_direction : string -> bool

(** [process_ship_name s] is [true] iff [s] is in [c_SHIPS_LIST]. *)
val process_ship_name : string -> bool

(** [process_string s] is [true] iff 
    [s] is composed of only alphabetical elements.*)
val process_string : string -> bool

(** [parse_lay_down_ship x y direction ship] is
    [Valid (x - 1, y - 1, direction ship] if [x] and [y] are numbers and
    [direction] is a direction type and [ship] is a ship type else 
    [InvalidCommand]. *)
val parse_lay_down_ship : string -> string -> string -> string -> command

(** [parse_target x y target] is [Target (x - 1, y - 1)] if [x] and [y] are 
    valid numbers else [InvalidCommand]. *)
val parse_target : string -> string -> string -> command

(** [parse_remove remove ship] is [Remove ship] if [remove] is the remove string
    and [ship] is a valid ship type else [InvalidCommand]. *)
val parse_remove : string -> string -> command

(** [parse_single_word word] is 
    [FinishPlacement] if [word] is the finish string,
    [Random] if [word] is the random string,
    [YesNo true] if [word] is the yes string,
    [YesNo false] if [word] is the no string,
    [Quit] if [word] is the quit string
    else [InvalidCommand]. *)
val parse_single_word : string -> command

(** [cleaned_to_command str_lst] will parse [str_lst] into
    a valid command or an [InvalidCommand] *)
val cleaned_to_command : string list -> command

(** [clean_str str] is [str] with leading and trailing spaces removed, and
    turned lowercase and divided on spaces into a list of substrings and
    with all empty strings in that list removed. *)
val clean_str : string -> string list

(** [parse str] is the [command] [str] corresponds to. *)
val parse : string -> command

(** [parse_difficulty str] is the [difficulty] corresponding to [str]. *)
val parse_difficulty : string -> difficulty