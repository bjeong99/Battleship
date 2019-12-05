type command = 
  | Quit
  | InvalidCommand
  | Valid of int * int * string * string
  | Target of int * int
  | YesNo of bool
  | Remove of string
  | FinishPlacement
  | Random

type difficulty = 
  | Easy
  | Medium
  | InvalidDifficulty

val parse : string -> command

val parse_difficulty : string -> difficulty

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
