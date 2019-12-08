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

(** [parse str] is the [command] [str] corresponds to. *)
val parse : string -> command

(** [parse_difficulty str] is the [difficulty] corresponding to [str]. *)
val parse_difficulty : string -> difficulty