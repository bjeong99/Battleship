(*
type name = string

val print_game_board : name -> Battleship.t -> unit
val get_name : unit -> name
val pregame_setup : unit -> Battleship.t
val play : Battleship.t -> State.t -> unit 
*)