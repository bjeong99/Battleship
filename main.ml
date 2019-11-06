let n_cols = 10
let n_rows = 10

let cHIT_CHAR = "X"
let cMISS_CHAR = "O"
let cUNKNOWN_CHAR  = "?"

type hit = 
  | Hit
  | Miss
  | Unknown

type damage = 
  | NoDamage
  | Damage

type ship_type = 
  | Battleship of damage
  | AircraftCarrier of damage
  | Destroyer of damage
  | Cruiser of damage
  | Submarine of damage

let print_player_guesses matrix =
  for i = 0 to (n_rows - 1) do
    let s = ref "" in 
    for j = 0 to (n_cols - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Hit -> cHIT_CHAR
        | Miss -> cMISS_CHAR
        | Unknown -> cUNKNOWN_CHAR  
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done

let print_player_ship_board matrix =
  for i = 0 to (n_rows - 1) do
    let s = ref "" in 
    for j = 0 to (n_cols - 1) do 
      let tile_rep = 
        match matrix.(i).(j) with
        | Battleship NoDamage -> "B"
        | Battleship Damage -> "b"
        | AircraftCarrier NoDamage -> "A"
        | AircraftCarrier Damage -> "a"
        | Destroyer NoDamage -> "D"
        | Destroyer Damage -> "d"
        | Cruiser NoDamage -> "C"
        | Cruiser Damage -> "c"
        | Submarine NoDamage -> "S"
        | Submarine Damage -> "s"
      in s := !s ^ " " ^ tile_rep
    done;
    print_endline !s;
  done

(*
let func1 () = 
  let new_board1 = Array.make_matrix n_rows n_cols (AircraftCarrier NoDamage)
  in new_board1.(0).(1) <- Submarine Damage;
  new_board1.(9).(9) <- Destroyer Damage;
  new_board1.(8).(9) <- Destroyer Damage;
  print_player_ship_board new_board1;
  print_endline ""

let func2 () = 
  let new_board1 = Array.make_matrix n_rows n_cols (Unknown)
  in new_board1.(0).(1) <- Hit;
  new_board1.(9).(9) <- Miss;
  new_board1.(8).(9) <- Miss;
  print_player_guesses new_board1;
  print_endline ""

let () = func1 ()
let () = func2 ()

let rec get_name () = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name: \n");
  print_string  "> ";
  match read_line () with
  | s -> if s = "" then get_name () else s

type name = string
type player = 
  | Player1 of name
  | Player2 of name

let get_both_player_names () = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name Player One. \n");
  let plyr1 = get_name () in 
  ANSITerminal.(print_string [red]
                  "\n\nPlease enter your name Player Two. \n");
  let plyr2 = get_name () in 
  (plyr1, plyr2)

let () = 
  match get_both_player_names () with
  | (s1, s2) -> print_endline s1; print_endline s2
(*| _ -> failwith "illegal"*)

*)

type pre_game_state = 
  | Quit
  | Continue of Battleship.t

let rec lay_down_ship pre_game player = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease lay down your ships on the map. \n");
  ANSITerminal.(print_string [red]
                  "\n\nSpecify the placement as a column, followed
                  by a space, then the row, followed by the space,
                  for where you want the head of the ship to be. Next
                  add a space and put in either left, right, up or down
                  for the orientation of the ship. Finally,
                  put in the type of ship, which is one
                  of battleship, aircraft carrier, destroyer,
                  cruiser or submarine. \n");
  match read_line () with
  | s -> 
    match Command.parse s with
    | Command.Quit -> 
      ANSITerminal.(print_string [red]
                      "\n\nQuitting game.\n");
      Quit
    | Command.InvalidCommand -> 
      ANSITerminal.(print_string [red]
                      "\n\nYour declaration had an error. Try again.\n");
      lay_down_ship pre_game player
    | Command.Valid (x, y, direction, ship) -> 
      match (Battleship.insert_ship (x, y) (Battleship.string_to_direction direction) (Battleship.string_to_ship ship) (Battleship.choose_player player) pre_game) with
      | Battleship.Success new_pre_game -> 
        Battleship.print_player_ship_board new_pre_game (Battleship.choose_player player);
        Continue new_pre_game
      | Battleship.Failure (p_game, Battleship.BoundsError) ->
        ANSITerminal.(print_string [red]
                        "\n\nYour coordinates were not on the grid. Try again.\n");
        lay_down_ship p_game player
      | Battleship.Failure (p_game, Battleship.OccupiedTile) ->
        ANSITerminal.(print_string [red]
                        "\n\nYour coordinates for the ship were occupied already. Try again.\n");
        lay_down_ship p_game player
      | Battleship.Failure (p_game, Battleship.OutOfShips) ->
        ANSITerminal.(print_string [red]
                        "\n\nYou have placed all the ships of that type already. Try again.\n");
        lay_down_ship p_game player

let rec lay_down_player_ships pre_game player = 
  if Battleship.remaining_ships (Battleship.choose_player player) pre_game = 0 then Continue pre_game
  else let remaining_ships = (Battleship.remaining_ships_to_place (Battleship.choose_player player) pre_game) in 
    print_endline "These are your remaining ships to place. ";
    List.map print_endline remaining_ships |> ignore;
    match lay_down_ship pre_game player with
    | Quit -> Quit
    | Continue new_pre_game -> lay_down_player_ships new_pre_game player

let lay_down_both_ships pre_game = 
  ANSITerminal.(print_string [red]
                  "\n\nPlease place your ships Player One. \n");
  let result_1 = 
    lay_down_player_ships pre_game true in 
  match result_1 with
  | Quit -> Quit
  | Continue new_pre_game ->
    ANSITerminal.(print_string [red]
                    "\n\nPlease place your ships Player Two. \n");
    let result_2 = lay_down_player_ships pre_game false in 
    match result_2 with
    | Quit -> Quit
    | Continue final_pre_game -> Continue final_pre_game

let pre_game pre_game = 
  lay_down_both_ships pre_game

(*
let rec main_game_loop=
  failwith "Unimplemented"
*)

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Battleship Game.\n");
  match pre_game (Battleship.initialize_pregame ()) with
  | Quit -> print_endline "Closing game";
  | Continue final_pre_game -> final_pre_game |> ignore

let () = main ()


(*
open Battleship
open Command
open State
let get_coord lst = 
  (List.nth lst ((List.length lst)-2), List.nth lst ((List.length lst)-1))

let rec get_ship_name lst acc= 
  match lst with 
  | a :: b :: [] -> acc 
  | h :: t -> get_ship_name t (h ^ " " ^ acc)
  | _ -> acc

let rec continue_game state bs = 
  failwith "unimplemented"

let rec place_ship player = 
  if player = "player 1" then 
    begin
      match read_line () with 
      | exception End_of_file -> ()
      | comm ->
        match (parse comm) with 
        | Finished -> 
          if player = "player 1" then
            begin
              if true (* change later *) then place_ship "player 2"
              else print_endline "Can't finish unless all ships placed!";
              place_ship player
            end
        | Place lst -> 
          let (x, y) = get_coord lst in 
          let ship = get_ship_name lst in 
          failwith "unimplemented"
        | Remove lst -> 
          let (x, y) = get_coord lst in 
          failwith "unimplemented"
        | Quit ->
          print_endline "You've quit the game. Goodbye!\n";
          exit 0
        | _ -> print_endline "Invalid command during placing ships!";
          place_ship player
    end
  else 
    begin
    end

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Battleship Game.\n");
  print_endline "Please place your ships on the grid!";
  place_ship "player_1"

let () = main ()
*)