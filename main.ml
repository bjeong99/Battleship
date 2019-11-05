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
          else 
            print_endline "Finished putting down ships";
          continue_game state bs
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