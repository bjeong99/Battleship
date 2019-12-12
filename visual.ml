let c_TIME_DELAY = 2.0

open Graphics

(* [box] represents a box on the screen. *)
type box = {
  x : int;
  y : int;
  width : int;
  height : int;
  text : string;
  color : Graphics.color;
}

(** [instantiate_box x y width height text color] is a box
    with upper left x [x] upper left y [y] width [width] height
    [height] and display text color [color]. *)
let instantiate_box x y width height text color = {
  x = x;
  y = y;
  width = width;
  height = height;
  text = text;
  color = color;
}

(** [draw_box box] draws the border of the box and text with color
    specified (border as color) (text black).  *)
let draw_box box = 
  set_color box.color;
  draw_rect box.x box.y box.width box.height;
  moveto box.x box.y;
  set_color black;
  draw_string box.text

(* [board] represents a game board. *)
type board = {
  cols : int;
  rows : int;
  width : int;
  height : int;
  color : Graphics.color;
}
(** [instantiate_board cols rows width height] makes
    a new board with those attributes and color [blue]. *)
let instantiate_board cols rows width height = {
  cols = cols;
  rows = rows;
  width = width;
  height = height;
  color = blue;
}

(** [create_pairs m n] generates pairs from (1, 1) to (m, n) inclusive. *)
let create_pairs m n = 
  let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else acc
  in create_rows m n []

let standard_pairs = 
  create_pairs 10 10

let scaled = 
  List.map 
    (fun (x, y) -> 50 * x, 50 * y) standard_pairs

(** [draw_board board] draws board to screen. *)
let draw_board board = 
  set_color board.color;
  List.map 
    (fun (x, y) -> draw_rect x y board.width board.height) scaled |> ignore

let pairs2 = 
  create_pairs 10 10 

let scaled2 = 
  List.map (fun (x, y) -> 50 * x, 50 * y) pairs2

let bottom_added_scaled2 = 
  List.map (fun (x, y) -> 50 * x + 800, 50 * y + 00) pairs2

(** [draw_generalized_board bottom_right_x bottom_right_x] 
    draws a board with lower left corner at [bottom_roght_x] and
    [bottom_right_y] based on the [board]. *)
let draw_generalized_board bottom_right_x bottom_right_y board = 
  set_color board.color;
  List.map (fun (x, y) -> draw_rect x y 50 50) bottom_added_scaled2 |> ignore

(* [filler] represents a filled in block of color. *)
type filler = {
  x : int;
  y : int;
  width : int;
  height : int;
  color : Graphics.color;
  filler : int list;
}

(** [instantiate_filler x y width height] is a new filler
    with the given arguments as attributes. *)
let instantiate_filler x y width height = {
  x = x;
  y = y;
  width = width;
  height = height;
  color = cyan;
  filler = [];
}

(** [draw_filler filler] draws the filled rectangle to screen
    based on [filler]. *)
let draw_filler filler = 
  set_color filler.color;
  fill_rect filler.x filler.y filler.width filler.height

(** [fill_box box] draws a filled rectanble based on a 
    [box] attributes. *)
let fill_box (box : box) = 
  set_color green;
  fill_poly 
    [|(box.x - 24, box.y - 24); 
      (box.x - 24, box.y + 24); 
      (box.x + 24, box.y + 24); 
      (box.x + 24, box.y - 24);|]

(** [fill_box_hits box] draws a filled rectanble based on a 
    [box] attributes as a ellipse, with radius 5 for x and y. *)
let fill_box_hits (box : box) = 
  set_color box.color;
  fill_ellipse box.x box.y 5 5

(** [draw_ships ship_matrix] draws rectangles
    on the screen representing the ships based on [ship_matrix]. *)
let draw_ships ship_matrix = 
  ship_matrix 
  |> List.map fill_box
  |> ignore

type point = int * int

(* [screen] represents the items on a screen. *)
type screen = {
  player : bool;
  hit_already : bool;
  ship_pos1 : point list;
  ship_pos2 : point list;
  ship_matrix1 : box list;
  ship_matrix2 : box list;
  player1_guesses : point list;
  player2_guesses : point list;
  player1_hits : box list;
  player2_hits : box list;
  player1_misses : box list;
  player2_misses : box list;
  box_list : box;
  image : Graphics.image option;
}


let empty_box = instantiate_box 0 0 0 0 "" white

(** [instantiate_screen player_1_random player_2_random pos1 pos2]
    is the screen with ship positions for player 1 and 2 as
    [pos1] and [pos2] and
    [player_1_random] and [player_2_random] as the matrix
    of points.  *)
let instantiate_screen player_1_random player_2_random pos1 pos2 = {
  player = true;
  hit_already = false;
  ship_pos1 = pos1; 
  ship_pos2 = pos2; 
  ship_matrix1 = player_1_random;
  ship_matrix2 = player_2_random;
  player1_guesses = [];
  player2_guesses = [];
  player1_hits = [];
  player1_misses = [];
  player2_hits = [];
  player2_misses = [];
  box_list = empty_box;
  image = None;
}

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [check_point_hit x y screen] checks if  a point [x, y] was targeted
    for that player and hit the enemy based on [screen]. *)
let check_point_hit x y screen = 
  if screen.player then
    List.mem (x, y) screen.ship_pos2
  else 
    List.mem (x, y) screen.ship_pos1

(** [check_victory_screen screen] is [true] iff  
    all the opponent ships of the player have been hit. *)
let check_victory_screen screen =   
  if screen.player then
    List.for_all 
      (fun (x, y) -> List.mem (x, y) screen.player1_guesses) screen.ship_pos2
  else
    List.for_all 
      (fun (x, y) -> List.mem (x, y) screen.player2_guesses) screen.ship_pos1

(** [point_not_targeted x y screen] is [true] iff
    [(x, y)] was not targeted previosuly based on [screen]. *)
let point_not_targeted x y screen = 
  if screen.player then
    not (List.mem (x, y) screen.player1_guesses)
  else
    not (List.mem (x, y) screen.player2_guesses)

(** [add_box_to_screen box screen] adds a [box] to 
    [screen] on [box_list] to show. *)
let add_box_to_screen box screen  = {
  screen with box_list = box
}

(** [mouse_to_coord x y h w] determines which grid square
    an action occurred on the screen, adn returns the 
    center of that square. *)
let mouse_to_square x y h w = 
  let div_x = x / h in
  let div_y = y / w in
  (div_x * h + h / 2, div_y * w + w / 2)

(** [fill_grid_square x y b] fills a grid square if [b] 
    with the a green rectangle.  *)
let fill_grid_square x y b = 
  if b then 
    let (x', y') = mouse_to_square x y 50 50 in 
    if x' >= 50 && y' >= 50 && x' <= 550 && y' <= 550 then
      (set_color green; 
       fill_poly 
         [|(x' - 24, y' - 24); 
           (x' - 24, y' + 24); 
           (x' + 24, y' + 24); 
           (x' + 24, y' - 24);|];
       set_color red)

(** [draw_box_list box_list ] draws group of boxes on the screen
    from [box_list] with the box attributes. *)
let draw_box_list box_list =
  box_list
  |> List.map draw_box 
  |> ignore

(* [text] reprsents a text blurb on the screen a [x] and [y] with [text]. *)
type text = {
  x : int;
  y : int;
  text : string;
  color : Graphics.color;
}

(** [instantiate_text x y text] is a text that is displayed with
    the corresponding attributes and printed black on screen. *)
let instantiate_text x y text = {
  x = x;
  y = y;
  text = text;
  color = black;
}

(** [draw_text text] has the effect of drawing text to the screen. *)
let draw_text text = 
  set_color text.color;
  moveto text.x text.y;
  draw_string text.text

(* [point_box] represents if an action occured on a point coordinate*)
type point_box = 
  | Point of point
  | NotPoint

(** [mouse_to_coord x y h w] determines which grid square
    an action occurred on the screen and returns
    the lower left corner of that square, not the center
    like in [mouse_to_square]. *)
let mouse_to_coord x y h w = 
  let div_x = x / h in
  let div_y = y / w in
  div_x, div_y


(** [ box_to_point x y screen] determines if an action
    has on a box and which box it was on the screen.  *)
let box_to_point x y screen = 
  if screen.player then
    let (x', y') = mouse_to_coord (x - 800) y 50 50 in 
    if x' >= 1 && y' >= 1 && x' <= 10 && y' <= 10 then
      Point (x', y')
    else NotPoint
  else 
    let (x', y') = mouse_to_coord x y 50 50 in 
    if x' >= 1 && y' >= 1 && x' <= 10 && y' <= 10 then
      Point (x', y')
    else NotPoint

(** [add_p1_guess x y screen ] adds a box to the player
    guess representing a hit or a miss in the appropriate field of
    [screen] and for the player one.  *)
let add_p1_guess x y screen = {
  screen with 
  player1_guesses = (x, y) :: screen.player1_guesses;
  player1_hits = 
    (if check_point_hit x y screen then 
       instantiate_box (50 * x + 825) (50 * y + 25) 40 40 "" red :: 
       screen.player1_hits
     else 
       screen.player1_hits);
  player1_misses = 
    (if check_point_hit x y screen then 
       screen.player1_misses
     else 
       instantiate_box (50 * x + 825) (50 * y + 25) 40 40 "" white :: 
       screen.player1_misses);
  hit_already = true;
}

(** [add_p2_guess x y screen ] adds a box to the player
    guess representing a hit or a miss in the appropriate field of
    [screen] and for the player one.  *)
let add_p2_guess x y screen = {
  screen with 
  player2_guesses = (x, y) :: screen.player2_guesses;
  player2_hits =  
    (if check_point_hit x y screen then 
       instantiate_box (50 * x + 25) (50 * y + 25) 40 40 "" magenta :: 
       screen.player2_hits
     else screen.player2_hits);
  player2_misses =
    (if check_point_hit x y screen then 
       screen.player2_misses
     else instantiate_box (50 * x + 25) (50 * y + 25) 40 40 "" white :: 
          screen.player2_misses);
  hit_already = true;
}


(** [add_to_player_guess (x, y) screen ] adds a box to the player
    guess representing a hit or a miss in the appropriate field of
    [screen] and for the right player.  *)
let add_to_player_guess (x, y) screen = 
  match screen.player with
  | true -> add_p1_guess x y screen
  | false -> add_p2_guess x y screen

(** [update_point_pressed point_pressed screen] updates the screen
    based on which coordinate box on the grid has been pressed. *)
let update_point_pressed point_pressed screen = 
  match point_pressed, screen.player with
  | Point (x, y), true 
  | Point (x, y), false -> 
    if point_not_targeted x y screen then
      add_to_player_guess (x, y) screen
    else screen
  | NotPoint, true 
  | NotPoint, false -> screen

(** [ point_button_handler x y screen] determines if an action
    has occured on the point coordinate boxes or not.  *)
let point_button_handler x y screen = 
  (box_to_point x y screen
   |> update_point_pressed) screen

(* 
(* [ship_box] represents if a action occurs on the enter box. *)
type ship_box = 
  | AircraftCarrierBox
  | BattleshipBox
  | CruiserBox
  | DestroyerBox
  | SubmarineBox
  | NotInShipBox

(** [ button_to_ship_box x y ] determines if an action
    has occured on the ship boxes or not.  *)
let button_to_ship_box x y = 
  if x >= 600 && x <= 700 && y >= 450 && y <= 550 then AircraftCarrierBox
  else if x >= 600 && x <= 700 && y >= 350 && y <= 450 then BattleshipBox
  else if x >= 600 && x <= 700 && y >= 250 && y <= 350 then CruiserBox
  else if x >= 600 && x <= 700 && y >= 150 && y <= 250 then DestroyerBox
  else if x >= 600 && x <= 700 && y >= 50 && y <= 150 then SubmarineBox
  else NotInShipBox

(** [update_ship_pressed ship_pressed screen] updates the boxes
    to print the ship on the screen. *))
let update_ship_pressed ship_pressed screen = 
  match ship_pressed with 
  | AircraftCarrierBox -> 
    let box = instantiate_box 600 450 100 100 "" yellow in 
    add_box_to_screen box screen
  | BattleshipBox ->
    let box = instantiate_box 600 350 100 100 "" yellow in 
    add_box_to_screen box screen
  | CruiserBox ->
    let box = instantiate_box 600 250 100 100 "" yellow in 
    add_box_to_screen box screen
  | DestroyerBox ->
    let box = instantiate_box 600 150 100 100 "" yellow in 
    add_box_to_screen box screen
  | SubmarineBox ->
    let box = instantiate_box 600 50 100 100 "" yellow in 
    add_box_to_screen box screen
  | NotInShipBox -> screen 

(** [ship_button_handler x y screen] handles actions for the quit
    button based on a click at [x][y].

    Requires [x][y] must lie opn the button and there must have been a click.*)
let ship_button_handler x y screen = 
  (button_to_ship_box x y 
   |> update_ship_pressed) screen
  *)

(* [enter_box] represents if a action occurs on the enter box. *)
type enter_box = 
  | EnterBox 
  | NotInEnterBox

(** [ enter_button_to_box x y ] determines if an action
    has occured on the box or not.  *)
let enter_button_to_box x y = 
  if x >= 600 && x <= 800 && y >= 50 && y <= 550 then EnterBox
  else NotInEnterBox

(** [update_enter_pressed enter_pressed screen] updates the screen
    based on whether the enter box has been pressed. *)
let update_enter_pressed enter_pressed screen = 
  match enter_pressed with 
  | EnterBox -> 
    let box = instantiate_box 600 50 200 500 "" magenta in 
    add_box_to_screen box screen
  | NotInEnterBox ->
    screen

(** [enter_button_handler x y screen] handles actions for the quit
    button based on a click at [x][y].

    Requires [x][y] must lie opn the button and there must have been a click.*)
let enter_button_handler x y screen = 
  (enter_button_to_box x y
   |> update_enter_pressed) screen

(* 
type direction_box = 
  | LeftBox
  | RightBox
  | UpBox 
  | DownBox 
  | NotInDirectionBox

let button_to_direction_box x y = 
  if x >= 700 && x <= 800 && y >= 350 && y <= 450 then LeftBox
  else if x >= 700 && x <= 800 && y >= 250 && y <= 350 then RightBox
  else if x >= 700 && x <= 800 && y >= 150 && y <= 250 then UpBox
  else if x >= 700 && x <= 800 && y >= 50 && y <= 150 then DownBox
  else NotInDirectionBox

let update_direction_pressed direction_pressed screen = 
  match direction_pressed with 
  | LeftBox -> 
    let box = instantiate_box 700 350 100 100 "" blue in 
    add_box_to_screen box screen
  | RightBox ->
    let box = instantiate_box 700 250 100 100 "" blue in 
    add_box_to_screen box screen
  | UpBox ->
    let box = instantiate_box 700 150 100 100 "" blue in 
    add_box_to_screen box screen
  | DownBox ->
    let box = instantiate_box 700 50 100 100 "" blue in 
    add_box_to_screen box screen
  | NotInDirectionBox -> screen

let direction_button_handler x y screen = 
  (button_to_direction_box x y 
   |> update_direction_pressed) screen

type quit_box = 
  | QuitBox 
  | NotInQuitBox 

let quit_button_to_box x y = 
  if x >= 700 && x <= 800 && y >= 450 && y <= 550 then QuitBox
  else NotInQuitBox

let update_quit_pressed quit_pressed screen = 
  match quit_pressed with 
  | QuitBox -> 
    let box = instantiate_box 700 450 100 100 "" red in 
    add_box_to_screen box screen
  | NotInQuitBox ->
    screen

let quit_button_handler x y screen = 
  (quit_button_to_box x y
   |> update_quit_pressed) screen *)

let enter_box = instantiate_box 600 50 200 500 " ENTER" green

let game_board = instantiate_board 10 10 50 50

let ocean_filler1 = instantiate_filler 50 50 500 500
let ocean_filler2 = instantiate_filler 850 50 500 500

let turn_text1 = instantiate_text 625 575 "Player 1 to move."
let turn_text2 = instantiate_text 625 575 "Player 2 to move."

let victory1 = instantiate_text 600 600 "Victory for Player 1!"
let victory2 = instantiate_text 600 600 "Victory for Player 2!"

(** [draw_p1 screen] draws [screen] to the screen based on a
    turn for player 1. *)
let draw_p1 screen = 
  let () = () in 
  draw_filler ocean_filler1;
  draw_board game_board;
  draw_ships screen.ship_matrix1;
  draw_filler ocean_filler2;
  draw_generalized_board 800 0 game_board;
  List.map fill_box_hits screen.player1_hits |> ignore;
  List.map fill_box_hits screen.player1_misses |> ignore;
  List.map fill_box_hits screen.player2_hits |> ignore;
  draw_box screen.box_list

(** [draw_p2 screen] draws [screen] to the screen based on a
    turn for player 2. *)
let draw_p2 screen = 
  let () = () in 
  draw_filler ocean_filler1;
  draw_board game_board;
  draw_filler ocean_filler2;
  draw_generalized_board 800 0 game_board;
  draw_ships screen.ship_matrix2;
  List.map fill_box_hits screen.player1_hits |> ignore;
  List.map fill_box_hits screen.player2_hits |> ignore;
  List.map fill_box_hits screen.player2_misses |> ignore;
  draw_box screen.box_list

(** [draw_screen screen] draws [screen] to the screen. *)
let draw_screen screen = 
  if screen.player then 
    draw_p1 screen
  else 
    draw_p2 screen

(** [change_player screen ] changes the player whose turn it is to move.*)
let change_player screen = {
  screen with 
  player = not screen.player;
  box_list = enter_box;
  hit_already = false;
}

(** [victory winner] is the winner screen based on [winner]. *)
let rec victory winner = 
  if winner then let () = draw_text victory1 in  victory winner
  else let () = draw_text victory2 in  victory winner

(** [check_coord_on_grid x y screen] if [x][y] is a coordinate
    over the grid of buttons and that no hit has been
    made in this [screen] cycle. *)
let check_coord_on_grid x y screen = 
  not screen.hit_already && ((x >= 50 && x <= 550) || (x >= 850 && x <= 1350))

(** [loop_enter_button x y screen ] is the loop helper fucntion
    that deals with clicks and events on the enter button at [x][y]
    and updates [screen]. *)
let rec loop_enter_button x y screen = 
  let new_screen = screen |> enter_button_handler x y in  
  draw_screen new_screen;
  if new_screen <> screen then 
    let () = Unix.sleepf c_TIME_DELAY in 
    loop (new_screen |> change_player)
  else loop (new_screen)

(** [loop_point_button x y screen ] is the loop helper fucntion
    that deals with clicks and events on the grid point buttons at [x][y]
    and updates [screen]. *)
and loop_point_button x y screen = 
  let new_screen = screen |> point_button_handler x y in 
  draw_screen new_screen;
  if check_victory_screen new_screen 
  then 
    let () = clear_graph () in 
    victory screen.player
  else
    new_screen |> loop

(** [loop screen] is the main game loop processing events with handlers
    based on [screen] and keyboard and mous input. *)
and loop screen = 
  if button_down () then begin
    let x, y = mouse_pos () in 
    if screen.hit_already && x >= 600 && x <= 800 then 
      loop_enter_button x y screen
    else if check_coord_on_grid x y screen then
      loop_point_button x y screen
    else
      let () = draw_screen screen in
      loop screen
  end
  else 
    let _ = draw_screen screen in 
    loop screen

let title_text = instantiate_text 650 600 "CS 3110 Battleship"

let quit_box = instantiate_box 700 450 100 100  " Quit" green

let turn_text = instantiate_text 675 575 "Turn : "
let player1_text = instantiate_text 250 575 "Player 1's Board"
let player2_text = instantiate_text 1050 575 "Player 2's Board"

let aircraftcarrer_box = 
  instantiate_box 600 450 100 100 " Aircraft Carrier" green
let battleship_box = instantiate_box 600 350 100 100 " Battleship" green
let cruiser_box = instantiate_box 600 250 100 100 " Cruiser" green 
let destroyer_box = instantiate_box 600 150 100 100 " Destroyer" green
let submarine_box = instantiate_box 600 50 100 100 " Submarine" green

let left_box = instantiate_box 700 350 100 100 " Left" green
let right_box = instantiate_box 700 250 100 100 " Right" green
let up_box = instantiate_box 700 150 100 100 " Up" green
let down_box = instantiate_box 700 50 100 100 " Down" green

(** [start_graphics screen] begins displaying the graphics
    for battleship on the screen. *)
let start_graphics screen = 
  Graphics.open_graph " 1400x650";

  draw_text title_text;
  draw_text turn_text;
  draw_text player1_text;
  draw_text player2_text;

  draw_box enter_box;

  draw_filler ocean_filler1;
  draw_filler ocean_filler2;


  draw_board game_board;
  draw_generalized_board 800 0 game_board;
  loop screen

open Battleship

(** [repeated_random_insertion player battleship] repeated adds
    ships for the [player] into [battleship]  randomly until
    all ships are added.  *)
let rec repeated_random_insertion player battleship  = 
  if remaining_ships (choose_player player) battleship = 0 then battleship
  else
    try 
      (battleship 
       |> random_ship (choose_player player)
       |> (fun ((x, y), direction, ship) -> 
           insert_ship 
             (x, y) 
             (string_to_direction direction) 
             (string_to_ship ship) 
             (choose_player player) 
             battleship)
       |> (fun result -> 
           match result with 
           | Success b -> b 
           | Failure _ -> failwith "failure"))
      |> repeated_random_insertion player
    with 
    | _ -> repeated_random_insertion player battleship 


(** [get_box_from_dict dict player] is a list of boxes reprsenting ship 
    locations from [dict] for [player]. *)
let get_box_from_dict dict player = 
  let x_spacing = if player then 25 else 825 in
  dict 
  |> get_player_dict (choose_player player)
  |> List.map (fun (_, t) -> t)
  |> List.flatten
  |> List.map (fun (x, y, _) -> (x, y))
  |> List.map (fun (x, y) -> (x + 1, y + 1))
  |> List.map (fun (x, y) -> 
      instantiate_box (50 * x + x_spacing) (50 * y + 25) 50 50 "" green)

(** [get_coords_from_dict dict player] scales up coordinates from
    [dict] for [player]. *)
let get_coords_from_dict dict player = 
  dict 
  |> get_player_dict (choose_player player)
  |> List.map (fun (_, t) -> t)
  |> List.flatten
  |> List.map (fun (x, y, _) -> (x + 1, y + 1))

let _ =
  let random_insertion = 
    Battleship.empty
    |> repeated_random_insertion true 
    |> repeated_random_insertion false in 
  let random_board_p1 = get_box_from_dict random_insertion true in
  let random_board_p2 = get_box_from_dict random_insertion false in
  let pos1 = get_coords_from_dict random_insertion true in 
  let pos2 = get_coords_from_dict random_insertion false in 
  let screen = instantiate_screen random_board_p1 random_board_p2 pos1 pos2 in 
  let _ = ANSITerminal.(print_string [red] "\nOpening Screen\n") in
  let _ = ANSITerminal.(print_string [red] "\nType anything to start\n") in
  try start_graphics screen with
  | Graphic_failure("fatal I/O error") ->
    ANSITerminal.(print_string [red] "\nClosing Screen\n")

