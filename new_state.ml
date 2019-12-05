open Human
open Hard_ai

type player = 
  | Player1
  | Player2

let bool_to_player b = 
  if b then Player1 else Player2

type game_player = 
  | Human of Human.t
  | AI of Hard_ai.t

type t = {
  current_player : bool;
  next_player : bool;
  player1 : game_player;
  player2 : game_player;
}

let init_state player_1 player_2 player_1_pregame player_2_pregame = 
  failwith "Unimplemented"
(*if player_1 && player_2 then
  {current_player = true;
   next_player = false;
   player1 = init_human;
   player2 = init_human; }
  else if player_1 && not player_2 then
  {current_player = true;
   next_player = false;
   player1 = init_human;
   player2 = init_hard_ai; }
  else failwith "Player1 cannot be an AI in this game. "*)

let update_player st = {
  st with 
  current_player = not st.current_player; 
  next_player = not st.next_player;
}

let get_current_player st = 
  st.current_player

let get_next_player st = 
  st.next_player

let resolve st = 
  failwith "Unimplemented"

