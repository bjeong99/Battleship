type t = State.t

type point = int * int
type t = {
  guess_phase : bool;
  target_horizontal : bool;
  hit_coord : point;
  horizontal_points : point list;
  vertical_points : point list;
}

(*
The Hard AI works in this manner:
It has a guess stage where it goes across the board, 
  THE GUESS PROCEDURE:
  Randomly Guess 
  If it hits a ship, in which case it enters the hit stage, 
  store the horizontal and vertical tiles around the ship

The hit stage works like this:
  If the ship is sunk, go back in the guess procedure
  Otherwise choose horizontal targeting 
    - if there is no hit on either horizontal side 
      - switch to a vertical target and accumulate vertical targets
    - continue until the ship is sunk

*)