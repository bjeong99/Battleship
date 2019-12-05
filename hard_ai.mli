type point = int * int

type t = {
  guess_phase : bool;
  target_horizontal : bool;
  current_location : point option;
  hit_coord : point option;
  horizontal_points : point list;
  vertical_points : point list;
  remaining_coords : point list;
  locations_targeted : point list;
}

(** [initialize_hard_ai] is a starting instance of a hard AI. *)
val initialize_hard_ai : t

(** [random_guess_target ai] is a pair of the new ai, with random coordinate 
    targeted updated functionally, as well as the random coordinate itself.*)
val random_target : t -> (t * point)

(** [random_to_smart ai p] is a the new ai such that it is now
    targeting in a non random manner, starting with the point [p].
    Requires: A hit on a ship must have occurred just before this function
    is applied. *)
val random_to_smart : t -> point -> t

(** [smart_target ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Requires: can only use when a ship has been hit and not sunk yet.*)
val smart_target : t -> (t * point)

(** [smart_update_neighbors ai p] is the new ai with 
    additional coordinates to target.
    Requires: can only use when a ship has been hit and not sunk yet. *)
val smart_hit_ship : t -> point -> t

(** [smart_to_random ai b] is the new ai in the random phase.
    Requires: A ship that was being targeted was just sunk.*)
val smart_to_random : t -> bool -> t


