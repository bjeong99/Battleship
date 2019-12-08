type point = int * int

type t = {
  guess_phase : bool;

  insane : bool;

  remaining_coords : point list;
  locations_targeted : point list;

  hit_left_bound : bool;
  hit_right_bound : bool;
  hit_top_bound : bool;
  hit_bottom_bound : bool;

  left_list : point list;
  right_list : point list;
  top_list : point list;
  bottom_list : point list;
}
(** [initialize_hard_ai] is a starting instance of a hard AI. *)
val initialize_hard_ai : t

(** [all_bounds_or_all_lists ai] is true iff all the lists
    for smart targeting are empty or all bounds are all true,
    false otherwise. *)
val all_bounds_or_all_lists : t -> bool

(*
(** [get_target_horizontal ai] is [true] iff the horizontal targetinbg phase
    is being used.*)
val get_target_horizontal : t -> bool
*)

(** [get_insane_phase ai] is the targeting phase the [ai] is in, either
    the insane or guess type. *)
val get_insane_phase : t -> bool

(** [get_guess phase ai] is the targeting phase the [ai] is in, either
    the random or smart phase. *)
val get_guess_phase : t -> bool

(** [random_guess_target ai] is a pair of the new ai, with random coordinate 
    targeted updated functionally, as well as the random coordinate itself.*)
val random_target : t -> (point * t)

(** [random_to_smart ai] is a the new ai such that it is now
    targeting in a non random manner, starting with the point [p].
    Requires: A hit on a ship must have occurred just before this function
    is applied. *)
val random_to_smart : t -> t

(** [smart_target ai] is a pair of the new ai, with a coordinate 
    targeted updated functionally, as well as the targeted coordinate itself.
    Requires: can only use when a ship has been hit and not sunk yet.*)
val smart_target : t -> (point * t)

(** [update_smart_after_miss ai] is the new ai with the targeting direction
    potentially updated. 
    Requires: Use after AI in smart mode misses. *)
val update_smart_after_miss : t -> t

(*
(** [smart_update_neighbors ai p] is the new ai with 
    additional coordinates to target.
    Requires: can only use when a ship has been hit and not sunk yet. *)
val smart_update_neighbors : t -> point -> t
*)

(** [update_smart_ai_after_hit ai p] is the new ai with 
    additional coordinates to target after the point [p] with a ship was hit.
    Requires: Use only immediately after a ship has been hit by the AI. *)
val update_smart_ai_after_hit : t -> point -> t

(** [smart_to_random ai] is the new ai in the random phase.
    Requires: A ship that was being targeted was just sunk.*)
val smart_to_random : t -> t

(** [insane_target ai] is the point targeteted by [ai] with the functionally
    updated ai. The point is chosen probabilistically in an algorithm
    described elsewhere.*)
val insane_target : t -> point * t

(** [insane_to_smart ai] is a the new ai such that it is now
    targeting in a non smart manner, starting with the point [p].
    Requires: A hit on a ship must have occurred just before this function
    is applied. *)
val insane_to_smart : t -> t

(** [smart_to_insane ai] is the new ai in the INSANE phase.
    Requires: A ship that was being targeted was just sunk.*)
val smart_to_insane : t -> t



