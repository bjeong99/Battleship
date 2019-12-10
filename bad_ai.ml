(* Bad AI

   type t = {
   pos_targeted : (int * int) list;
   pos_remaining : (int * int) list;
   }

   let create_pairs m n = 
   let rec create_rows m n acc =
    if m > 0 then 
      let init = List.init n (fun elt -> m) in
      let columns = List.init n (fun elt -> elt + 1) in 
      create_rows (m - 1) n (acc @ List.combine init columns)
    else 
      acc
   in create_rows m n []

   let choose_target pairs_list = 
   pairs_list 
   |> List.length 
   |> (fun l -> l - 1) 
   |> Random.int 
   |> List.nth pairs_list

   let update_targeted_locations ai_data target = {
   pos_targeted = target :: (ai_data.pos_targeted);
   pos_remaining = 
    List.filter (fun elt -> elt <> target) (ai_data.pos_remaining);
   }

   let initialize_ai () = {
   pos_targeted = [];
   pos_remaining = create_pairs 10 10;
   }

   let target ai_data = 
   let chosen_target = choose_target ai_data.pos_remaining in 
   let ai_data' = update_targeted_locations ai_data chosen_target in 
   (chosen_target, ai_data')

   let direction_list = ["up"; "down"; "left"; "right"]
   let directions = [1;2;3;4;]
   let ships = [1;2;3;4;5;]

   let randomly_laydown_ships pairs_list = 
   let random_direction = List.nth directions (Random.int (List.length directions)) in 
   let random_ship = List.nth ships (Random.int (List.length ships)) in 
   let random_position = choose_target pairs_list in 
   (random_position, random_direction, random_ship)











*)
