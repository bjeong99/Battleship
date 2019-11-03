
type object_phrase = string list

type command = 
  | Fire of object_phrase
  | Place of object_phrase
  | Remove of object_phrase
  | Inventory
  | Remaining of object_phrase
  | Quit

exception Empty

exception Malformed

let parse str =
  (** Split string by spaces and takes out empty strings *)
  let words = String.split_on_char ' ' str in
  let listWords = List.filter (fun str -> str <> "") words in
  match listWords with
  | [] -> raise Empty
  | h :: t -> if h = "fire" && t <> [] then Fire t
    else if h = "place" && t <> [] then Place t
    else if h = "remove" && t <> [] then Remove t
    else if h = "inventory" && t == [] then Inventory 
    else if h = "remaining" && t <> [] then Remaining t
    else if h = "quit" && t == [] then Quit
    else raise Malformed
