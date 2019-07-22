type object_phrase = string list

type command = 
  | Fight of object_phrase
  | Use of object_phrase
  | Swap of object_phrase
  | Run

exception Empty

exception Malformed


let rec removespace lst acc = 
  match lst with
    [] -> acc
  |h::t -> if h = "" then removespace t acc else removespace t (h::acc)

let parsehelper lst = 
  match lst with
    [] -> []
  |h::t -> t

let parse str =
  let list = List.rev(removespace (String.split_on_char ' ' (str)) []) in
  if List.length list = 0 then raise(Empty) else if
    String.lowercase_ascii(List.hd list) = "fight" &&
    List.length (parsehelper list) != 0 then 
    Fight(parsehelper list) else if
    String.lowercase_ascii(List.hd list) = "use" && 
    List.length (parsehelper list) != 0 then 
    Use (parsehelper list) else if 
    String.lowercase_ascii(List.hd list) = "swap" &&
    List.length (parsehelper list) != 0 then
    Swap (parsehelper list) else if 
    String.lowercase_ascii(List.hd list) = "run"  &&
    List.length (parsehelper list) == 0 then
    Run 
  else raise(Malformed)

