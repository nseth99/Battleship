
type position_phrase = (char * int)

type ship_phrase = string

type direction_phrase = string

type command =
  | Hit of position_phrase
  | Board
  | Pegboard
  | Quit
  | Place of (position_phrase * direction_phrase)
  | Random
  | Randomize
  | Help


exception Empty

exception Invalid

(**[t_to_attack acc lst] takes a string list [lst] and returns a string list 
   [acc] without the empty spaces, leading spaces and trailing spaces. 
   Example: [""; ""; "hit"; ""; "B6"] becomes ["hit"; "B6"] *)
let rec t_to_attack acc = function
  | [] -> acc
  | h::t -> 
    if h = "" 
    then t_to_attack acc t
    else h::t_to_attack acc t

let parse_directives phrase =
  let input_lst = String.split_on_char ' ' (String.trim phrase) in
  let attack_lst = t_to_attack [] input_lst in
  match attack_lst with
  | [] -> raise Empty
  | s::v::t -> let s = String.uppercase_ascii s in 
    if (((String.length s) = 2) || ((String.length s) = 3))
    && (('A' <= String.get (String.uppercase_ascii s) 0) 
        && (String.get (String.uppercase_ascii s) 0 <= 'J') 
        && (1 <= (int_of_string (String.sub s 1 ((String.length s) - 1)))
            && ((int_of_string (String.sub s 1 ((String.length s) - 1))) <= 10))
        && (t = [])) 
    && ((String.lowercase_ascii v) = "across" || 
        ( (String.lowercase_ascii v) = "down"))
    then Place ((String.get s 0, 
                 (int_of_string (String.sub s 1 ((String.length s) - 1)))),
                String.lowercase_ascii v)
    else raise Invalid
  | h::t -> let s = String.uppercase_ascii h in

    (* if (String.lowercase_ascii h = "hit") *)
    if (((String.length s) = 2)||((String.length s) = 3))
    && (('A' <= String.get (String.uppercase_ascii s) 0) 
        && (String.get (String.uppercase_ascii s) 0 <= 'J') 
        && (1 <= (int_of_string (String.sub s 1 ((String.length s) - 1))) &&
            ((int_of_string (String.sub s 1 ((String.length s) - 1))) <= 10))
        && (t = [])) 
    then Hit (String.get s 0, 
              (int_of_string (String.sub s 1 ((String.length s) - 1))))
    else if ((String.lowercase_ascii h) = "board") && (t = []) 
    then Board
    else if ((String.lowercase_ascii h) = "quit") && (t=[]) 
    then Quit
    else if ((String.lowercase_ascii h) = "pegboard") && (t = []) 
    then Pegboard
    else if ((String.lowercase_ascii h) = "random") && (t = []) then Random
    else if ((String.lowercase_ascii h) = "randomize") && (t = []) 
    then Randomize
    else if ((String.lowercase_ascii h) = "help")&&(t = []) then Help
    else if ((String.lowercase_ascii h) = "") then raise Empty
    else raise Invalid