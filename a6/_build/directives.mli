(** [position_phrase] is a a [(char, int)] representation of a square on the
    board. *)
type position_phrase = (char * int)

(** [ship_phrase] is a string representation of a ship model. *)
type ship_phrase = string

(** [direction_phrase] is a string representation of how to place the ship. *)
type direction_phrase = string

(** The type [command] represents a player command that is made into a verb
    followed by a position_phrase or direction_phrase *)
type command =
  | Hit of position_phrase
  | Board
  | Pegboard
  | Quit
  | Place of (position_phrase * direction_phrase)
  | Random
  | Randomize
  | Help

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when an invalid command is encountered. *)
exception Invalid

val t_to_attack : string list -> string list -> string list

(**[parse_directives phrase] takes a string input and returns it's equivalent 
   command.
   Raises Empty if string is empty
   Raises Invalid if string is not a valid command
   Example: ["hit B6"] becomes Hit (B,6) *)
val parse_directives : string -> command