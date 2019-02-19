
(** A representation of the different models of a ship. *)
type ship_model =
  | Carrier
  | Battleship
  | Destroyer
  | Submarine
  | Patrol

(** A representation of a ship with a mutable size. *)
type ship = ship_model * int ref

(** A representation of all possible peg colors. *)
type color =
  | Red of ship_model
  | White
  | Sunk

(** A representation of a position on the board. *)
type square =
  | Water
  | Ship of ship
  | Peg of color


(** A representation of a position reference on the board. *)
type pos = (char * int)

(** The representation of a board *)
type board = square array array

(** The abstract representation of a player object with a name, board, 
    pegboard, ship list, and score. *)
type player = 
  { mutable name: string; 
    board: board; 
    pegboard: board; 
    mutable ships: ship list;
    mutable score: int }

(** [init_ships] gives a [ship] list with the intial sizes of each ship. *)
val init_ships: unit -> (ship_model * int ref) list

(** [init_board ()] gives the initial [board] 2-D array. Each square is intially
    empty, represented as [Water]. *)
val init_board: unit -> square array array

(** [init_player ()] initializes a new player whose [name] is the empty string,
    with an initial board [board], pegboard [pegboard] and 
    initial [ship] list. *)
val init_player: unit -> player

(** [place_all_ship (m,s) (x,y) n dir p] places the ship [(m,s)] on 
    player [p]'s [board] at start position [(x,y)] going string direction [dir]
    until int size [n] of the ship is reached.
    Rasies: [Invalid_argument s] if ship [(m,s)] cannot be placed from start
    position [(x,y)] using direction [dir]. *)
val place_all_ship: 
  ship_model * int ref -> int * int -> int -> string -> player -> unit

(** [add_ship m (x,y) dir p] adds the corresponding ship with ship_model [m] *)
val add_ship: ship_model -> char * int -> string -> player -> unit


(** [string_of_ship m] gives the string representation of a ship_model [m]. *)
val string_of_ship: ship_model -> string

(** [int_pos (x,y)] converts the [pos] representation of a square on the board 
    as a valid entry for the [board] array with row [x] and column [y] 
    respectively. *)
val int_pos: char * int -> int * int

(** [string_lttr_of_ship m] gives the single letter string representation of
    a ship model [m]. *)
val string_lttr_of_ship: ship_model -> string

(** [update_boards pos p1 p2] changes the [board] of [p2] and [pegboard] 
    of [p1] depending on what is found at pos in the board of [p2]. If
    a ship is found, a red peg is placed on both boards, a white peg will be
    placed on [p1]'s [pegboard] otherwise. If a peg is already found at [pos],
    then the corresponding boards of [p1] and [p2] remain unchanged. *)
val update_boards: char * int -> player -> player -> unit

(** [nut_peg (x,y) p1 p2] checks if square [(x,y)] has a valid location for
    peg placement on boards and pegboards *)
val nut_peg : char * int -> player -> player -> bool

(** [check_win p] returns true if [p] has no more ships in their [ship] list. *)
val check_win: player -> bool

(** [print_ships lst] prints the model ships of player's ship list [lst]. *)
val print_ships: (ship_model * 'a) list -> unit

(** [print_board board] prints an ascii-text representation of board [board]. *)
val print_board: square array array -> unit

val get_ship_size: ship_model -> (ship_model * 'a) list -> 'a