(** The abstract representation of players with a current, first, and second 
    field. *)
type players  = {mutable current: Board.player; mutable first: Board.player;
                 mutable second:Board.player}

(** [init_players p1 p2] is the initial state of the players [p1] and [p2]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_players: Board.player -> Board.player -> players

(** [is_turn a] returns true if it is [a.first's] turn and false otherwise *)
val is_turn: players -> bool

(** [next_turn p] updates the current field in [p] to account for a change
    of turns*)
val next_turn: players -> unit

(** [hit p pos] checks which player's turn it is and updates each player's board 
    and pegboards accordingly. *)
val hit: players -> char * int -> unit

(** [hit5 players positions] checks which player's turn it is and 
    updates each player's board and pegboards accordingly as many times as in 
    positions list [positions]. *)
val hit5: players -> (char * int) list -> unit

(** [place_random p] chooses a random position and direction on the board and 
    places one ship in that space. *)
val place_random: players -> unit

(** [place_random p] chooses 5 random positions and directions on the board and
    places
    all ships in those spaces. *)
val place_random_all: int -> players -> unit

(** [place p pos dir ship] places a ship [ship] in the given position [pos] in
    the correct direction [dir]. The current field of [p] is updated. *)
val place: Board.ship_model -> char * int -> string -> players -> unit

(** [check_place p] is true if the first field of [p] contains 5 ships. *)
val check_place: players -> bool

(** [notPeg p] calls [nut_peg] on the current player of [p] and then updates the
    current field of [p]*)
val notPeg: players -> char * int -> unit

(** [notPeg5 players positions] calls [nut_peg] on the current player of 
    [players] and then updates the current board of [players]
     as many times as in positon list [positions]. *)
val notPeg5: players -> (char * int) list -> unit
