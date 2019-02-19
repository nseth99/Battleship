type ship_model =
  | Carrier
  | Battleship
  | Destroyer
  | Submarine
  | Patrol

type ship = ship_model * int ref

type color =
  | Red of ship_model 
  | White
  | Sunk

type square =
  | Water
  | Ship of ship
  | Peg of color

type pos = (char * int)

type board = square array array

type player = 
  {mutable name: string; 
   board: board; 
   pegboard: board; 
   mutable ships: ship list;
   mutable score: int}

let init_ships () =
  [(Carrier, ref 5);
   (Battleship, ref 4); 
   (Destroyer, ref 3); 
   (Submarine, ref 3); 
   (Patrol, ref 2)]

let init_board () =
  Array.make_matrix 10 10 Water

let init_player () =
  { name = ""; board = init_board (); pegboard = init_board ();
    ships = init_ships (); score = 0 }

(** [int_pos (x,y)] converts the [pos] representation of a square on the board 
    as a valid entry for the [board] array with row [x] and column [y] 
    respectively. *)
let int_pos (x, y) = 
  (int_of_char x - 65, y-1)

(** [pos_update (x,y) dir] changes the indices of position [(x,y)] depending
    on direction [dir]. *)
let pos_update (x,y) dir = 
  if dir = "across"
  then (x, y+1)
  else (x+1, y) 

(** [update_ships p ship] updates the ship list of player [p] without the ship
    represented by ship_model [m]. *)
let update_ships p m = 
  p.ships <- List.filter (fun (x,y) -> x != m) p.ships  

let rec place_all_ship (m,s) (x,y) n dir p =
  if n = 0
  then ()
  else 
  if p.board.(x).(y) == Water 
  then begin 
    place_all_ship (m,s) (pos_update (x,y) dir) (n-1) dir p;
    p.board.(x).(y) <- Ship (m,s);
    update_ships p m;
  end
  else raise (Invalid_argument "Ship cannot be placed.")

(** [ship_find x lst] returns the [ship] corresponding to ship_model [x] in
    ship list [lst]. *)
let rec ship_find (x : ship_model) = function
  | [] -> raise Not_found
  | ((m,s) as h)::t -> if (x == m) then h else ship_find x t

let add_ship m (x,y) dir p =
  let (x, y) = int_pos (x, y) in 
  let (m, s) = ship_find m p.ships in 
  let n = !s in
  place_all_ship (m,s) (x,y) n dir p

let string_of_ship = function
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Destroyer -> "Destroyer"
  | Submarine -> "Submarine"
  | Patrol -> "Patrol"

(** [ship_of_string m] gives the ship representation of a string [m]. *)
let ship_of_string = function
  |  "Carrier" -> Carrier
  |  "Battleship" -> Battleship
  | "Destroyer" -> Destroyer
  | "Submarine" -> Submarine
  | "Patrol" -> Patrol
  | _ -> failwith "Invalid"

(** [string_lttr_of_ship m] gives the single letter string representation of
    a ship model [m]. *)
let string_lttr_of_ship = function
  | Carrier -> "C "
  | Battleship -> "B "
  | Destroyer -> "D "
  | Submarine -> "S "
  | Patrol -> "P " 

(** [update_ship_size m lst] updates size of ship_model [m] found in ship
    list [lst]. 
    Raises: [Not_found] if ship_model [m] is not found in ship list [lst]. *)
let update_ship_size m lst = 
  let (m,s) = ship_find m lst in 
  s := !s - 1

(** [update_score m p] updates the score of player [p] depending on ship_model 
    [m]. *)
let update_score m p =
  match m with
  | Carrier -> p.score <- p.score + 100;
  | Battleship -> p.score <- p.score + 75;
  | Destroyer -> p.score <- p.score + 50;
  | Submarine -> p.score <- p.score + 50;
  | Patrol -> p.score <- p.score + 25

(** [get_ship_size m lst] returns the int ref [size] of ship_model [m] found
    in ship list [lst].
    Raises: [Not_found] if ship_model [m] is not found in ship list [lst]. *)
let get_ship_size m lst =
  snd (ship_find m lst)

let update_boards (a,b) p1 p2 =
  let (x,y) = int_pos (a,b) in 
  let result = p2.board.(x).(y) in 
  match result with
  | Water -> 
    ANSITerminal.(print_string [Bold;blue] "- - - - - - - - - - - - - -\n");
    ANSITerminal.(print_string [Bold;blue] 
                    (p1.name^": "^(Char.escaped a)^"-"^(string_of_int b)));
    ANSITerminal.(print_string [Bold;blue] 
                    ("\n"^p2.name^": Miss. \n"));
    ANSITerminal.(print_string [Bold;blue] "- - - - - - - - - - - - - -\n");
    ANSITerminal.(print_string [Reset;white] "\n");
  | Ship (m, size) -> begin
      match m with
      | Carrier
      | Battleship
      | Destroyer
      | Submarine
      | Patrol ->
        ANSITerminal.(print_string [Bold;blue] "- - - - - - - - - - - - - -\n");
        ANSITerminal.(print_string [Bold;blue] 
                        (p1.name^": "^(Char.escaped a)^"-"^(string_of_int b)));
        ANSITerminal.(print_string [Bold;blue] 
                        ("\n"^p2.name^": Hit. "^(string_of_ship m)^". "));
        update_score m p1;
        update_ship_size m p2.ships;
        let size = !(get_ship_size m p2.ships) in 
        if (size = 0) 
        then begin 
          ANSITerminal.(print_string [Bold;blue] 
                          ("Sunk."));
          update_ships p2 m;
          p1.score <- p1.score + 1000
        end;
        ANSITerminal.(print_string [Bold;blue] 
                        "\n- - - - - - - - - - - - - -\n\n");
    end
  | Peg c -> begin
      match c with
      | Red m -> 
        ANSITerminal.(print_string [Bold;blue]
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Bold;blue] 
                        (p2.name^": You already hit part of "
                         ^(string_of_ship m)^". Try again.\n") );
        ANSITerminal.(print_string [Bold;blue] 
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Reset;white] "\n");

      | White ->
        ANSITerminal.(print_string [Bold;blue] 
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Bold;blue] 
                        (p2.name^
                         ": You've already tried this spot. Try again.\n") );
        ANSITerminal.(print_string [Bold;blue] 
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Reset;white] "\n");

      | Sunk -> 
        ANSITerminal.(print_string [Bold;blue] 
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Bold;blue] 
                        (p2.name^
                         ": You've already sunk this ship. Try again.\n") );
        ANSITerminal.(print_string [Bold;blue]
                        "- - - - - - - - - - - - - - - - - - - - - - - - - -
                        \n");
        ANSITerminal.(print_string [Reset;white] "\n");
    end

(** [make_matrix_lst lst] creates a two-dimensional list of array list [lst].*)
let rec make_matrix_lst = function
  | [] -> []
  | h::t -> (Array.to_list h)::(make_matrix_lst t)

(** [update_reds m board] changes all red pegs of sunk ship [m] to Peg [Sunk] *)
let update_reds m board =
  let lst = board |> Array.to_list |> make_matrix_lst |> List.concat in
  let rec nested_reds board board_lst (x, y) =
    match board_lst with 
    | [] -> ();
    | h::t ->
      match h with
      | Peg (Red ship) -> 
        if m = ship 
        then ( board.(x).(y) <- Peg Sunk; 
               nested_reds board t (if y+1 = 10 then (x+1, 0) else (x,y+1) ) )
        else nested_reds board t (if y+1 = 10 then (x+1, 0) else (x,y+1) )
      | _ -> nested_reds board t (if y+1 = 10 then (x+1, 0) else (x,y+1) )
  in nested_reds board lst (0, 0)


let nut_peg (x,y) p1 p2 =
  let (x,y) = int_pos (x,y) in 
  let result = p2.board.(x).(y) in 
  match result with
  | Water -> 
    p1.pegboard.(x).(y) <- Peg White;
    p2.board.(x).(y) <- Peg White;
    true
  | Ship (m, size) ->
    ( try 
        ignore(get_ship_size m p2.ships);
        ( p1.pegboard.(x).(y) <- Peg (Red m);
          p2.board.(x).(y) <- Peg (Red m) )
      with
        _ ->  
        ( p1.pegboard.(x).(y) <- Peg Sunk;
          p2.board.(x).(y) <- Peg Sunk;
          update_reds m p2.board;
          update_reds m p1.pegboard ) );
    true
  | Peg c -> false


let check_win p =
  (List.length p.ships = 0)

(** [print_horizontal ()] prints a string of dash characters followed by a 
    newline. *)
let print_horizontal () =
  print_endline "  - - - - - - - - - - - - - - - - - - - - -"

(** [print_vertical ()] prints a vertical character as a string *)
let print_vertical () =
  ANSITerminal.( print_string [Reset;white] "| ")

(** [print_numbers ()] prints the numerical axis of a battleship [board] *)
let print_numbers () = 
  print_endline "    1   2   3   4   5   6   7   8   9   10"

(** [print_ships lst] prints the ships of player's ship list [lst]. *)
let rec print_ships = function
  | [] -> ();
  | (m, s)::t -> ANSITerminal.(print_string [Reset; blue]  
                                 ((string_of_ship m)^" | ")); print_ships t

(** [nested_printer lst] prints what is found at each [square] in [lst]. *)
let rec nested_printer = function
  | [] -> print_endline ""
  | h::t -> 
    match h with
    | Water -> ANSITerminal.(print_string [Reset; blue] "~ "); 
      print_vertical ();
      nested_printer t
    | Ship (m, s) -> 
      ANSITerminal.(print_string [Bold; yellow]
                      (string_lttr_of_ship m);); 
      print_vertical (); nested_printer t
    | Peg c -> 
      begin
        match c with
        | Red _ -> 
          ANSITerminal.(print_string [Bold;red] "H "); 
          print_vertical (); nested_printer t
        | White -> 
          ANSITerminal.(print_string [Bold;white] "M "); 
          print_vertical (); nested_printer t
        | Sunk -> 
          ANSITerminal.(print_string [Bold;black] "X "); 
          print_vertical (); nested_printer t
      end

let print_board board =
  let matrix_lst = board |> Array.to_list |> make_matrix_lst in
  print_numbers ();
  let rec print_rows n board_lst =
    match board_lst with
    | [] -> print_horizontal ();
    | v::l -> 
      print_horizontal ();
      n |> char_of_int |> print_char; print_string " ";
      print_vertical (); 
      nested_printer v;
      print_rows (n+1) l 
  in 
  print_rows 65 matrix_lst