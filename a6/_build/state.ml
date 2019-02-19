open Board
open Unix

type players  = 
  { mutable current: Board.player; 
    mutable first: Board.player; 
    mutable second: Board.player }

let init_players p1 p2 = 
  { current = p1; first = p1; second = p2 }

let is_turn players = 
  players.current = players.first

let next_turn players = 
  if players.current = players.first 
  then players.current <- players.second 
  else players.current <- players.first

let hit players pos =  
  if is_turn players 
  then update_boards pos players.first players.second
  else update_boards pos players.second players.first

let rec hit5 players positions =
  match positions with
  | [] -> ();
  | pos::t -> update_boards pos players.current (if is_turn players 
                                                 then players.second 
                                                 else players.first);
    hit5 players t

let notPeg players pos = 
  if is_turn players
  then begin 
    if nut_peg pos players.first players.second 
    then next_turn players;
  end
  else begin 
    if nut_peg pos players.second players.first 
    then next_turn players;
  end

let rec notPeg5 players positions =
  match positions with
  | [] -> next_turn players;
  | pos::t -> ignore (nut_peg pos players.current (if is_turn players 
                                                   then players.second 
                                                   else players.first));
    notPeg5 players t

(** [check_ships p] is true if the first field of [p] contains 0 ships.
    [check_ships p] then updates the turn and initializies the ships again *)
let check_ships players = 
  if List.length (players.current.ships) = 0
  then 
    players.current.ships <- init_ships ()

let check_place players = 
  List.length (players.current.ships) = 5

let rec place_random players = 
  (* let seed = Random.init (int_of_float (Unix.gettimeofday ())) in  *)
  let random_pos = 
    (* let seed = Random.init (int_of_float (Unix.gettimeofday ())) in  *)
    (Char.chr (Random.int(10) + 65), (Random.int(11))) in
  let next_ship1 = (fst (List.nth players.first.ships 0)) in
  let next_ship2 = (fst (List.nth players.second.ships 0)) in
  let placement = if Random.bool () then "across" else "down" in 
  if (is_turn players)
  then
    try 
      (* print_endline (String.make 1 (fst(random_pos))); *)
      (* print_endline (string_of_int (snd(random_pos))); *)
      ((add_ship next_ship1 random_pos) placement players.first);
      (check_ships players)
    with 
      exn -> place_random players 

  else  
    try
      ((add_ship next_ship2 random_pos placement players.second);
       (check_ships players))
    with 
      exn -> place_random players


let rec place_random_all n players =
  if n = 0 then 
    check_ships players
  else let random_pos = 
         (Char.chr (Random.int(11) + 65), (Random.int(11))) in
    let next_ship1 = (fst (List.nth players.first.ships 0)) in
    let next_ship2 = (fst (List.nth players.second.ships 0)) in
    let placement = if Random.bool () then "across" else "down" in 
    if (is_turn players)
    then
      try 
        ((add_ship next_ship1 random_pos) placement players.first);
        place_random_all (n-1) players
      with 
        exn -> place_random_all n players 

    else  
      try
        ((add_ship next_ship2 random_pos) placement players.second);
        place_random_all (n-1) players
      with 
        exn -> place_random_all n players

let place ship pos dir players = 
  add_ship ship pos dir players.current;
  check_ships players