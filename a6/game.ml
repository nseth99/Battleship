open Board
open Directives
open State
open Unix

(** A representation of which direction a targeted ship is heading. *)
type direction =
  |Up
  |Down
  |Left
  |Right

(**[clear ()] clears the screen on the top level*)
let clear () = ignore(Sys.command("clear"))


(**[easter_egg ()] plays a message at the beginning of each game if the 
   player chooses to play against the computer. *)
let easter_egg () =        
  try 
    ignore (Sys.command("say \"Are you ready to lose?\" "));
  with
    _ -> ()

(**[reprompt_attack players mode n] prints out the player's pegboard
   and the ships they have left. Then, [reprompt_attack] prints out 
   the player's score. If the game is in salvo mode, the number of remaining
   player attacks is printed. *)
let reprompt_attack players mode n =         
  clear();
  ANSITerminal.(print_string [Bold; blue]
                  ("Here's your pegboard "^players.current.name^":"));
  ANSITerminal.(print_string [Reset;white] "\n");
  print_board players.current.pegboard;
  ANSITerminal.(print_string [Bold; blue] "Ships left: ");
  if (is_turn players)
  then print_ships players.second.ships
  else print_ships players.first.ships;
  ANSITerminal.(print_string [Bold; blue] ("\nScore: ") );
  ANSITerminal.(print_string [Reset; blue]
                  (string_of_int players.current.score) );
  if mode = 3 then (ANSITerminal.(print_string [Bold; blue] 
                                    "\nAttacks left: ");
                    ANSITerminal.(print_string [Reset; blue] 
                                    (string_of_int n) ) )


(**[valid_phrase pos players] returns true if the [pos] on the [players] 
   pegboard is either water or a ship and false if it is a peg.*)
let valid_phrase pos players =
  let (x,y) = int_pos pos in
  match players.current.pegboard.(x).(y) with
  | Water -> true
  | Ship _ -> true
  | Peg _ -> false

let rec rand_even () = 
  let randc = Random.int(10) in
  let randn = Random.int(10) in
  if (randc + randn) mod 2 = 0 then (randc, randn) else rand_even ()

(**[print_stack stack] prints either the stack or 'empty' if 
   [stack] is empty*)
let rec print_stack stack = 
  match stack with
  |[]-> print_endline "empty"
  |h::t -> let space = (String.make 1 (Char.chr((fst (fst h))+65)))
                       ^(string_of_int ((snd (fst h))+1)) in
    print_endline space;
    print_stack t

(**[addstack x y ship stack list] adds [x] and [y] as a position_phrase
   and (direction_phrase*[ship] to the [stack]*)
let addstack x y ship stack list = 
  let stack1 = if (x+1 <= 9) && ((List.mem (x+1, y) list) = false)
    then ((x+1, y), (Down, ship))::stack else stack in
  let stack2 = if (y+1 <= 9) && ((List.mem (x, y+1) list) = false) 
    then ((x, y+1), (Right, ship))::stack1 else stack1 in
  let stack3 = if (x-1 >= 0) && ((List.mem (x-1, y) list) = false)
    then ((x-1, y), (Up, ship))::stack2 else stack2 in
  if (y-1 >= 0) && ((List.mem (x, y-1) list) = false) 
  then ((x, y-1), (Left, ship))::stack3 else stack3 

(**[char_pos (x,y)] changes an (x,y) integer position into characters.*)
let char_pos (x,y) = (Char.chr (x+65), y+1)

(**[stack_rem_ship stack ship acc] checks if the element on [stack] equals
   [ship] and calls itself recursively. If they are not equal, it will
   call itself recursively, adding that element on [stack] to the 
   [acc] *)
let rec stack_rem_ship stack ship acc =
  match stack with
  |[] -> acc
  |h::t -> if (snd (snd h) = ship) then stack_rem_ship t ship acc 
    else stack_rem_ship t ship (acc@[h])

(**[stack_rem_RL stack ship acc] checks if the element on [stack] equals
   the direction and calls itself recursively. If they are not equal, it will
   call itself recursively, adding that element on [stack] to the 
   [acc] *)
let rec stack_rem_RL stack ship acc =
  match stack with
  |[] -> acc
  |h::t -> if ((snd(snd h) = ship) && 
               ((fst(snd h) = Right)||(fst(snd h) = Left)))
    then stack_rem_RL t ship acc 
    else stack_rem_RL t ship (acc@[h])


(**[stack_rem_UD stack ship acc] checks if the element on [stack] equals
   the direction and calls itself recursively. If they are not equal, it will
   call itself recursively, adding that element on [stack] to the 
   [acc] *)
let rec stack_rem_UD stack ship acc =
  match stack with
  |[] -> acc
  |h::t -> if ((snd(snd h) = ship) && ((fst(snd h) = Up)||(fst(snd h) = Down)))
    then stack_rem_UD t ship acc 
    else stack_rem_UD t ship (acc@[h])


(**[valid_players] places all of the second player's (AI) ships randomly on 
   the board. If all of the ships have been placed, then [next_turn] is called
   to update the [players] turn.*)
let rec game_helper_ai_place players = 
  Random.init (int_of_float (Unix.gettimeofday ()));
  let ships_left = List.length players.second.ships in
  place_random_all ships_left players;
  if check_place players then 
    begin 
      clear();
      next_turn players; 
      game_helper players false 2 [] 5 [] []
    end
  else game_helper_ai_place players


(** [game_helper players p mode list n positions] Checks the win condition, and 
    prints an ASCII message as well as the final scores if the game is finished. 
    Otherwise, [game_helper] prompts each player to attack.  *)
and game_helper players p mode list n stack (positions : (char * int) list) = 
  if (check_win players.current)
  then 
    begin 
      next_turn players;
      ANSITerminal.(print_string [Bold; red]
                      ("\n 
 ██████╗  █████╗ ███╗   ███╗███████╗
██╔════╝ ██╔══██╗████╗ ████║██╔════╝
██║  ███╗███████║██╔████╔██║█████╗  
██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  
╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗
 ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝

 ██████╗ ██╗   ██╗███████╗██████╗   
██╔═══██╗██║   ██║██╔════╝██╔══██ 
██║   ██║██║   ██║█████╗  ██████╔╝  
██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗  
╚██████╔╝ ╚████╔╝ ███████╗██║  ██║  
 ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝ ") );
      ANSITerminal.(print_string [Bold; red] (
          if mode = 2 
          then 
            if check_win players.first 
            then ("\n\nThe AI has beaten you!
            \nBetter luck next time! >:)\n\nAI's score: "
                  ^(string_of_int players.second.score)^"\n"
                  ^players.first.name^"'s score: "^
                  (string_of_int players.first.score^"\n"))
            else "\n\nCongrats "^
                 players.current.name^".\nYou've beat the AI!\n\n"
                 ^players.first.name^"'s score: "^
                 (string_of_int players.first.score^"\nAI's score: "
                  ^(string_of_int players.second.score)^"\n")
          else
            ("\n\nCongrats "^players.current.name^"."^
             "\nYou've sunk all of the enemy's ships!\n\n"^
             players.current.name^"'s score: "^
             (string_of_int players.current.score)^"\n"^
             (if players.current = players.first
              then (players.second.name^"'s score: "^
                    (string_of_int players.second.score)^"\n")
              else (players.first.name^"'s score: "^
                    (string_of_int players.first.score)^"\n")))));
      ANSITerminal.(print_string [Reset] "\n"); exit 0 
    end
  else 
  if not (mode = 2) || is_turn players then
    try
      if not p && n > 0 then 
        ( reprompt_attack players mode n;
          ANSITerminal.(print_string [Reset;white] 
                          "\n\nTo attack a ship type a coordinate.");
          print_string "\nFor example, \
                        type \"G4\" to fire a cannon at G4.\n";
        )
      else
      if not p
      then ( hit5 players positions; notPeg5 players positions; 
             game_helper players false mode list
               (List.length players.current.ships) stack []);
      ANSITerminal.(print_string [Reset; white] "\n> ");

      let input_c = read_line () in
      let command = parse_directives input_c in

      match command with
      | Hit position_phrase -> 
        clear ();
        if mode = 3
        then (
          if n > 0
          then ( 
            (if not (List.mem position_phrase positions)
             && valid_phrase position_phrase players
             then game_helper players false mode list (n-1) stack
                 (position_phrase::positions)
             else game_helper players false mode list n stack
                 (positions))))
        else 
          ( hit players position_phrase;
            notPeg players position_phrase;
            game_helper players false mode list n stack positions )

      | Help -> 
        reprompt_attack players mode n;
        ANSITerminal.(print_string [Bold;red] 
                        "\n\nTo hit a ship type a coordinate.");
        ANSITerminal.(print_string [Bold;red] 
                        "\nValid coordinates include an alphanumeric code \
                         from 'A'-'J' and 1-10.");
        ANSITerminal.(print_string [Bold;red] 
                        "\nOr type 'quit' to quit.");
        ANSITerminal.(print_string [Reset;white] "\n");
        game_helper players true mode list n stack positions

      | Quit -> 
        ANSITerminal.(print_string [Bold;red] "\nThanks for playing!\n");
        ANSITerminal.(print_string [Reset] "\n"); exit 0;

      | _ -> 
        reprompt_attack players mode n;
        ANSITerminal.(print_string [Bold;red] "\n\nInvalid command. \
                                               Try attacking a square!");
        ANSITerminal.(print_string [Reset;white] "\n");
        game_helper players true mode list n stack positions 

    with
    | Invalid ->
      reprompt_attack players mode n;
      ANSITerminal.(print_string [Bold;red] "\n\nI don't understand. \
                                             Try typing a valid command.");
      ANSITerminal.(print_string [Bold;red] "\nYou can type \"help\" \
                                             for more info.");
      ANSITerminal.(print_string [Reset;white] "\n");
      game_helper players true mode list n stack positions 

    | Empty -> 
      reprompt_attack players mode n;
      ANSITerminal.(print_string [Bold;red] 
                      "\n\nIt seems like you typed nothing, \
                       try typing something.");
      ANSITerminal.(print_string [Reset;white] "\n");
      game_helper players true mode list n stack positions 

    | exn -> 
      reprompt_attack players mode n;
      ANSITerminal.(print_string [Bold;red] "\n\nWe couldn't target that \
                                             postion. Try again!");
      ANSITerminal.(print_string [Reset;white] "\n"); 
      game_helper players true mode list n stack positions;

  else 
  if List.length stack = 0 then (*no targets*)
    (* let randc = 2*Random.int(5) + 1 in *)
    (* let randn = 2*Random.int(5) + 1 in *)
    let rand_int_pos = rand_even () in
    let randc = fst(rand_int_pos) in
    let randn = snd(rand_int_pos) in
    let random_pos = 
      (* let seed = Random.init (int_of_float (Unix.gettimeofday ())) in  *)
      (Char.chr (randc + 65), (randn + 1)) in
    if List.mem (randc, randn) list 
    then game_helper players p mode list n stack positions
    else 
      let (x,y) = (randc, randn) in 
      let result = players.first.board.(x).(y) in 
      match result with
      | Water -> (* hit water nothing happened*)
        hit players random_pos;
        notPeg players random_pos;
        reprompt_attack players mode n;
        game_helper players true mode ((randc, randn)::list) n stack positions
      | Ship (m, size) -> (*first hit on ship, add all 4 to stack*)
        hit players random_pos;
        notPeg players random_pos;
        reprompt_attack players mode n;
        (* print_endline "hit"; *)
        (* print_endline (string_of_int x); *)
        (* print_endline (string_of_int y); *)
        let newstack = addstack x y m stack list in
        (* print_stack newstack; *)
        game_helper players true mode ((randc, randn)::list) n 
          newstack positions
      | Peg c -> print_endline "Try again!";
        game_helper players true mode ((randc, randn)::list) n stack positions
  else if List.mem (fst(fst (List.hd stack)), snd(fst (List.hd stack))) list 
  then game_helper players p mode list n (List.tl stack) positions
  else 
    match stack with
    | [] -> game_helper players true mode list n stack positions
    | h::t -> if List.mem (fst h) list 
      then game_helper players true mode list n t positions
      else 
        let (x,y) = (fst(fst h), snd(fst h)) in 
        let result = players.first.board.(x).(y) in
        match result with
        | Water -> (* hit water nothing happened*)
          hit players (char_pos (x,y));
          notPeg players (char_pos (x,y));
          reprompt_attack players mode n;
          (* print_stack t; *)
          game_helper players true mode ((x,y)::list) n t positions
        | Ship (m, size) -> begin (*hit on ship*)
            hit players (char_pos (x,y));
            notPeg players (char_pos (x,y));
            reprompt_attack players mode n;
            (* update_ship_size m players.first.ships; *)
            (* update_ship_size m players.second.ships; *) (
              try 
                ignore(get_ship_size m players.first.ships)
              with
              |Not_found ->
                let newstack = stack_rem_ship t m [] in
                (* print_stack newstack; *)
                game_helper players true mode ((x, y)::list) n 
                  newstack positions
              |_ -> game_helper players true mode ((x, y)::list) n 
                      stack positions);
            if (snd (snd h) = m) && ((fst(snd h) = Down)) then begin (*HIT D *)
              (*remove all the LRs *)
              (* print_endline "hit down"; *)
              let newstack = stack_rem_RL t m [] in
              if x < 9  && ((List.mem (x+1, y) list) = false) then begin
                (* print_stack (((x+1, y), (Down, m))::newstack); *)
                game_helper players true mode ((x, y)::list) n 
                  (((x+1, y), (Down, m))::newstack) positions
              end
              else 
                (* print_stack newstack; *)
                game_helper players true mode ((x, y)::list) n 
                  newstack positions
            end
            else if (snd (snd h) = m) && ((fst(snd h) = Up)) then begin (*HIT U *)
              (*remove all the LRs *)
              (* print_endline "hit up"; *)
              let newstack = stack_rem_RL t m [] in
              if x > 0 && ((List.mem (x-1, y) list) = false) then begin
                (* print_stack (((x-1, y), (Up, m))::newstack); *)
                game_helper players true mode ((x, y)::list) n 
                  (((x-1, y), (Up, m))::newstack) positions
              end
              else 
                (* print_stack newstack; *)
                game_helper players true mode ((x, y)::list) n 
                  newstack positions
            end
            else if (snd (snd h) = m) && ((fst(snd h) = Left)) then begin(*HIT L *)
              (*remove all the LRs *)
              (* print_endline "hit left"; *)
              let newstack = stack_rem_UD t m [] in
              if y > 0 && ((List.mem (x, y-1) list) = false) then begin
                (* print_stack (((x, y-1), (Left, m))::newstack); *)
                game_helper players true mode ((x, y)::list) n 
                  (((x, y-1), (Left, m))::newstack) positions
              end
              else 
                (* print_stack newstack; *)
                game_helper players true mode ((x, y)::list) n 
                  newstack positions
            end
            else if (snd (snd h) = m) && ((fst(snd h) = Right)) then begin 
              (*HIT R*)
              (*remove all the LRs *)
              print_endline "hit right";
              let newstack = stack_rem_UD t m [] in
              if y < 9 && ((List.mem (x, y+1) list) = false) then begin
                (* print_stack (((x, y+1), (Right, m))::newstack); *)
                game_helper players true mode ((x, y)::list) n 
                  (((x, y+1), (Right, m))::newstack) positions
              end
              else 
                (* print_stack newstack; *)
                game_helper players true mode ((x, y)::list) n 
                  newstack positions
            end
            else
              (* print_endline "hit new ship"; *)
              let newstack = addstack x y m t list in
              (* print_stack newstack; *)
              game_helper players true mode ((x, y)::list) n newstack positions
          end
        | Peg c -> print_endline "wtf happened this not supposed to happen";
          game_helper players true mode ((x,y)::list) n t positions



(** [game_helper_init players p mode] Prompts each player to place their ship 
    and initializes the [players] board and pegboard. *)
let rec game_helper_init players p mode =
  if p 
  then (   
    ANSITerminal.(print_string [Bold;blue] 
                    ("\nHere's your board "^players.current.name^":\n"));
    ANSITerminal.(print_string [Reset;white] "");
    print_board players.current.board;
    print_endline "\nTo place a ship, type a coordinate and a direction for \
                   the ship.";
    print_endline "For example: type \"G4 across\" to place the ship at G4 \
                   across.\n";
    print_endline "You can also say \"random\" to place your ship randomly ";
    print_endline "or \"randomize\" to place all ships randomly." 
  );

  let next_ship = (fst (List.nth players.current.ships 0)) in
  print_endline ("\nWhere would you like to place the "
                 ^(string_of_ship next_ship)^"?");
  print_string "> ";

  try
    let input_c = read_line () in
    let command = parse_directives input_c in
    match command with

    | Place (p,d) -> 
      place next_ship p d players;
      place_ship_mode players mode;

    | Randomize -> 
      Random.init (int_of_float (Unix.gettimeofday ()));
      let ships_left = List.length players.current.ships in
      place_random_all ships_left players;
      place_ship_mode players mode;

    | Random -> Random.init (int_of_float (Unix.gettimeofday ()));
      place_random players;
      place_ship_mode players mode;

    | Quit -> 
      ANSITerminal.(print_string [Bold;red] "\nSee you later!\n");
      ANSITerminal.(print_string [Reset] "\n"); exit 0

    | Help ->
      clear();
      ANSITerminal.(print_string [Bold;blue] 
                      ("\nHere's your board "^players.current.name^":\n"));
      ANSITerminal.(print_string [Reset;white] "");
      print_board players.current.board; 
      ANSITerminal.(print_string [Bold;red] 
                      "\nTo place a ship type a coordinate and a direction.\n");
      ANSITerminal.(print_string [Bold;red] 
                      "Valid coordinates include an alphanumeric code from \
                       'A'-'J' and 1-10.");
      ANSITerminal.(print_string [Bold;red] 
                      "\nValid directions include \"across\" and \"down\".");
      ANSITerminal.(print_string [Reset;white] "\n");
      game_helper_init players false mode

    | _ ->     
      clear();
      ANSITerminal.(print_string [Bold;blue] 
                      ("\nHere's your board "^players.current.name^":\n"));
      ANSITerminal.(print_string [Reset;white] "");
      print_board players.current.board; 
      ANSITerminal.(print_string [Bold;red] "\nInvalid command. \
                                             Try placing your ship!");
      ANSITerminal.(print_string [Reset;white] "\n");
      game_helper_init players false mode

  with
  | Invalid ->
    clear();
    ANSITerminal.(print_string [Bold;blue] 
                    ("\nHere's your board "^players.current.name^":\n"));
    ANSITerminal.(print_string [Reset;white] "");
    print_board players.current.board; 
    ANSITerminal.(print_string [Bold;red] "\nI don't understand. \
                                           Try typing a valid command.");
    ANSITerminal.(print_string [Bold;red] "\nYou can type \"help\" \
                                           for more info.");
    ANSITerminal.(print_string [Reset;white] "\n");
    game_helper_init players false mode

  | Empty -> 
    clear();
    ANSITerminal.(print_string [Bold;blue] 
                    ("\nHere's your board "^players.current.name^":\n"));
    ANSITerminal.(print_string [Reset;white] "");
    print_board players.current.board; 
    ANSITerminal.(print_string [Bold;red] 
                    "\nIt seems like you typed nothing, try typing something.");
    ANSITerminal.(print_string [Reset;white] "\n");
    game_helper_init players false mode

  | exn -> 
    clear();
    ANSITerminal.(print_string [Bold;blue] 
                    ("\nHere's your board "^players.current.name^":\n"));
    ANSITerminal.(print_string [Reset;white] "");
    print_board players.current.board; 
    ANSITerminal.(print_string [Bold;red] "\nWe couldn't place your ship there.\
                                           Try again!");
    ANSITerminal.(print_string [Reset;white] "\n"); 
    game_helper_init players false mode

(* [place_ship_mode players mode] Determines which add ship method to call
   depending on [mode]. *)
and place_ship_mode players mode = 
  if check_place players 
  then 
    ( clear();
      if players.current = players.second
      then ( next_turn players; game_helper players false mode [] 
               (List.length players.current.ships) [] [] )
      else ( next_turn players; 
             if mode = 2 
             then game_helper_ai_place players
             else game_helper_init players true mode ) );
  clear();
  game_helper_init players true mode


let play mode = 
  let p1 = init_player () in
  let p2 = init_player () in 
  let players = init_players p1 p2 in
  ANSITerminal.(print_string [Bold;green] "Please enter a name for \
                                           player one: ");
  let p1_name = read_line() in
  if (String.lowercase_ascii (String.trim p1_name) = "quit")
  then  
    begin 
      ANSITerminal.(print_string [Reset] "\n"); 
      exit 0; 
    end
  else
    p1.name <- p1_name;
  ANSITerminal.(print_string [Reset;green] 
                  ("\nWelcome to the game "^(p1.name)^"!\n\n"));
  if mode = 1 || mode = 3
  then
    ( ANSITerminal.(print_string [Bold;green] "Please enter a name for \
                                               player two: ");
      let p2_name = read_line() in
      if (String.lowercase_ascii (String.trim p2_name) = "quit")
      then  
        begin 
          ANSITerminal.(print_string [Reset] "\n"); 
          exit 0; 
        end
      else
        p2.name <- p2_name;
      ANSITerminal.(print_string [Reset;green] 
                      ("\nWelcome to the game "^(p2.name)^"!\n\n"));
      game_helper_init players true mode)
  else
    ( p2.name <- "AI";
      game_helper_init players true mode)

(* [ascii_welcome] prints a welcome message in ascii representation 
   Source for ascii text: 
   http://patorjk.com/software/taag/#p=display&h=2&v=0&f=ANSI%20Shadow&t=battleship *)
let ascii_welcome () =
  clear ();
  ANSITerminal.set_autoreset false;
  ANSITerminal.resize 83 36;
  ANSITerminal.(print_string [white] 
                  "
 ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗
 ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝
 ██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗  
 ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝  
 ╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗
  ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝
 ");
  ANSITerminal.(print_string [white]
                  "
 ████████╗ ██████╗ 
 ╚══██╔══╝██╔═══██╗
    ██║   ██║   ██║
    ██║   ╚██████╔╝
    ╚═╝    ╚═════╝ 
 ");
  ANSITerminal.(print_string [Blink; red]
                  "
 ██████╗  █████╗ ████████╗████████╗██╗     ███████╗███████╗██╗  ██╗██╗██████╗ 
 ██╔══██╗██╔══██╗╚══██╔══╝╚══██╔══╝██║     ██╔════╝██╔════╝██║  ██║██║██╔══██╗
 ██████╔╝███████║   ██║      ██║   ██║     █████╗  ███████╗███████║██║██████╔╝
 ██╔══██╗██╔══██║   ██║      ██║   ██║     ██╔══╝  ╚════██║██╔══██║██║██╔═══╝ 
 ██████╔╝██║  ██║   ██║      ██║   ███████╗███████╗███████║██║  ██║██║██║     
 ╚═════╝ ╚═╝  ╚═╝   ╚═╝      ╚═╝   ╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝ 
 ")


let game () =
  ascii_welcome ();
  let rec game_prompt () = 
    ANSITerminal.(print_string [Reset; Bold; white] "\n\nType ");
    ANSITerminal.(print_string [Bold;red] "'Local' ");
    ANSITerminal.(print_string [Bold;white] "to begin a two player game.");
    ANSITerminal.(print_string [Bold;white] "\nType ");
    ANSITerminal.(print_string [Bold;red] "'Computer' ");
    ANSITerminal.(print_string [Bold;white] "to play against the computer AI.");
    ANSITerminal.(print_string [Bold;white] "\nType ");
    ANSITerminal.(print_string [Bold;red] "'Salvo' ");
    ANSITerminal.(print_string [Bold;white] "to play a more advanced two \
                                             player game.\n\n");
    ANSITerminal.(print_string [Reset;white]  "> ");
    match read_line () with
    | s -> if String.lowercase_ascii (String.trim s) = "local"
      then begin
        print_string "\n";
        play 1
      end
      else 
      if String.lowercase_ascii (String.trim s) = "computer"
      then begin
        easter_egg ();
        print_string "\n";
        play 2
      end
      else 
      if String.lowercase_ascii (String.trim s) = "salvo"
      then begin
        print_string "\n";
        play 3
      end
      else 
      if String.lowercase_ascii (String.trim s) = "quit" 
      then ( ANSITerminal.(print_string [Reset] "\n"); exit 0 )
      else
        begin
          ANSITerminal.(print_string [red] 
                          "\nThat isn't a valid choice. Try again!"); 
          game_prompt ()
        end
  in game_prompt ()


let () = game ()
