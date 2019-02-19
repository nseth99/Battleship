open OUnit2
open Board
open Directives

let make_init_ships_test 
    (name : string) 
    (u : unit) 
    (expected_output : (ship_model * int ref) list ): test = 
  name >:: (fun _ -> assert_equal expected_output (init_ships u)) 

let make_init_board_test 
    (name : string) 
    (u : unit) 
    (expected_output : square array array ): test = 
  name >:: (fun _ -> assert_equal expected_output (init_board u)) 

let make_init_board_test 
    (name : string) 
    (u : unit) 
    (expected_output : square array array ): test = 
  name >:: (fun _ -> assert_equal expected_output (init_board u)) 

let make_init_player_test 
    (name : string) 
    (u : unit) 
    (expected_output : player ): test = 
  name >:: (fun _ -> assert_equal expected_output (init_player u)) 

let make_int_pos_test 
    (name : string) 
    (pos : char * int) 
    (expected_output : int * int): test = 
  name >:: (fun _ -> assert_equal expected_output (int_pos pos))

let make_string_of_ship_test 
    (name : string) 
    (ship : ship_model) 
    (expected_output : string): test = 
  name >:: (fun _ -> assert_equal expected_output (string_of_ship ship))

let make_string_lttr_of_ship_test 
    (name : string) 
    (ship : ship_model) 
    (expected_output : string): test = 
  name >:: (fun _ -> assert_equal expected_output (string_lttr_of_ship ship))

let make_check_win_test 
    (name : string) 
    (p : player)
    (expected_output : bool): test = 
  name >:: (fun _ -> assert_equal expected_output (check_win p))

let board_tests = [
  make_init_ships_test "init_ships" () [(Carrier, ref 5);
                                        (Battleship, ref 4); 
                                        (Destroyer, ref 3); 
                                        (Submarine, ref 3); 
                                        (Patrol, ref 2)];

  make_init_board_test "init board" () (Array.make_matrix 10 10 Water);

  make_init_player_test "init player" () { name = ""; board = init_board ();
                                           pegboard = init_board ();
                                           ships = init_ships (); score = 0};

  make_int_pos_test "int_pos" ('A', 1) (0, 0);

  make_string_of_ship_test "Carrier" Carrier "Carrier";
  make_string_of_ship_test "Battleship" Battleship "Battleship";
  make_string_of_ship_test "Destroyer" Destroyer "Destroyer";
  make_string_of_ship_test "Submarine" Submarine "Submarine";
  make_string_of_ship_test "Patrol" Patrol "Patrol";

  make_string_lttr_of_ship_test "Carrier" Carrier "C ";
  make_string_lttr_of_ship_test "Battleship" Battleship "B ";
  make_string_lttr_of_ship_test "Destroyer" Destroyer "D ";
  make_string_lttr_of_ship_test "Submarine" Submarine "S ";
  make_string_lttr_of_ship_test "Patrol" Patrol "P ";

  make_check_win_test "win" {name = ""; board = init_board ();
                             pegboard = init_board ();
                             ships = []; score = 0} true ;

  make_check_win_test "no win" {name = ""; board = init_board ();
                                pegboard = init_board ();
                                ships = init_ships (); score = 0}  false ;

]

let make_parse_test
    (name : string) 
    (str : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_directives str))

let make_parse_exception
    (name : string) 
    (str : string) 
    (expected_output : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_output (fun() -> parse_directives str))

let command_tests =
  [
    make_parse_test "test 1" "B6" (Hit('B', 6));
    make_parse_test "test 2" "A10" (Hit('A', 10));
    make_parse_test "test board" "board " Board;
    make_parse_test "test sp board" "      board" Board;
    make_parse_test "test sp pegboard sp" "      pegboard     " Pegboard;
    make_parse_test "test sp hit J10" "   J10" (Hit('J', 10));
    make_parse_test "test sp words" "    G8  " (Hit('G', 8));
    make_parse_test "test sp place J10" "   J10 across"
      (Place(('J', 10), "across"));
    make_parse_test "test sp place J10" "   J10       across" 
      (Place(('J', 10), "across"));
    make_parse_test "test case1 sp place J10" "   J10       across" 
      (Place(('J', 10), "across"));
    make_parse_test "test case2 sp place J10" "   J10       ACROSS" 
      (Place(('J', 10), "across"));
    make_parse_test "test case3 sp place J10" "   J10       ACROSS" 
      (Place(('J', 10), "across"));
    make_parse_test "test case2 place J10" " J10 ACROSS" 
      (Place(('J', 10), "across"));
    make_parse_test "test case3 place J10" " J10 ACROSS" 
      (Place(('J', 10), "across"));

    make_parse_test "test sp words" "    G8  " (Hit('G', 8));
    make_parse_test "test sp quit" "    quit  " (Quit);
    make_parse_test "test case quit" "QUIT" (Quit);
    make_parse_test "test sp case quit" "     QUIT     " (Quit);
    make_parse_test "test sp randomize" "    randomize  " Randomize;
    make_parse_test "test sp random" "    random  " Random;
    make_parse_test "test case randomize" "RANDOMIZE" Randomize;
    make_parse_test "test case random" "RANDOM" Random;
    make_parse_test "test case sp randomize" "RANDOMIZE" Randomize;
    make_parse_test "test case sp random" "RANDOM" Random;
    make_parse_test "test case help" "HELP" Help;
    make_parse_test "test case sp help" "      HELP     " Help;
    make_parse_test "test sp help" "      help       " Help;

    make_parse_exception "test empty1" "" Empty;
    make_parse_exception "test empty2" "    " Empty;


    make_parse_exception "test inv case hit extra" "B6 fun" Invalid;
    make_parse_exception "test inv sp board" "board ex" Invalid;
    make_parse_exception "test inv pegboard" "   pegboard ex" Invalid;
    make_parse_exception "test inv quit" "quit now" Invalid;
    make_parse_exception "test inv random" "random now" Invalid;
    make_parse_exception "test inv randomize" "randomize now" Invalid;
    make_parse_exception "test inv place1" "PLACE J10 SS" 
      Invalid;
    make_parse_exception "test inv place2" "place across" 
      Invalid;
    make_parse_exception "test inv place2" "place" 
      Invalid;
    make_parse_exception "test inv place3" "across" 
      Invalid;
    make_parse_exception "test inv place4" "place K10 across down" 
      Invalid;
    make_parse_exception "test inv place5" "K10 down a" 
      Invalid;

  ]

let suite =
  "test suite for A6" >::: List.flatten [
    board_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite