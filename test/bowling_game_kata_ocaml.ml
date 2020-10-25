open OUnit2
open Bowling_game_kata.Bowling_game

let test_gutter_game _ =
    let rolls = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] in
    assert_equal ~printer: string_of_int 0 (score rolls)

let suite =
    "Bowling Game" >::: [
        "A gutter game" >:: test_gutter_game
    ]

let () =
    run_test_tt_main suite
