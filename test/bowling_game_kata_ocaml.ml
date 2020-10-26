open OUnit2
open Bowling_game_kata.Bowling_game

let always n _ = n

let () = run_test_tt_main ("Bowling Game" >::: [
    "A gutter game has a score of zero" >:: (fun _ ->
        let rolls = List.init 20 @@ always 0 in
        let result = score rolls in
        assert_equal 0 result ~printer:string_of_int)
])
