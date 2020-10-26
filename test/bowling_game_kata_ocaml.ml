open OUnit2
open Bowling_game_kata.Bowling_game

let always n _ = n

let () = run_test_tt_main ("Bowling Game" >::: [
  "A gutter game has a score of zero" >:: (fun _ ->
    let rolls = List.init 20 @@ always 0 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 0 result);

  "A game with all rolls knocking 1 pin has a score of twenty" >:: (fun _ ->
    let rolls = List.init 20 @@ always 1 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 20 result)
])
