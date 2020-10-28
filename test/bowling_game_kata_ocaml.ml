open Fun
open OUnit2
open Bowling_game_kata.Bowling_game

let () = run_test_tt_main ("Bowling Game" >::: [
  "A gutter game has a score of zero" >:: (fun _ ->
    let rolls = List.init 20 @@ const 0 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 0 result);

  "A game with all rolls knocking 1 pin has a score of twenty" >:: (fun _ ->
    let rolls = List.init 20 @@ const 1 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 20 result);

  "A game with a 5 and all zeroes rolls has a score of 5" >:: (fun _ ->
    let rolls = 5 :: (List.init 19 @@ const 0) in
    let result = score rolls in
    assert_equal ~printer:string_of_int 5 result);

  "A spare as first result with a bonus of 1" >:: (fun _ ->
    let rolls = 3::7::1::(List.init 17 @@ const 0) in
    let result = score rolls in
    assert_equal ~printer:string_of_int 12 result);

  "A spare as second result" >:: (fun _ ->
    let rolls = 0::0::3::7::1::(List.init 15 @@ const 0) in
    let result = score rolls in
    assert_equal ~printer:string_of_int 12 result);

  "A spare with a bonus of 2" >:: (fun _ ->
    let rolls = 0::0::3::7::2::(List.init 15 @@ const 0) in
    let result = score rolls in
    assert_equal ~printer:string_of_int 14 result);

  "A strike with a bonus of 2 scores 14" >:: (fun _ ->
    let rolls = 10::1::1::(List.init 16 @@ const 0) in
    let result = score rolls in
    assert_equal ~printer:string_of_int 14 result);
])
