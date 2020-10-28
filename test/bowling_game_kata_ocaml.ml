open Fun
open OUnit2
open Bowling_game_kata.Bowling_game

let rolls_with ~nb_pins ~times =
  List.init times @@ const nb_pins

let () = run_test_tt_main ("Bowling Game" >::: [
  "A gutter game has a score of zero" >:: (fun _ ->
    let rolls = rolls_with ~nb_pins:0 ~times:20 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 0 result);

  "A game with all rolls knocking 1 pin has a score of twenty" >:: (fun _ ->
    let rolls = rolls_with ~nb_pins:1 ~times:20 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 20 result);

  "A game with a 5 and all zeroes rolls has a score of 5" >:: (fun _ ->
    let rolls = 5 :: rolls_with ~nb_pins:0 ~times:19 in
    let result = score rolls in
    assert_equal ~printer:string_of_int 5 result);

  ("A spare" >::: [
    "in first frame with a bonus of 1 scores 12" >:: (fun _ ->
      let rolls = [3; 7; 1] @ rolls_with ~nb_pins:0 ~times:17 in
      let result = score rolls in
      assert_equal ~printer:string_of_int 12 result);

    "in second frame with a bonus of 1 scores 12" >:: (fun _ ->
      let rolls = [0; 0; 3; 7; 1] @ rolls_with ~nb_pins:0 ~times:15 in
      let result = score rolls in
      assert_equal ~printer:string_of_int 12 result);

    "with a bonus of 2 scores 14" >:: (fun _ ->
      let rolls = [0; 0; 3; 7; 2] @ rolls_with ~nb_pins:0 ~times:15 in
      let result = score rolls in
      assert_equal ~printer:string_of_int 14 result);
  ]);

  ("A strike" >::: [
    "with a bonus of 2 scores 14" >:: (fun _ ->
      let rolls = [10; 1; 1] @ rolls_with ~nb_pins:0 ~times:16 in
      let result = score rolls in
      assert_equal ~printer:string_of_int 14 result);

    "in last frame with a bonus of 2 scores 12" >:: (fun _ ->
      let rolls = rolls_with ~nb_pins:0 ~times:18 @ [10; 1; 1] in
      let result = score rolls in
      assert_equal ~printer:string_of_int 12 result);
  ]);
]);
