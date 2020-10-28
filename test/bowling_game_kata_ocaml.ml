open OUnit2
open Bowling_game_kata.Bowling_game

(*
 * High card (aucun du reste)
 * Une paire
 * Deux paires
 * Brelan
 *)
let () = run_test_tt_main ("Poker hand" >::: [
  "Reconnais une paire" >:: (fun _ ->
    let resultat = meilleure_combinaison [ (As, Pique) ; (As, Coeur) ] in
    assert_equal UnePaire resultat);
])
