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
    let resultat = meilleure_combinaison [ Carte (As, Pique) ;  Carte (As, Coeur) ; Carte (Deux, Pique) ; Carte (Trois, Pique) ; Carte (Quatre, Pique) ] in
    assert_equal UnePaire resultat ~printer:string_of_categorie);

  "Reconnais deux paires" >:: (fun _ ->
    let resultat = meilleure_combinaison [ Carte (As, Pique) ;  Carte (As, Coeur) ; Carte (Deux, Pique) ; Carte (Deux, Coeur) ; Carte (Quatre, Pique) ] in
    assert_equal DeuxPaires resultat ~printer:string_of_categorie);

  "Reconnais deux autres paires" >:: (fun _ ->
    let resultat = meilleure_combinaison [ Carte (As, Pique) ;  Carte (As, Coeur) ; Carte (Deux, Pique) ; Carte (Deux, Carreau) ; Carte (Quatre, Pique) ] in
    assert_equal DeuxPaires resultat ~printer:string_of_categorie);

  "Reconnais yet again deux autres paires" >:: (fun _ ->
    let resultat = meilleure_combinaison [ Carte (As, Pique) ;  Carte (As, Coeur) ; Carte (Deux, Pique) ; Carte (Quatre, Carreau) ; Carte (Quatre, Pique) ] in
    assert_equal DeuxPaires resultat ~printer:string_of_categorie);
])
