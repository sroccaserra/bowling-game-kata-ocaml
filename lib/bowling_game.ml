type categorie = UnePaire | DeuxPaires
type rang = As | Deux | Trois | Quatre | Cinq | Six | Sept | Huit | Neuf | Dix | Valet | Dame | Roi
type couleur = Pique | Coeur | Trefle | Carreau
type carte = Carte of rang * couleur
type main = carte list

let rang_de (c: carte) : rang =
  match c with
  | Carte (r, _) -> r

let meilleure_combinaison (m: main) : categorie =
  let rangs = List.sort compare @@ List.map rang_de m in
  match rangs with
  | a :: b :: c :: d :: _ when a == b && c == d -> DeuxPaires
  | a :: b :: _ :: d :: [e] when a == b && d == e -> DeuxPaires
  | _ :: b :: c :: d :: [e] when b == c && d == e -> DeuxPaires
  | _ -> UnePaire

let string_of_categorie (c: categorie) : string =
  match c with
  | UnePaire -> "UnePaire"
  | DeuxPaires -> "DeuxPaires"
