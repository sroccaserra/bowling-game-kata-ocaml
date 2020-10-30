let frame_score x y z =
  match x with
  | 10 -> 10 + y + z
  | _ ->
    let sum = x + y in
    match sum with
    | 10 -> 10 + z
    | n -> n

let rec score ?(n = 1) rolls =
  match rolls with
  | [] -> 0
  | [_] -> 0
  | [x; y] when n = 10 -> x + y
  | [_; _] -> 0
  | 10 :: y :: z :: rest ->
      frame_score 10 y z + score ~n:(succ n) (y :: z :: rest)
  | x :: y :: z :: rest ->
      frame_score x y z + score ~n:(succ n) (z :: rest)
