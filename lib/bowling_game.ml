let rec score ?(n = 1) rolls =
  match rolls with
  | [] -> 0
  | [_] -> 0
  | [x; y] when n = 10 -> x + y
  | [_; _] -> 0
  | 10 :: y :: z :: rest ->
    10 + y + z + score ~n:(succ n) (y :: z :: rest)
  | x :: y :: z :: rest when x + y = 10 ->
    10 + z + score ~n:(succ n) (z :: rest)
  | x :: y :: z :: rest ->
     x + y + score ~n:(succ n) (z :: rest)
