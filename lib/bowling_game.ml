let rec score ?(n = 1) = function
  | [] -> 0
  | [_] -> failwith "wrong number of rolls"
  | [x; y; z] when n = 10 -> x + y + z
  | 10 :: y :: z :: rest ->
    10 + y + z + score ~n:(succ n) (y :: z :: rest)
  | x :: y :: z :: rest when x + y = 10 ->
    10 + z + score ~n:(succ n) (z :: rest)
  | x :: y :: rest ->
     x + y + score ~n:(succ n) rest
