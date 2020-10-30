let is_strike =
  (==) 10

let is_spare x y =
  x + y = 10

let is_last_frame =
  (==) 10

let rec score ?(n = 1) = function
  | [] -> 0
  | [x; y; z] when is_last_frame n ->
      x + y + z
  | x :: y :: z :: rest when is_strike x ->
      10 + y + z + score ~n:(succ n) (y :: z :: rest)
  | x :: y :: z :: rest when is_spare x y  ->
      10 + z + score ~n:(succ n) (z :: rest)
  | x :: y :: rest ->
      x + y + score ~n:(succ n) rest
  | [_] -> failwith "wrong number of rolls"
