let rec score ?(n:int=1) (xs:int list) : int =
  let n' = succ n in
  match xs with
  | [] -> 0
  | [x; y; z] when is_last_frame n ->
      x + y + z
  | x :: y :: z :: rest when is_strike x ->
      10 + y + z + score ~n:n' (y :: z :: rest)
  | x :: y :: z :: rest when is_spare x y  ->
      10 + z + score ~n:n' (z :: rest)
  | x :: y :: rest ->
      x + y + score ~n:n' rest
  | [_] -> failwith "wrong number of rolls"
and is_strike = (==) 10
and is_spare x y = x + y == 10
and is_last_frame = (==) 10
