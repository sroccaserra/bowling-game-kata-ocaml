type roll = Roll of int

let rec score ?(n:int=1) (xs:roll list) : int =
  let n' = succ n in
  match xs with
  | [] -> 0
  | [Roll x; Roll y; Roll z] when is_last_frame n ->
      x + y + z
  | Roll x :: Roll y :: Roll z :: rest when is_strike x ->
      10 + y + z + score ~n:n' (Roll y :: Roll z :: rest)
  | Roll x :: Roll y :: Roll z :: rest when is_spare x y  ->
      10 + z + score ~n:n' (Roll z :: rest)
  | Roll x :: Roll y :: rest ->
      x + y + score ~n:n' rest
  | [_] -> failwith "wrong number of rolls"
and is_strike = (==) 10
and is_spare x y = x + y == 10
and is_last_frame = (==) 10
