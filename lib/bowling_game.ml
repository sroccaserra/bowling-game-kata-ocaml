type roll = Roll of int

type frame = Frame of int
let next (Frame n) = Frame (n + 1)

let rec score ?(f:frame=Frame 1) (xs:roll list) : int =
  let f' = next f in
  match xs with
  | [] -> 0
  | [Roll x; Roll y; Roll z] when is_last_frame f ->
      x + y + z
  | Roll x :: Roll y :: Roll z :: rest when is_strike x ->
      10 + y + z + score ~f:f' (Roll y :: Roll z :: rest)
  | Roll x :: Roll y :: Roll z :: rest when is_spare x y  ->
      10 + z + score ~f:f' (Roll z :: rest)
  | Roll x :: Roll y :: rest ->
      x + y + score ~f:f' rest
  | [_] -> failwith "wrong number of rolls"
and is_strike = (==) 10
and is_spare x y = x + y == 10
and is_last_frame (Frame n) = n == 10
