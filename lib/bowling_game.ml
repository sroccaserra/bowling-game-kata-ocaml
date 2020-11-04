type roll = Roll of int

type frame = Frame of int
let next (Frame n) = Frame (n + 1)

let rec score ?(f=Frame 1) = function
  | [] -> 0
  | [Roll x; Roll y; Roll z] when is_last_frame f ->
      x + y + z
  | r1 :: (Roll y as r2) :: (Roll z as r3) :: rest when is_strike r1 ->
      10 + y + z + score ~f:(next f) (r2 :: r3 :: rest)
  | r1 :: r2 :: (Roll z as r3) :: rest when is_spare r1 r2 ->
      10 + z + score ~f:(next f) (r3 :: rest)
  | Roll x :: Roll y :: rest ->
      x + y + score ~f:(next f) rest
  | [_] -> failwith "wrong number of rolls"
and is_strike = (=) @@ Roll 10
and is_spare (Roll x) (Roll y) = 10 == x + y
and is_last_frame = (=) @@ Frame 10
