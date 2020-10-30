let frame_score x y z =
  match x with
  | 10 -> 10 + y + z
  | _ ->
    let sum = x + y in
    match sum with
    | 10 -> 10 + z
    | n -> n

let rec score ?(frame_number = 1) rolls =
  let next_frame_number = frame_number + 1 in
  match rolls with
  | [] -> 0
  | [_] -> 0
  | [x; y] when frame_number = 10 -> x + y
  | [_; _] -> 0
  | 10 :: y :: z :: rest ->
      frame_score 10 y z + score ~frame_number:next_frame_number (y :: z :: rest)
  | x :: y :: z :: rest ->
      frame_score x y z + score ~frame_number:next_frame_number (z :: rest)
