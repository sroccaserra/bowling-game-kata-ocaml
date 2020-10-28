let frame_score n_1 n_2 n_3 =
  match n_1 with
  | 10 -> 10 + n_2 + n_3
  | _ ->
    let sum = n_1 + n_2 in
    match sum with
    | 10 -> 10 + n_3
    | n -> n

let rec score ?(frame_number = 1) rolls =
  let next_frame_number = frame_number + 1 in
  match rolls with
  | [] -> 0
  | n_1 :: n_2 :: [] when frame_number = 10 -> n_1 + n_2
  | _ :: _ :: [] -> 0
  | 10 :: n_2 :: n_3 :: rest ->
      frame_score 10 n_2 n_3 + score ~frame_number:next_frame_number ([n_2; n_3] @ rest)
  | n_1 :: n_2 :: n_3 :: rest ->
      frame_score n_1 n_2 n_3 + score ~frame_number:next_frame_number (n_3 :: rest)
  | [_] ->
      failwith "missing arguments"
