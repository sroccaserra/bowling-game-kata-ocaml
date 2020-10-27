let frame_score ?(n_3 = 0) n_1 n_2 =
  let sum = n_1 + n_2 in
  match sum with
  | 10 -> 10 + n_3
  | n -> n

let rec score = function
  | [] -> 0
  | n_1::n_2::n_3::rest ->
      frame_score n_1 n_2 ~n_3:n_3 + score (n_3::rest)
  | n_1::n_2::rest ->
      frame_score n_1 n_2 + score rest
  | _ -> failwith "missing arguments"
