let frame_score ?(spare_bonus = 0) n_1 n_2 =
  let sum = n_1 + n_2 in
  match sum with
  | 10 -> 10 + spare_bonus
  | n -> n

let rec score ?(sum = 0) = function
  | n_1::n_2::n_3::rest ->
      score ~sum:(sum+frame_score n_1 n_2 ~spare_bonus:n_3) (n_3::rest)
  | n_1::n_2::rest ->
      score ~sum:(sum+frame_score n_1 n_2) rest
  | [] -> sum
  | _ -> failwith "missing arguments"
