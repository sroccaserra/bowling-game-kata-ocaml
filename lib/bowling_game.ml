let frame_score n_1 n_2 =
  let sum = n_1 + n_2 in
  match sum with
  | 10 -> 11
  | n -> n

let rec score ?(sum = 0) = function
  | n_1::n_2::rest -> score ~sum:(sum+frame_score n_1 n_2) rest
  | [] -> sum
  | _ -> failwith "missing arguments"
