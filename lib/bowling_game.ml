let frame_score n_1 n_2 =
  let sum = n_1 + n_2 in
  match sum with
  | 10 -> 11
  | n -> n

let rec score ?sum rolls =
  match sum with
  | None -> score ~sum:0 rolls
  | Some n ->
      match rolls with
      | n_1::n_2::rest -> score ~sum:(n+frame_score n_1 n_2) rest
      | [] -> n
      | _ -> failwith "missing arguments"
