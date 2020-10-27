let frame_score n_1 n_2 =
  n_1 + n_2

let rec score ?sum rolls =
  match sum with
  | None -> score ~sum:0 rolls
  | Some n ->
      match rolls with
      | n_1::n_2::rest -> score ~sum:(n+frame_score n_1 n_2) rest
      | [] -> n
      | _ -> failwith "missing arguments"
