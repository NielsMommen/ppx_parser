let list_eq eq l1 l2 =
  let rec check l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | h1 :: tl1, h2 :: tl2 when eq h1 h2 -> check tl1 tl2
    | _ -> false
  in
  check l1 l2
