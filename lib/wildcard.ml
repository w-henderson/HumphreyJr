let wildcard_match p t =
  let nth s n = if n < String.length s then Some (String.get s n) else None in
  let rec m i j =
    match (nth p i, nth t j) with
    | Some '*', Some _ -> m i (j + 1) || m (i + 1) j
    | Some '*', None -> m (i + 1) j
    | Some s_i, Some s_j -> s_i = s_j && m (i + 1) (j + 1)
    | Some _, None -> false
    | None, Some _ -> false
    | None, None -> true
  in
  m 0 0
