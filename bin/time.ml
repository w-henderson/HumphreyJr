exception TimeError

let day_of_index index =
  match index with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> raise TimeError

let month_of_index index =
  match index with
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> raise TimeError

let rec pad n c s = if n - String.length s > 0 then pad (n - 1) c (c ^ s) else s
let string_of_int_padded i = pad 2 "0" (string_of_int i)

let http_timestamp_of_unix_timestamp timestamp =
  let tm = Unix.gmtime timestamp in
  day_of_index tm.tm_wday ^ ", "
  ^ string_of_int_padded tm.tm_mday
  ^ " " ^ month_of_index tm.tm_mon ^ " "
  ^ string_of_int_padded (tm.tm_year + 1900)
  ^ " "
  ^ string_of_int_padded tm.tm_hour
  ^ ":"
  ^ string_of_int_padded tm.tm_min
  ^ ":"
  ^ string_of_int_padded tm.tm_sec
  ^ " GMT"
