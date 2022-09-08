open Request
open Response

let starts_with s p = String.sub s 0 (String.length p) = p

let ends_with s p =
  String.sub s (String.length s - String.length p) (String.length p) = p

let strip_prefix s p =
  let sl = String.length s in
  let pl = String.length p in
  if starts_with s p then String.sub s pl (sl - pl) else s

let strip_suffix s p =
  let sl = String.length s in
  let pl = String.length p in
  if ends_with s p then String.sub s 0 (sl - pl) else s

let rec contains s p =
  try
    match starts_with s p with
    | true -> true
    | false -> contains (String.sub s 0 (String.length s - 1)) p
  with Invalid_argument _ -> false

let handle route directory request =
  let route_without_wildcard = strip_suffix route "*" in
  let url_without_route = strip_prefix request.url route_without_wildcard in
  let path = directory ^ "/" ^ url_without_route in
  let path = if ends_with path "/" then path ^ "/index.html" else path in

  match contains path ".." with
  | true -> response 404 "Not Found"
  | false -> (
      try
        let ic = open_in_bin path in
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        let _ = really_input ic buf 0 len in
        let response = response_binary 200 buf in
        add_header response "Content-Type" "text/html"
      with Sys_error e -> response 404 e)
