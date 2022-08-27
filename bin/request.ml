exception InvalidRequest

type method_ = Get | Post | Put | Delete | Head | Options

type request = {
  method_ : method_;
  url : string;
  headers : (string * string) list;
  body : bytes option;
}

let string_of_method method_ =
  match method_ with
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Head -> "HEAD"
  | Options -> "OPTIONS"

let string_of_header (name, value) = name ^ ": " ^ value

let string_of_body body =
  match body with Some body -> Bytes.to_string body | None -> ""

let string_of_request request =
  let method_ = string_of_method request.method_ in
  let url = request.url in
  let headers = List.map string_of_header request.headers in
  let body = string_of_body request.body in
  method_ ^ " " ^ url ^ "\n" ^ String.concat ", " headers ^ "\n\n" ^ body

let method_of_string string =
  match string with
  | "GET" -> Get
  | "POST" -> Post
  | "PUT" -> Put
  | "DELETE" -> Delete
  | "HEAD" -> Head
  | "OPTIONS" -> Options
  | _ -> raise InvalidRequest

let is_keep_alive request =
  List.exists
    (fun (name, value) ->
      String.lowercase_ascii name = "connection"
      && String.lowercase_ascii value = "keep-alive")
    request.headers

let parse_header header =
  match String.split_on_char ':' header with
  | name :: value -> (String.trim name, String.trim (String.concat ":" value))
  | _ -> raise InvalidRequest

let parse_request channel =
  let first_line = input_line channel in
  let method_, url =
    match String.split_on_char ' ' first_line with
    | [ method_; url; _ ] -> (method_of_string method_, url)
    | _ -> raise InvalidRequest
  in

  let rec parse_headers headers =
    let line = input_line channel in
    if line = "\r" then headers
    else parse_headers (headers @ [ parse_header line ])
  in
  let headers = parse_headers [] in

  let content_length =
    match
      List.find_opt
        (fun (name, _) -> String.lowercase_ascii name == "content-length")
        headers
    with
    | Some (_, value) -> int_of_string value
    | None -> 0
  in

  let body =
    if content_length == 0 then None
    else
      let buf = Bytes.create content_length in
      let _ = really_input channel buf 0 content_length in
      Some buf
  in

  { method_; url; headers; body }
