type response = { status : int; headers : (string * string) list; body : bytes }

let string_of_headers headers =
  let strings = List.map (fun (k, v) -> k ^ ": " ^ v) headers in
  String.concat "\r\n" strings

let bytes_of_response response =
  let first_line = "HTTP/1.1 " ^ Status.from response.status ^ "\r\n" in
  let headers = string_of_headers response.headers ^ "\r\n\r\n" in
  let meta_bytes = Bytes.of_string (first_line ^ headers) in
  Bytes.concat Bytes.empty [ meta_bytes; response.body ]

let default_headers content_length =
  [
    ("Content-Length", string_of_int content_length);
    ("Server", "HumphreyJr");
    ("Date", Time.http_timestamp_of_unix_timestamp (Unix.time ()));
  ]

let response status body =
  {
    status;
    headers = default_headers (String.length body);
    body = Bytes.of_string body;
  }

let response_binary status body =
  { status; headers = default_headers (Bytes.length body); body }

let add_header response name value =
  {
    status = response.status;
    headers = response.headers @ [ (name, value) ];
    body = response.body;
  }

let add_mime_type response path =
  let t =
    match List.rev (String.split_on_char '.' path) with
    | [] -> Mime.default
    | x :: _ -> Mime.from x
  in
  add_header response "Content-Type" t

let set_keep_alive response keep_alive =
  {
    status = response.status;
    headers =
      response.headers
      @ [ ("Connection", if keep_alive then "keep-alive" else "close") ];
    body = response.body;
  }
