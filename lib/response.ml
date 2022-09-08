exception InvalidResponse

type response = { status : int; headers : (string * string) list; body : bytes }

let string_of_status status =
  match status with
  | 200 -> "200 OK"
  | 404 -> "404 Not Found"
  | 500 -> "500 Internal Server Error"
  | _ -> raise InvalidResponse

let string_of_headers headers =
  let strings = List.map (fun (k, v) -> k ^ ": " ^ v) headers in
  String.concat "\r\n" strings

let bytes_of_response response =
  let first_line = "HTTP/1.1 " ^ string_of_status response.status ^ "\r\n" in
  let headers = string_of_headers response.headers ^ "\r\n\r\n" in
  let meta_bytes = Bytes.of_string (first_line ^ headers) in
  Bytes.concat Bytes.empty [ meta_bytes; response.body ]

let default_headers content_length =
  [
    ("Content-Length", string_of_int content_length);
    ("Server", "HumphreyJr");
    ("Date", Time.http_timestamp_of_unix_timestamp (Unix.time ()));
    ("Connection", "close");
    ("Content-Type", "text/plain");
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

let set_keep_alive response keep_alive =
  if keep_alive then
    {
      status = response.status;
      headers = response.headers @ [ ("Connection", "keep-alive") ];
      body = response.body;
    }
  else response
