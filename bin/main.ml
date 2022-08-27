open Unix
open Request
open Response

let () =
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock (ADDR_INET (inet_addr_any, 8000));
  listen sock 16;
  let rec loop () =
    let stream, _ = accept sock in
    let request = parse_request stream in
    let response = response 200 request.url in
    let bytes = bytes_of_response response in
    let _ = write stream bytes 0 (Bytes.length bytes) in
    loop ()
  in
  loop ()
