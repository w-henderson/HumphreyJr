open Unix
open Request
open Response

let rec request_handler in_channel out_channel =
  let request = parse_request in_channel in
  let response = response 200 request.url in
  let is_keep_alive = is_keep_alive request in
  let response = set_keep_alive response is_keep_alive in

  let bytes = bytes_of_response response in
  output_bytes out_channel bytes;
  flush out_channel;

  match is_keep_alive with
  | true -> request_handler in_channel out_channel
  | false -> ()

let stream_handler stream =
  let in_channel = in_channel_of_descr stream in
  let out_channel = out_channel_of_descr stream in
  request_handler in_channel out_channel;

  shutdown stream SHUTDOWN_ALL;
  close_out out_channel;
  try
    close_in in_channel;
    close stream
  with _ ->
    ();
    exit 0

let () =
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock (ADDR_INET (inet_addr_any, 8000));
  listen sock 16;
  let rec loop () =
    let stream, _ = accept sock in
    match Unix.fork () with 0 -> stream_handler stream | _ -> loop ()
  in
  loop ()
