open Request
open Response
open Base64
open Sha1

let magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let add_header k v r = add_header r k v

let handshake (request : request) channel =
  let _, handshake_key =
    List.find (fun (k, _) -> k = "Sec-WebSocket-Key") request.headers
  in
  let sec_websocket_accept =
    handshake_key ^ magic_string |> Bytes.of_string |> hash |> encode
  in
  let response =
    response 101 ""
    |> add_header "Upgrade" "websocket"
    |> add_header "Connection" "Upgrade"
    |> add_header "Sec-WebSocket-Accept" sec_websocket_accept
    |> bytes_of_response
  in
  output_bytes channel response;
  flush channel
