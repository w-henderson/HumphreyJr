open Frame

type websocket = { in_channel : in_channel; out_channel : out_channel }

let recv ws = (parse_frame ws.in_channel).payload

let send ws s =
  let frame = new_frame Text (Bytes.of_string s) in
  output_bytes ws.out_channel (bytes_of_frame frame);
  flush ws.out_channel
