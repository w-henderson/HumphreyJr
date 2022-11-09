open HumphreyJr.Frame
open Unix

let in_channel_of_bytes b =
  let in_file_descr, out_file_descr = pipe () in
  let in_channel = in_channel_of_descr in_file_descr in
  let out_channel = out_channel_of_descr out_file_descr in
  output_bytes out_channel b;
  flush out_channel;
  in_channel

let test () =
  let payload = Bytes.of_string "hello" in
  let frame = new_frame Text payload in
  let serialized_frame = bytes_of_frame frame in
  let channel = in_channel_of_bytes serialized_frame in
  let deserialized_frame = parse_frame channel in
  assert (frame = deserialized_frame)
