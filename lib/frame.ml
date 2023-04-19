type opcode = Continuation | Text | Binary | Close | Ping | Pong

type frame = {
  fin : bool;
  rsv : bool * bool * bool;
  opcode : opcode;
  mask : bool;
  length : int;
  masking_key : bytes;
  payload : bytes;
}

exception Frame_error

let opcode_of_int = function
  | 0 -> Continuation
  | 1 -> Text
  | 2 -> Binary
  | 8 -> Close
  | 9 -> Ping
  | 10 -> Pong
  | _ -> raise Frame_error

let int_of_opcode = function
  | Continuation -> 0
  | Text -> 1
  | Binary -> 2
  | Close -> 8
  | Ping -> 9
  | Pong -> 10

(*let int64_from_be b =
  let len = Bytes.length b in
  let rec inner i acc =
    if i >= 0 then
      let byte = Char.code (Bytes.get b i) in
      let bytel = Int64.of_int byte in
      inner (i - 1) (Int64.logor acc (Int64.shift_left bytel ((len - i) * 8)))
    else acc
  in
  inner (len - 1) 0L*)

let int_of_bool b = if b then 1 else 0

let int_of_rsv (b0, b1, b2) =
  (int_of_bool b0 lsl 2) lor (int_of_bool b1 lsl 1) lor int_of_bool b2

let new_frame opcode payload =
  {
    fin = true;
    rsv = (false, false, false);
    opcode;
    mask = false;
    length = Bytes.length payload;
    masking_key = Bytes.make 4 '\x00';
    payload;
  }

let parse_frame channel =
  let header = Bytes.create 2 in
  really_input channel header 0 2;
  let header_0 = Char.code (Bytes.get header 0) in
  let header_1 = Char.code (Bytes.get header 1) in
  let fin = header_0 land 0x80 <> 0 in
  let rsv =
    (header_0 land 0x40 <> 0, header_0 land 0x20 <> 0, header_0 land 0x10 <> 0)
  in
  let opcode = opcode_of_int (header_0 land 0x0F) in
  let mask = header_1 land 0x80 <> 0 in
  let length =
    match header_1 land 0x7F with
    | 126 ->
        really_input channel header 0 2;
        Int64.to_int (Bytes.get_int64_be header 0)
    | 127 ->
        let buf = Bytes.create 8 in
        really_input channel buf 0 8;
        Int64.to_int (Bytes.get_int64_be header 0)
    | i -> i
  in
  let masking_key =
    match mask with
    | true ->
        let buf = Bytes.create 4 in
        really_input channel buf 0 4;
        buf
    | false -> Bytes.make 4 '\x00'
  in
  let payload = Bytes.create length in
  really_input channel payload 0 length;
  let map f b =
    let rec inner i =
      if i < Bytes.length b then (
        Bytes.set_uint8 b i (f i (Bytes.get_uint8 b i));
        inner (i + 1))
      else ()
    in
    inner 0
  in
  map (fun i tem -> tem lxor Bytes.get_uint8 masking_key (i mod 4)) payload;
  { fin; rsv; opcode; mask; length; masking_key; payload }

let bytes_of_frame frame =
  let length_length =
    match frame.length with
    | i when i < 126 -> 0
    | i when i < 65536 -> 2
    | _ -> 8
  in
  let masking_key_length = if frame.mask then 4 else 0 in
  let length = 2 + length_length + masking_key_length + frame.length in
  let b = Bytes.create length in
  let header_0 =
    (int_of_bool frame.fin lsl 7)
    lor int_of_rsv frame.rsv lor int_of_opcode frame.opcode
  in
  Bytes.set_uint8 b 0 header_0;
  let header_1 =
    match frame.length with
    | i when i < 126 -> (int_of_bool frame.mask lsl 7) lor frame.length
    | i when i < 65536 ->
        Bytes.set_int16_be b 2 i;
        (int_of_bool frame.mask lsl 7) lor 126
    | i ->
        Bytes.set_int64_be b 2 (Int64.of_int i);
        (int_of_bool frame.mask lsl 7) lor 127
  in
  Bytes.set_uint8 b 1 header_1;
  if frame.mask then Bytes.blit frame.masking_key 0 b (2 + length_length) 4
  else ();
  Bytes.blit frame.payload 0 b
    (2 + length_length + masking_key_length)
    frame.length;
  b
