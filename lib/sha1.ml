let init input =
  let input_length = Bytes.length input in
  let output_length = ((Bytes.length input * 8) + 583) / 512 * 64 in
  let bytes =
    Bytes.init output_length (fun i -> try Bytes.get input i with _ -> '\x00')
  in
  Bytes.set bytes (Bytes.length input) '\x80';
  Bytes.set_int64_be bytes (output_length - 8) (Int64.of_int (input_length * 8));
  bytes

let ( <<< ) = Int32.shift_left
let ( >>> ) = Int32.shift_right_logical
let ( &&& ) = Int32.logand
let ( ||| ) = Int32.logor
let ( ~~~ ) = Int32.lognot
let ( ^^^ ) = Int32.logxor
let ( +++ ) = Int32.add
let left_rot i n = i <<< n ||| (i >>> 32 - n)

let process_chunk bytes chunk_id (h0, h1, h2, h3, h4) =
  let words =
    Bytes.init 320 (fun i ->
        if i < 64 then Bytes.get bytes (i + (chunk_id * 64)) else '\x00')
  in
  let rec extend = function
    | 80 -> ()
    | i ->
        let w3 = Bytes.get_int32_be words ((i * 4) - 12) in
        let w8 = Bytes.get_int32_be words ((i * 4) - 32) in
        let w14 = Bytes.get_int32_be words ((i * 4) - 56) in
        let w16 = Bytes.get_int32_be words ((i * 4) - 64) in
        let w = left_rot (w3 ^^^ w8 ^^^ w14 ^^^ w16) 1 in
        Bytes.set_int32_be words (i * 4) w;
        extend (i + 1)
  in
  extend 16;
  let a = ref !h0 in
  let b = ref !h1 in
  let c = ref !h2 in
  let d = ref !h3 in
  let e = ref !h4 in
  let f = ref 0l in
  let k = ref 0l in
  let k_values = [| 0x5A827999l; 0x6ED9EBA1l; 0x8F1BBCDCl; 0xCA62C1D6l |] in
  let rec mainloop = function
    | 80 -> ()
    | i ->
        (match i with
        | i when i < 20 ->
            f := !b &&& !c ||| (~~~(!b) &&& !d);
            k := k_values.(0)
        | i when i < 40 ->
            f := !b ^^^ !c ^^^ !d;
            k := k_values.(1)
        | i when i < 60 ->
            f := !b &&& !c ||| (!b &&& !d) ||| (!c &&& !d);
            k := k_values.(2)
        | _ ->
            f := !b ^^^ !c ^^^ !d;
            k := k_values.(3));
        let temp =
          left_rot !a 5 +++ !f +++ !e +++ !k +++ Bytes.get_int32_be words (i * 4)
        in
        e := !d;
        d := !c;
        c := left_rot !b 30;
        b := !a;
        a := temp;
        mainloop (i + 1)
  in
  mainloop 0;
  h0 := !h0 +++ !a;
  h1 := !h1 +++ !b;
  h2 := !h2 +++ !c;
  h3 := !h3 +++ !d;
  h4 := !h4 +++ !e

let hash input =
  let bytes = init input in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let chunks = Bytes.length bytes / 64 in
  let rec process_chunks = function
    | i when i = chunks -> ()
    | i ->
        process_chunk bytes i (h0, h1, h2, h3, h4);
        process_chunks (i + 1)
  in
  process_chunks 0;
  let result = Bytes.make 20 '\x00' in
  Bytes.set_int32_be result 0 !h0;
  Bytes.set_int32_be result 4 !h1;
  Bytes.set_int32_be result 8 !h2;
  Bytes.set_int32_be result 12 !h3;
  Bytes.set_int32_be result 16 !h4;
  result
