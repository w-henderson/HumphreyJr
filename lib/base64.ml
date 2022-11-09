let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let get s n = try Some (Bytes.get s n) with _ -> None
let set s n v = Bytes.set_uint8 s n (Char.code v)
let sub s n l = try Some (Bytes.sub s n l) with _ -> None
let init arr = String.init 4 (fun i -> arr.(i))
let chr c = Char.chr (c mod 256)
let ( $ ) = Bytes.get

let getchr s n =
  try
    Some
      (match Char.code (String.get s n) with
      | c when c >= 65 && c <= 90 -> c - 65
      | c when c >= 97 && c <= 122 -> c - 71
      | c when c >= 48 && c <= 57 -> c + 4
      | 43 -> 62
      | 47 -> 63
      | 61 -> -1
      | _ -> assert false)
  with _ -> None

let encode_one = function
  | [ a; b; c ] ->
      let acode = Char.code a in
      let bcode = Char.code b in
      let ccode = Char.code c in
      let a = alphabet.[acode lsr 2] in
      let b = alphabet.[((acode land 0x03) lsl 4) lor (bcode lsr 4)] in
      let c = alphabet.[((bcode land 0x0f) lsl 2) lor (ccode lsr 6)] in
      let d = alphabet.[ccode land 0x3f] in
      init [| a; b; c; d |]
  | [ a; b ] ->
      let acode = Char.code a in
      let bcode = Char.code b in
      let a = alphabet.[acode lsr 2] in
      let b = alphabet.[((acode land 0x03) lsl 4) lor (bcode lsr 4)] in
      let c = alphabet.[(bcode land 0x0f) lsl 2] in
      let d = '=' in
      init [| a; b; c; d |]
  | [ a ] ->
      let acode = Char.code a in
      let a = alphabet.[acode lsr 2] in
      let b = alphabet.[(acode land 0x03) lsl 4] in
      let c = '=' in
      let d = '=' in
      init [| a; b; c; d |]
  | _ -> assert false

let encode a =
  let rec inner i acc =
    match sub a i 3 with
    | Some a -> inner (i + 3) (acc ^ encode_one [ a $ 0; a $ 1; a $ 2 ])
    | None -> (
        match Bytes.length a - i with
        | 1 -> acc ^ encode_one [ a $ i ]
        | 2 -> acc ^ encode_one [ a $ i; a $ i + 1 ]
        | _ -> acc)
  in
  inner 0 ""

let decode s =
  let result = Bytes.make (String.length s / 4 * 3) '\x00' in
  let rec inner i acc =
    match (getchr s i, i mod 4) with
    | Some -1, _ -> inner (i + 1) (acc + 1)
    | Some c, 0 ->
        (* XXXXXX-- -------- -------- *)
        set result (i / 4 * 3) (chr (c lsl 2));
        inner (i + 1) acc
    | Some c, 1 ->
        (* ------XX XXXX---- -------- *)
        set result
          (i / 4 * 3)
          (chr (Char.code (result $ i / 4 * 3) lor (c lsr 4)));
        set result ((i / 4 * 3) + 1) (chr (c lsl 4));
        inner (i + 1) acc
    | Some c, 2 ->
        (* -------- ----XXXX XX------ *)
        set result
          ((i / 4 * 3) + 1)
          (chr (Char.code (result $ (i / 4 * 3) + 1) lor (c lsr 2)));
        set result ((i / 4 * 3) + 2) (chr (c lsl 6));
        inner (i + 1) acc
    | Some c, 3 ->
        (* -------- -------- --XXXXXX *)
        set result
          ((i / 4 * 3) + 2)
          (chr (Char.code (result $ (i / 4 * 3) + 2) lor c));
        inner (i + 1) acc
    | _ -> acc
  in
  let ignored_chars = inner 0 0 in
  if ignored_chars = 0 then result
  else Bytes.sub result 0 (Bytes.length result - ignored_chars)
