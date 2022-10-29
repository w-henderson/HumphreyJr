open HumphreyJr.Base64

let encode_harness input expected =
  let actual = encode (Array.of_seq (String.to_seq input)) in
  assert (actual = expected)

let decode_harness input expected =
  let actual = decode input in
  assert (actual = Array.of_seq (String.to_seq expected))

let test_encode () =
  encode_harness "foo" "Zm9v";
  encode_harness "yeet" "eWVldA==";
  encode_harness "hello" "aGVsbG8="

let test_decode () =
  decode_harness "Zm9v" "foo";
  decode_harness "eWVldA==" "yeet";
  decode_harness "aGVsbG8=" "hello"

let test () =
  test_encode ();
  test_decode ()
