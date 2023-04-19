open HumphreyJr.Sha1

let harness input expected =
  let expected = Bytes.of_string expected in
  let output = hash input in
  assert (output = expected)

let harness_string input expected =
  let input = Bytes.of_string input in
  harness input expected

let test_short_hash () =
  harness_string "foo"
    "\x0b\xee\xc7\xb5\xea\x3f\x0f\xdb\xc9\x5d\x0d\xd4\x7f\x3c\x5b\xc2\x75\xda\x8a\x33"

let test_chunked_hash () =
  harness (Bytes.make 128 '\x69')
    "\x12\xb2\x1b\xec\x7c\x75\xa2\x0f\xa8\xc5\xac\xe0\x22\x17\x9a\x81\x5c\xd7\x95\xa1"

let test_empty_hash () =
  harness Bytes.empty
    "\xda\x39\xa3\xee\x5e\x6b\x4b\x0d\x32\x55\xbf\xef\x95\x60\x18\x90\xaf\xd8\x07\x09"

let test () =
  test_short_hash ();
  test_chunked_hash ();
  test_empty_hash ()
