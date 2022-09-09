open HumphreyJr.Wildcard

let test () =
  (* Identical matches *)
  assert (wildcard_match "ab" "ab");
  assert (not (wildcard_match "ab" "cd"));

  (* Zero character matches *)
  assert (wildcard_match "ab*" "ab");
  assert (wildcard_match "*ab" "ab");

  (* Basic matches *)
  assert (wildcard_match "ab*" "abcd");
  assert (wildcard_match "*cd" "abcd");
  assert (not (wildcard_match "ab" "abcd"));

  (* Multiple matches *)
  assert (wildcard_match "ab*ef" "abcdef");
  assert (wildcard_match "ab*d" "abcd");
  assert (wildcard_match "ab*ef" "abef");
  assert (wildcard_match "*cd*" "cd");
  assert (wildcard_match "*cd*" "abcd");
  assert (wildcard_match "*cd*" "cdef");
  assert (wildcard_match "*cd*" "abcdef");
  assert (not (wildcard_match "*ab" "abc"));
  assert (not (wildcard_match "a*f" "abcd"));
  assert (not (wildcard_match "a*f" "cdef"));

  (* Just matches *)
  assert (wildcard_match "*" "ab");

  (* Empty string *)
  assert (wildcard_match "" "");
  assert (wildcard_match "*" "")
