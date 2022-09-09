open HumphreyJr.Time

let t = http_timestamp_of_unix_timestamp

let test () =
  assert (t 1628437415. = "Sun, 08 Aug 2021 15:43:35 GMT");
  assert (t 1094474096. = "Mon, 06 Sep 2004 12:34:56 GMT");
  assert (t 1584716400. = "Fri, 20 Mar 2020 15:00:00 GMT");
  assert (t 1582979696. = "Sat, 29 Feb 2020 12:34:56 GMT");
  assert (t (-84337067.) = "Sun, 30 Apr 1967 21:02:13 GMT");
  assert (t (-28504100829.) = "Fri, 28 Sep 1066 10:12:51 GMT")
