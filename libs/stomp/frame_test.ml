open OUnit

open Stomp

let test_disconnect _ =
  let f = Frame.disconnect
  in
  assert_equal
    Frame.Disconnect
    (Frame.get_frame_type f);
  assert_equal
    (Some "0")
    (Frame.get_header "content-length" f)

let suite = "STOMP Frame Test" >:::
  [ "disconnect" >:: test_disconnect ]

let _ = run_test_tt_main suite
