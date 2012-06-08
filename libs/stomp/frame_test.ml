open OUnit

open Stomp

let test_disconnect _ =
  let f = Frame.disconnect
  in
  assert_equal
    Frame.Disconnect
    (Frame.get_frame_type f)

let suite = "STOMP Frame Test" >:::
  [ "disconnect" >:: test_disconnect ]

let _ = run_test_tt_main suite
