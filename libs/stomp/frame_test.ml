open OUnit

open Stomp
open Ort_prelude

let test_parse_single_msg _ =
  let data = "CONNECTED\nsession:foo\n\n\000"
  and parse_state = Frame.parse_state
  in
  match Frame.frames_of_data ~s:parse_state ~d:data with
    | Result.Error (Frame.Unknown_cmd cmd) ->
      assert_string ("Unknown command: " ^ cmd)
    | Result.Error Stream.Failure ->
      assert_string "Stream failure"
    | Result.Error (Stream.Error error) ->
      assert_string ("Stream error: " ^ error)
    | Result.Error _ ->
      assert_string "Unknown exception thrown"
    | Result.Ok ([frame], _) -> begin
      assert_equal
	~msg:"Not Connected frame"
	Frame.Connected
	(Frame.get_frame_type frame);
      assert_equal
	~msg:"Body not empty"
	""
	(Frame.get_body frame);
      assert_equal
	~msg:"Session not foo"
	(Some "foo")
	(Frame.get_header "session" frame)
    end
    | Result.Ok (_, _) ->
      assert_string "Parse wrong number of messages"

let test_parse_double_msg _ =
  let msg1 = "CONNECTED\nsession:foo\n\n\000"
  and msg2 = "RECEIPT\nreceipt-id:bar\n\n\000"
  and parse_state = Frame.parse_state
  in
  let data = msg1 ^ msg2
  in
  match Frame.frames_of_data ~s:parse_state ~d:data with
    | Result.Error (Frame.Unknown_cmd cmd) ->
      assert_string ("Unknown command: " ^ cmd)
    | Result.Error Stream.Failure ->
      assert_string "Stream failure"
    | Result.Error (Stream.Error error) ->
      assert_string ("Stream error: " ^ error)
    | Result.Error _ ->
      assert_string "Unknown exception thrown"
    | Result.Ok ([frame1; frame2], _) -> begin
      assert_equal
	~msg:"Not Connected frame"
	Frame.Connected
	(Frame.get_frame_type frame1);
      assert_equal
	~msg:"Body not empty"
	""
	(Frame.get_body frame1);
      assert_equal
	~msg:"Session not foo"
	(Some "foo")
	(Frame.get_header "session" frame1);
      assert_equal
	~msg:"Not Receipt frame"
	Frame.Receipt
	(Frame.get_frame_type frame2);
      assert_equal
	~msg:"Body not empty"
	""
	(Frame.get_body frame2);
      assert_equal
	~msg:"receipt-id not bar"
	(Some "bar")
	(Frame.get_header "receipt-id" frame2);
    end
    | Result.Ok (_, _) ->
      assert_string "Parse wrong number of messages"

let test_parse_with_null _ =
  let data = "MESSAGE\ndestination:/topic/foo\ncontent-length:1\n\n\000\000"
  and parse_state = Frame.parse_state
  in
  match Frame.frames_of_data ~s:parse_state ~d:data with
    | Result.Error (Frame.Unknown_cmd cmd) ->
      assert_string ("Unknown command: " ^ cmd)
    | Result.Error Stream.Failure ->
      assert_string "Stream failure"
    | Result.Error (Stream.Error error) ->
      assert_string ("Stream error: " ^ error)
    | Result.Error _ ->
      assert_string "Unknown exception thrown"
    | Result.Ok ([frame], _) -> begin
      assert_equal
	~msg:"Not Message frame"
	Frame.Message
	(Frame.get_frame_type frame);
      assert_equal
	~msg:"Body not null byte"
	"\000"
	(Frame.get_body frame);
      assert_equal
	~msg:"destination not /topic/foo"
	(Some "/topic/foo")
	(Frame.get_header "destination" frame)
    end
    | Result.Ok (_, _) ->
      assert_string "Parse wrong number of messages"

let test_space_in_header _ =
  let data = "CONNECTED\nsession: foo\n\n\000"
  and parse_state = Frame.parse_state
  in
  match Frame.frames_of_data ~s:parse_state ~d:data with
    | Result.Error (Frame.Unknown_cmd cmd) ->
      assert_string ("Unknown command: " ^ cmd)
    | Result.Error Stream.Failure ->
      assert_string "Stream failure"
    | Result.Error (Stream.Error error) ->
      assert_string ("Stream error: " ^ error)
    | Result.Error _ ->
      assert_string "Unknown exception thrown"
    | Result.Ok ([frame], _) -> begin
      assert_equal
	~msg:"Not Connected frame"
	Frame.Connected
	(Frame.get_frame_type frame);
      assert_equal
	~msg:"Body not empty"
	""
	(Frame.get_body frame);
      assert_equal
	~msg:"Session not set to foo"
	(Some "foo")
	(Frame.get_header "session" frame)
    end
    | Result.Ok (_, _) ->
      assert_string "Parse wrong number of messages"

let suite = "STOMP Frame Test" >:::
  [ "Single message" >:: test_parse_single_msg
  ; "Double mssage" >:: test_parse_double_msg
  ; "Null in body" >:: test_parse_with_null
  ; "Space in header" >:: test_space_in_header
  ]

let _ = run_test_tt_main suite
