open Core_extended.Std
open Ort_prelude
open Ort


(* Outgoing message types *)
type outgoing =
  | Connect
  | Send
  | Subscribe
  | Unsubscribe
  | Begin
  | Commit
  | Abort
  | Ack
  | Disconnect

type ack =
  | Client
  | Auto

(* Incoming message types *)
type incoming =
  | Connected
  | Message
  | Receipt
  | Error

type headers = (string * string) list

type 'a frame = { frame_type : 'a
		; headers    : headers
		; body       : string
		}

type parse_state = string


type error =
  | Unknown_cmd of string
  | Exn of exn

let make_frame t h b = 
  { frame_type = t
  ; headers    = h
  ; body       = b
  }

let header_add k v h = (k, v)::h
let header_get k h = List.Assoc.find h ~equal:(=) k

let string_of_ack = function
  | Client -> "client"
  | Auto   -> "auto"

let cmd_of_string = function
  | "CONNECTED" ->
    Return.Success Connected
  | "MESSAGE" ->
    Return.Success Message
  | "RECEIPT" ->
    Return.Success Receipt
  | "ERROR" ->
    Return.Success Error
  | unknown ->
    Return.Failure (Unknown_cmd unknown)

let get_frame_type frame = frame.frame_type

let get_header ~k frame = header_get k frame.headers

let get_body frame = frame.body

let connect ?(h = []) = function
  | None ->
    make_frame Connect h ""
  | Some (username, password) ->
    let h = h
            |> header_add "username" username
	    |> header_add "password" password
    in
    make_frame Connect h ""

let send ?(h = []) ~dst ~body =
  make_frame
    Send
    (header_add "destination" dst h)
    body

let subscribe ?(h = []) ?(ack = Client) ?(prefetch = 1) ~dst =
  let h = h
          |> header_add "destination" dst
	  |> header_add "prefetch" (string_of_int prefetch)
	  |> header_add "ack" (string_of_ack ack)
  in
  make_frame Subscribe h ""

let unsubscribe ?(h = []) ~dst =
  let h = h |> header_add "destination" dst
  in
  make_frame Unsubscribe h ""

let trans_begin ?(h = []) = raise (Failure "Not implemented")

let trans_commit ?(h = []) = raise (Failure "Not implemented")

let trans_abort ?(h = []) = raise (Failure "Not implemented")

let ack ?(h = []) ~mid =
  make_frame
    Ack
    (header_add "message-id" mid h)
    ""

let disconnect =
  make_frame Disconnect [] ""

let rec parse_msg m =
  Return.lift (fun () -> (msg m))
and msg = parser
  | [< c = text; ''\n'; h = headers; b = body >] -> (c, h, b)
and headers = parser
  | [< ''\n' >] -> []
  | [< key = text; '':'; value = text; ''\n'; hs = headers >] -> (key, value)::hs
and text s = s |> Seq.take_while ~f:((<>) '\n') |> Seq.to_list |> string_of_list
and body s = s |> Seq.to_list |> string_of_list


let rec frames_of_data ~s ~d =
  let s = s ^ d 
  in
  match String.lsplit2 ~on:'\000' s with
    | None ->
      Return.Success ([], s)
    | Some (msg, rest) -> begin
      match parse_msg (Seq.of_string msg) with
	| Return.Success (cmd, headers, body) -> begin
	  match cmd_of_string cmd with
	    | Return.Success cmd -> begin
	      let frame = make_frame cmd headers body
	      in
	      match frames_of_data ~s:rest ~d:"" with
		| Return.Success (frames, leftover) ->
		  Return.Success (frame::frames, leftover)
		| Return.Failure f ->
		  Return.Failure f
	    end
	    | Return.Failure f ->
	      Return.Failure f
	end
	| Return.Failure f ->
	  Return.Failure (Exn f)
    end
      
