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

type 'a t = { frame_type : 'a
	    ; headers    : headers
	    ; body       : string
	    }

type parse_state = string

exception Unknown_cmd of string
exception Frame_length

let header_add k v h = (k, v)::h
let header_get k h = List.Assoc.find h ~equal:(=) k

let content_length body h =
  header_add
    "content-length"
    (string_of_int (String.length body))
    h

let string_of_ack = function
  | Client -> "client"
  | Auto   -> "auto"

let cmd_of_string = function
  | "CONNECTED" ->
    Result.Ok Connected
  | "MESSAGE" ->
    Result.Ok Message
  | "RECEIPT" ->
    Result.Ok Receipt
  | "ERROR" ->
    Result.Ok Error
  | unknown ->
    Result.Error (Unknown_cmd unknown)

let string_of_cmd = function
  | Connect     -> "CONNECT"
  | Send        -> "SEND"
  | Subscribe   -> "SUBSCRIBE"
  | Unsubscribe -> "UNSUBSCRIBE"
  | Begin       -> "BEGIN"
  | Commit      -> "COMMIT"
  | Abort       -> "ABORT"
  | Ack         -> "ACK"
  | Disconnect  -> "DISCONNECT"

let make_frame t h b =
  let h = content_length b h
  in
  { frame_type = t
  ; headers    = h
  ; body       = b
  }


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

let subscribe ?(h = []) ?(ack = Client) ?(prefetch = None) ~dst =
  let h = h
          |> header_add "destination" dst
	  |> header_add "ack" (string_of_ack ack)
  in
  let h =
    match prefetch with
      | None ->
	h
      | Some p ->
	header_add "prefect" (string_of_int p) h
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

(*
 * We want to parse frames, where each frame
 * looks like:
 *
 * COMMAND\n
 * HEADER_KEY: VALUE\n
 * HEADER_KEY: VALUE\n
 * \n
 * BODY\000
 *
 * A frame can have a content-length header
 * that defines how long the BODY is supposed
 * to be, which is useful in the BODY has
 * a \000 in it.
 *
 * parse_frames takes a stream of characters
 * and returns a list of parsed frames.
 *)
let rec parse_frames m =
  Result.lift (fun () -> msg m)
and msg = parser
  | [< f = frame; fs = frame_aux >] -> f::fs
and frame = parser
  | [< c = text
    ;  ''\n' ?? "newline expected after command"
    ;  h = headers
    ;  b = body h
    >] -> (c, h, b)
and frame_aux = parser
  | [< ''\000'; f = frame; fs = frame_aux >] -> f::fs
  | [< >] -> []
and headers = parser
  | [< ''\n' >] -> []
  | [< key = header_key
    ;  '':' ?? "Colon expected"
    ;  value = header_value
    ;  ''\n' ?? "Newline expected after header"
    ;  hs = headers
    >] -> (key, value)::hs
and text s = s |> Seq.take_while ~f:((<>) '\n') |> Seq.to_list |> string_of_list
and header_key s = s |> Seq.take_while ~f:((<>) ':') |> Seq.to_list |> string_of_list
and header_value s = s |> text |> String.strip
and body h s =
  match header_get "content-length" h with
    | None ->
      s |> Seq.take_while ~f:((<>) '\000') |> Seq.to_list |> string_of_list
    | Some l ->
      let l = int_of_string l
      in
      let str = s |> Seq.take l |> Seq.to_list |> string_of_list
      in
      if String.length str = l then
	str
      else
	raise Frame_length


let parse_state = ""

let rec frames_of_tuples accum = function
  | [] ->
    Result.Ok (List.rev accum)
  | (cmd, headers, body)::fs -> begin
    match cmd_of_string cmd with
      | Result.Ok cmd ->
	let frame = make_frame cmd headers body
	in
	frames_of_tuples (frame::accum) fs
      | Result.Error f ->
	Result.Error f
  end

let rec frames_of_data ~s ~d =
  let s = s ^ d
  in
  match String.rsplit2 ~on:'\000' s with
    | None ->
      Result.Ok ([], s)
    | Some (msgs, rest) -> begin
      perform with module Result in
	fs <-- parse_frames (Seq.of_string msgs);
	frames <-- frames_of_tuples [] fs;
	Result.return (frames, rest)
    end

let to_string f =
  let headers =
    String.concat
      ~sep:"\n"
      (List.map ~f:(fun (k, v) -> k ^ ":" ^ v) f.headers)
  in
  String.concat
    ~sep:"\n"
    [ string_of_cmd f.frame_type
    ; headers
    ; f.body ^ "\000"
    ]
