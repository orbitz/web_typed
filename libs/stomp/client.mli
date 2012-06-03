open Ort_prelude

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

type 'a frame
type headers = (string * string) list

type parse_state

type error =
  | Unknown_cmd of string
  | Exn of exn

val get_frame_type : 'a frame -> 'a
val get_header     : k:string -> 'a frame -> string option
val get_body       : 'a frame -> string

val connect        : ?h:headers -> (string * string) option -> outgoing frame
val send           : ?h:headers -> dst:string -> body:string -> outgoing frame
val subscribe      : ?h:headers -> ?ack:ack -> ?prefetch:int -> dst:string -> outgoing frame
val unsubscribe    : ?h:headers -> dst:string -> outgoing frame
val trans_begin    : ?h:headers -> outgoing frame
val trans_commit   : ?h:headers -> outgoing frame
val trans_abort    : ?h:headers -> outgoing frame
val ack            : ?h:headers -> mid:string -> outgoing frame
val disconnect     : outgoing frame

val frames_of_data : s:parse_state -> d:string -> ((incoming frame list * parse_state), error) Return.t
