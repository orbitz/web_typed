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

type 'a t
type headers = (string * string) list

type parse_state

type error =
  | Unknown_cmd of string
  | Exn of exn

val get_frame_type : 'a t -> 'a
val get_header     : k:string -> 'a t -> string option
val get_body       : 'a t -> string

val connect        : ?h:headers -> (string * string) option -> outgoing t
val send           : ?h:headers -> dst:string -> body:string -> outgoing t
val subscribe      : ?h:headers -> ?ack:ack -> ?prefetch:int option -> dst:string -> outgoing t
val unsubscribe    : ?h:headers -> dst:string -> outgoing t
val trans_begin    : ?h:headers -> outgoing t
val trans_commit   : ?h:headers -> outgoing t
val trans_abort    : ?h:headers -> outgoing t
val ack            : ?h:headers -> mid:string -> outgoing t
val disconnect     : outgoing t

val parse_state    : parse_state
val frames_of_data : s:parse_state -> d:string -> ((incoming t list * parse_state), error) Result.t
