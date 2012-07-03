open Async.Std

open Ort_async

type host = string
type port = int
type args = (host * port)
type init_fail = Bad_host
type state = { parent : [> `Data of string ] -> unit
	     ; exited : Gen_server.exited Ivar.t
	     ; r      : Reader.t
	     ; w      : Writer.t
	     }
type msg = [ `Write of string
	   | `Close
	   ]

type t = { q       : msg Tail.t
	 ; exited  : Gen_server.exited Ivar.t
	 ; id      : unit ref
	 }


let rec loop self state =
  

let start args = ok

let call msg s = Tail.extend s.q msg

let exited s = s.exited

let equal s1 s2 = s1.id == s2.id
