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

type t

val start  : args -> (t, init_fail) Result.t Deferred.t
val call   : msg -> t -> unit
val exited : t -> unit
val equal  : t -> t -> bool
