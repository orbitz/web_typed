open Core.Std
open Async.Std

open Stomp

module Sub_map = Map.Make(String)

type args = (string * int * (string * string) option)

type subscription = (Frame.headers * string) Tail.t

type state = { subs        : subscription Sub_map.t
	     ; parse_state : Frame.parse_state
	     ; buffer      : string
	     ; r           : Reader.t
	     ; w           : Writer.t
	     }

type msg = [ `Send of (string * Frame.headers * string)
	   | `Subscribe of (Frame.headers * string)
	   | `Unsubscribe of (Frame.headers * string)
	   | `Ack of (Frame.headers * string)
	   | `Recv of Frame.t
	   | `Stop
	   ]

let init self (host, port, login) = ok

let handle_call state msg = Deferred.return (handle_call' state msg)

let handle_call' state = function
  | `Send (dst, hs, body) -> begin
    Writer.write
      state.w
      (Frame.to_string (Frame.send ~h:hs ~dst:dst ~body:body));
    Result.Ok (`Cont, state)
  end
  | `Subscribe -> Result.Ok (`Cont, state)
  | `Unsubscribe -> Result.Ok (`Cont, state)
  | `Ack (hs, mid) -> begin
    Writer.write
      state.w
      (Frame.to_string (Frame.ack ~h:hs ~mid:mid));
    Result.Ok (`Cont, state)
  end
  | `Recv frame -> Result.Ok (`Cont, state)
  | `Stop ->
    Result.Ok (`Stop, state)

let terminate state =
  Sub_map.iter (fun _ s -> Tail.close_if_open s) state.subs

