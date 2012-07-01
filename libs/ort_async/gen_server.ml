(*
 * This implements an Erlang-style gen_server.
 *
 * An implementor needs to provide the following
 * callbacks:
 *
 * - init: Takes arguments and returns a state
 * - handle_call: Takes a call and returns a state
 *                These are serialized
 * - terminate: Called upon termination with state
 *)
open Core.Std
open Async.Std

module type GEN_SERVER = sig
  type args
  type state
  type msg

  val init : msg Tail.t -> args -> (state, 'b) Result.t Deferred.t
  val handle_call : state -> msg -> ([`Cont | `Stop] * state, 'a) Result.t Deferred.t
  val terminate: state -> unit
end

module Make = functor (Gs : GEN_SERVER) -> struct
  type args  = Gs.args
  type state = Gs.state
  type msg   = Gs.msg

  type queue = msg Tail.t
  type exited = Normal | Failed

  type t = { q      : queue
	   ; exited : exited Ivar.t
	   }

  let make_gs () = { q      = Tail.create ()
		   ; exited = Ivar.create ()
		   }

  let do_call state msg =
    Deferred.bind
      (Monitor.try_with
	 (fun () -> Gs.handle_call state msg))
      (function
	| Result.Ok (Result.Ok next) ->
	  Deferred.return (Result.Ok next)
	| Result.Ok (Result.Error err) ->
	  Deferred.return (Result.Error (`Error err))
	| Result.Error exn ->
	  Deferred.return (Result.Error (`Exn exn)))

  let terminate_failure self state =
    Ivar.fill self.exited Failed;
    Gs.terminate state

  let terminate_normal self state =
    Ivar.fill self.exited Normal;
    Gs.terminate state

  let rec loop self state = function
    | Stream.Nil -> raise (Failure "Not implemented")
    | Stream.Cons (msg, msg') ->
      Deferred.upon
	(do_call state msg)
	(function
	  | Result.Ok (`Cont, state') ->
	    Deferred.upon
	      (Stream.next msg')
	      (loop self state')
	  | Result.Ok (`Stop, state') ->
	    terminate_normal self state'
	  | Result.Error _ ->
	    terminate_failure self state)

  let start args =
    let self = make_gs ()
    in
    Deferred.bind
      (Gs.init self.q args)
      (function
	| Result.Ok state -> begin
	  Deferred.upon
	    (Stream.next (Tail.collect self.q))
	    (loop self state);
	  Deferred.return (Result.Ok self)
	end
	| Result.Error f ->
	  Deferred.return (Result.Error f))

  let call msg gs = Tail.extend gs.q msg

  let exited gs = gs.exited
end

