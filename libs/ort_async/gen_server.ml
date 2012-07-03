(*
 * This implements an Erlang-style gen_server.
 *
 * An implementor needs to provide the following
 * functions:
 *
 * - init: Takes its own message queue, its own exited Ivar and start args
 *         init is responsible for not throwing exceptions, or the caller of
 *         start is responsible for handling any exceptions thrown.
 *         Can return Result.Ok state or Result.Failure err
 * - handle_call: This handles a call.  The first parameter is the current
 *                state, the next is the message.  It can return the following
 *                vales:
 *                Result.Ok (`Cont, state) -- Continue with the new state
 *                Result.Ok (`Stop, state) -- Stop normally with the new state
 *                Result.Failure err       -- Exit with a failure
 *                raise an exception       -- Exit with an error
 * - terminate: Called upon termination with state
 *
 * The functor creates a module with the following functions:
 * - start: Start the server with the args
 *          On success Result.Ok server, on failure result.Failure err
 *          This does not handle init throwing exceptions.
 * - call: Takes a message and a server, asychronously sends the message
 * - exited: Returns an Ivar that will be filed with Normal or Failed
 *           that will be filled when the server exits.
 *)
open Ort_prelude
open Core.Std
open Async.Std

type exit_t = Normal | Failed
type exited = exit_t Ivar.t

module type GEN_SERVER = sig
  type args
  type init_fail
  type state
  type msg

  val init :
    ((msg -> unit) * exited) ->
    args ->
    (state, init_fail) Result.t Deferred.t

  val handle_call :
    state ->
    msg ->
    ([`Cont | `Stop] * state, 'a) Result.t Deferred.t

  val terminate: state -> unit
end

module Make = functor (Gs : GEN_SERVER) -> struct
  type args       = Gs.args
  type start_fail = Gs.init_fail
  type state      = Gs.state
  type msg        = Gs.msg
  type queue      = msg Tail.t

  type t = { q      : queue
	   ; exited : exited
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

  let call msg gs = Tail.extend gs.q msg

  let exited gs = gs.exited

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
      (Gs.init (flip call self, self.exited) args)
      (function
	| Result.Ok state -> begin
	  Deferred.upon
	    (Stream.next (Tail.collect self.q))
	    (loop self state);
	  Deferred.return (Result.Ok self)
	end
	| Result.Error f ->
	  Deferred.return (Result.Error f))
end

