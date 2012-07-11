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

module Make : functor (Gs : GEN_SERVER) -> sig
  type args       = Gs.args
  type start_fail = Gs.init_fail
  type state      = Gs.state
  type msg        = Gs.msg
  type queue      = msg Tail.t

  type t

  val start  : args -> (t, start_fail) Result.t Deferred.t
  val call   : msg -> t -> unit
  val exited : t -> exited
  val equal  : t -> t -> bool
end
