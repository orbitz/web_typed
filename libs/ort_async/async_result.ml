open Async.Std
open Ort_prelude

type ('a, 'b) t = ('a, 'b) Result.t Deferred.t

let bind m f =
  Deferred.bind
    m
    (function
      | Result.Ok v ->
	f v
      | Result.Error err ->
	Deferred.return (Result.Error err))

let fail f =
  Deferred.return (Result.Error f)

let return v =
  Deferred.return (Result.Ok v)
