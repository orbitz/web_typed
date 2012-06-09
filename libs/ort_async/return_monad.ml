open Async.Std
open Ort_prelude

type ('a, 'b) t = ('a, 'b) Return.t Deferred.t

let bind m f =
  Deferred.bind
    m
    (function
      | Return.Success v ->
	f v
      | Return.Failure f ->
	Deferred.return (Return.Failure f))

let fail f =
  Deferred.return (Return.Failure f)

let return v =
  Deferred.return (Return.Success v)
