open Async.Std
open Ort_prelude

type ('a, 'b) t = ('a, 'b) Result.t Deferred.t

val bind   : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val fail   : 'a -> (_, 'a) t
val return : 'a -> ('a, _) t
