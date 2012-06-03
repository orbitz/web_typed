module Return : sig 
  type ('a, 'b) t =
    | Success of 'a
    | Failure of 'b

  val lift : (unit -> 'a) -> ('a, exn) t
end

val (|>) : 'a -> ('a -> 'b) -> 'b
