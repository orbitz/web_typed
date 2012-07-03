module Result : sig
  include module type of Core.Result
  val lift : (unit -> 'a) -> ('a, exn) t

  val bind   : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val return : 'a -> ('a, _) t
end

val (|>) : 'a -> ('a -> 'b) -> 'b
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val string_of_list : char list -> string
