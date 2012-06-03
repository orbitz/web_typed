module Return = struct
  type ('a, 'b) t =
    | Success of 'a
    | Failure of 'b
	
  let lift f =
    try
      Success (f ())
    with
      | anything ->
	Failure anything
end

let (|>) d f = f d

