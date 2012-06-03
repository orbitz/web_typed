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

let string_of_list l =
  let b = Buffer.create (List.length l) in
  l |> List.iter (Buffer.add_char b);
  Buffer.contents b
