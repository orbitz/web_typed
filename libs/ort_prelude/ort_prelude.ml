module Result = struct
  include Core.Result

  let lift f =
    try
      Ok (f ())
    with
      | exn ->
	Error exn

  let bind m f =
    match m with
      | Ok v ->
	f v
      | Error err ->
	Error err

  let return v = Ok v
end

let (|>) d f = f d

let string_of_list l =
  let b = Buffer.create (List.length l) in
  l |> List.iter (Buffer.add_char b);
  Buffer.contents b
