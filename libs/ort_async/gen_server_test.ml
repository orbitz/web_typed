open Core.Std
open Async.Std

module Test_server_impl = struct
  type args  = int
  type state = int
  type msg = [ `Incr of int | `Get of int Ivar.t ]

  let init start = Deferred.return (Result.Ok start)

  let handle_call state = function
    | `Incr n  -> Deferred.return (Result.Ok (`Cont, state + n))
    | `Get ret -> (Ivar.fill ret state; Deferred.return (Result.Ok (`Cont, state)))

  let terminate _ = ()
end

module Test_server = Ort_async.Gen_server.Make(Test_server_impl)

let incr gs n = Test_server.call (`Incr n) gs
let get gs    = let ivar = Ivar.create ()
		in
		Test_server.call (`Get ivar) gs;
		ivar

let test_gen_server () =
  Deferred.upon
    (Test_server.start 0)
    (function
      | Result.Ok gs -> begin
	incr gs 1;
	incr gs 2;
	Deferred.upon
	  (Ivar.read (get gs))
	  (function
	    | 3 -> Shutdown.shutdown 0
	    | n -> begin
	      Printf.printf "Error: Got %d\n" n;
	      Shutdown.shutdown 1
	    end)
      end
      | Result.Error _ ->
	Shutdown.shutdown 2)

let () =
  test_gen_server ();
  never_returns (Scheduler.go ())
