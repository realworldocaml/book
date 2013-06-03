open Core.Std
open Core_bench.Std

let alloc_linear () =
  for i = 1024 to 4096 do
    ignore(String.create i)
  done;
  for i = 4096 downto 1024 do
    ignore(String.create i)
  done

let alloc_constant () =
  for i = 1024 to 4096 do
    ignore(String.create 4096)
  done

let alloc_large_then_small () =
  ignore(String.create (1024 * 1024 * 1024));
  alloc_linear ()
 
let set_allocator a =
  Gc.tune ~allocation_policy:a ()

let mk_test ~name ~a f =
  let test () = set_allocator a; f () in
  Bench.Test.create ~name test

let () =
  let tests = [
    mk_test ~name:"Constant next-fit" ~a:0 alloc_constant;
    mk_test ~name:"Constant first-fit" ~a:1 alloc_constant;
    mk_test ~name:"Linear next-fit" ~a:0 alloc_linear;
    mk_test ~name:"Linear first-fit" ~a:1 alloc_linear;
    mk_test ~name:"Large then small next-fit" ~a:0 alloc_large_then_small;
    mk_test ~name:"Large then small first-fit" ~a:1 alloc_large_then_small;
  ] in
  Bench.make_command tests |> Command.run
