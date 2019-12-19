
open Core

module Tests(Hash : Base.Hash.S with type hash_value = int) = struct

  module Ppx_hash_lib = struct
    module Std = struct module Hash = Base.Hash.F(Hash) end
  end
  module Hash = Ppx_hash_lib.Std.Hash
  open Hash.Builtin

  let hash_fold_int s t =
    (* nanosleep forces threads to yield *)
    ignore (Unix.nanosleep 0.01);
    hash_fold_int s t

  let run_in_two_threads f1 x1 f2 x2 =
    let res1 = ref None in
    let t = Thread.create (fun () ->
      res1 := Some (f1 x1)
    ) ()
    in
    let res2 = f2 x2 in
    Thread.join t;
    let res1 = match !res1 with Some x -> x | None -> assert false in
    res1,res2

  module T = struct
    let some_list = List.init 10 ~f:Fn.id
    let h () = [%hash: int list] some_list
    let res0 = h ()
    let res1,res2 = run_in_two_threads h () h ()
    let%test_unit _ = [%test_result: int] res1 ~expect:res0
    let%test_unit _ = [%test_result: int] res2 ~expect:res0
  end

end

module T1 = Tests(Base.Hash)
module T2 = Tests(Siphash_lib.Siphash)
