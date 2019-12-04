open! Import
open Option_array


let%test_module "Cheap_option" = (
  module struct
    open For_testing.Unsafe_cheap_option

    let roundtrip_via_cheap_option (type a) (x : a) =
      let opt : a t = some x in
      assert (is_some opt);
      assert (phys_equal (value_exn opt) x)

    let%test_unit _ =
      roundtrip_via_cheap_option 0
    let%test_unit _ =
      roundtrip_via_cheap_option 1
    let%test_unit _ =
      roundtrip_via_cheap_option (ref 0)
    let%test_unit _ =
      roundtrip_via_cheap_option `x6e8ee3478e1d7449
    let%test_unit _ =
      roundtrip_via_cheap_option 0.0
    let%test _ =
      not (is_some none)

    let%test_unit "memory corruption" =
      let make_list () =
        List.init ~f:(fun i -> Some i) 5
      in
      Caml.Gc.minor ();
      let x = value_unsafe (some (make_list ())) in
      Caml.Gc.minor ();
      let _ = List.init ~f:(fun i -> Some (i*100)) 10000 in
      [%test_result: Int.t Option.t List.t]
        ~expect:(make_list ()) x
  end)

module Sequence = struct
  let length = length
  let get = get
  let set = set
end

include Base_for_tests.Test_blit.Test1_generic(struct
    include Option

    let equal a b = Option.equal Bool.equal a b
    let of_bool b = Some b
  end) (struct
    type nonrec 'a t = 'a t [@@deriving sexp]
    type 'a z = 'a
    include Sequence

    let create_bool ~len = init_some len ~f:(fun _ -> false)
  end)(Option_array)


let%test_unit "floats are not re-boxed" =
  let one = 1.0 in
  let array = init_some 1 ~f:(fun _ -> one) in
  assert (phys_equal one (get_some_exn array 0))

let%test_unit "segfault does not happen" =
  (* if [Core_array] is used instead of [Uniform_array], this dies with a segfault *)
  let _array = init 2 ~f:(fun i ->
    if i = 0 then Some 1.0 else None)
  in
  ()
