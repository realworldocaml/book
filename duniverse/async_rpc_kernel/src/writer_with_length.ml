open Core_kernel
open Poly

let of_writer { Bin_prot.Type_class. write; size } =
  let write buf ~pos a =
    let len = Nat0.of_int_exn (size a) in
    let pos = Nat0.bin_write_t buf ~pos len in
    write buf ~pos a
  in
  let size a =
    let len = Nat0.of_int_exn (size a) in
    Nat0.bin_size_t len + ((len : Bin_prot.Nat0.t) :> int)
  in
  { Bin_prot.Type_class. write; size }
;;

let of_type_class (bin_a : _ Bin_prot.Type_class.t) = of_writer bin_a.writer

let%test_module _ = (module struct
  let bigstring_bin_prot s =
    let bigstring = Bin_prot.Utils.bin_dump Bytes.bin_writer_t s in
    Bin_prot.Utils.bin_dump Bigstring.Stable.V1.bin_writer_t bigstring
  ;;

  let bin_prot_with_length s =
    let writer_with_length = of_writer Bytes.bin_writer_t in
    Bin_prot.Utils.bin_dump writer_with_length s
  ;;

  let test len =
    let s = Bytes.create len in
    let bigstring_version = bigstring_bin_prot s in
    let with_length_version = bin_prot_with_length s in
    if Bigstring.to_string bigstring_version <> Bigstring.to_string with_length_version
    then failwithf "mismatch for length %d" len ()
  ;;

  let%test_unit _ =
    for len = 0 to Int.pow 2 10 do test len done;
    for pow = 10 to 20 do
      let x = Int.pow 2 pow in
      test (x - 1);
      test x;
      test (x + 1);
    done
  ;;
end)
