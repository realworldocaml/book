open! Import
open! Int_conversions

let%test_module "pretty" =
  (module struct
    let check input output =
      List.for_all [ ""; "+"; "-" ] ~f:(fun prefix ->
        let input = prefix ^ input in
        let output = prefix ^ output in
        [%compare.equal: string] output (insert_underscores input))
    ;;

    let%test _ = check "1" "1"
    let%test _ = check "12" "12"
    let%test _ = check "123" "123"
    let%test _ = check "1234" "1_234"
    let%test _ = check "12345" "12_345"
    let%test _ = check "123456" "123_456"
    let%test _ = check "1234567" "1_234_567"
    let%test _ = check "12345678" "12_345_678"
    let%test _ = check "123456789" "123_456_789"
    let%test _ = check "1234567890" "1_234_567_890"
  end)
;;

let%test_module "conversions" =
  (module struct
    module type S = sig
      include Int.S

      val module_name : string
    end

    let test_conversion (type a b) loc ma mb a_to_b_or_error a_to_b_trunc b_to_a_trunc =
      let (module A : S with type t = a) = ma in
      let (module B : S with type t = b) = mb in
      let examples =
        [ A.min_value
        ; A.minus_one
        ; A.zero
        ; A.one
        ; A.max_value
        ; B.min_value |> b_to_a_trunc
        ; B.max_value |> b_to_a_trunc
        ]
        |> List.concat_map ~f:(fun a -> [ A.pred a; a; A.succ a ])
        |> List.dedup_and_sort ~compare:A.compare
        |> List.sort ~compare:A.compare
      in
      List.iter examples ~f:(fun a ->
        let b' = a_to_b_trunc a in
        let a' = b_to_a_trunc b' in
        match a_to_b_or_error a with
        | Ok b ->
          require
            loc
            (B.equal b b')
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion produced wrong value"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~output:(b : B.t)
                    ~expected:(b' : B.t)]);
          require
            loc
            (A.equal a a')
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion does not round-trip"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~output:(b : B.t)
                    ~round_trip:(a' : A.t)])
        | Error error ->
          require
            loc
            (not (A.equal a a'))
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion failed"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~expected_output:(b' : B.t)
                    ~error:(error : Error.t)]))
    ;;

    let test loc ma mb (a_to_b_trunc, a_to_b_or_error) (b_to_a_trunc, b_to_a_or_error) =
      test_conversion loc ma mb a_to_b_or_error a_to_b_trunc b_to_a_trunc;
      test_conversion loc mb ma b_to_a_or_error b_to_a_trunc a_to_b_trunc
    ;;

    module Int = struct
      include Int

      let module_name = "Int"
    end

    module Int32 = struct
      include Int32

      let module_name = "Int32"
    end

    module Int64 = struct
      include Int64

      let module_name = "Int64"
    end

    module Nativeint = struct
      include Nativeint

      let module_name = "Nativeint"
    end

    let with_exn f x = Or_error.try_with (fun () -> f x)
    let optional f x = Or_error.try_with (fun () -> Option.value_exn (f x))
    let alwaysok f x = Ok (f x)

    let%expect_test "int <-> int32" =
      test
        [%here]
        (module Int)
        (module Int32)
        (Caml.Int32.of_int, with_exn int_to_int32_exn)
        (Caml.Int32.to_int, with_exn int32_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Int32)
        (Caml.Int32.of_int, optional int_to_int32)
        (Caml.Int32.to_int, optional int32_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int <-> int64" =
      test
        [%here]
        (module Int)
        (module Int64)
        (Caml.Int64.of_int, alwaysok int_to_int64)
        (Caml.Int64.to_int, with_exn int64_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Int64)
        (Caml.Int64.of_int, alwaysok int_to_int64)
        (Caml.Int64.to_int, optional int64_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int <-> nativeint" =
      test
        [%here]
        (module Int)
        (module Nativeint)
        (Caml.Nativeint.of_int, alwaysok int_to_nativeint)
        (Caml.Nativeint.to_int, with_exn nativeint_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Nativeint)
        (Caml.Nativeint.of_int, alwaysok int_to_nativeint)
        (Caml.Nativeint.to_int, optional nativeint_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int32 <-> int64" =
      test
        [%here]
        (module Int32)
        (module Int64)
        (Caml.Int64.of_int32, alwaysok int32_to_int64)
        (Caml.Int64.to_int32, with_exn int64_to_int32_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int32)
        (module Int64)
        (Caml.Int64.of_int32, alwaysok int32_to_int64)
        (Caml.Int64.to_int32, optional int64_to_int32);
      [%expect {| |}]
    ;;

    let%expect_test "int32 <-> nativeint" =
      test
        [%here]
        (module Int32)
        (module Nativeint)
        (Caml.Nativeint.of_int32, alwaysok int32_to_nativeint)
        (Caml.Nativeint.to_int32, with_exn nativeint_to_int32_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int32)
        (module Nativeint)
        (Caml.Nativeint.of_int32, alwaysok int32_to_nativeint)
        (Caml.Nativeint.to_int32, optional nativeint_to_int32);
      [%expect {| |}]
    ;;

    let%expect_test "int64 <-> nativeint" =
      test
        [%here]
        (module Int64)
        (module Nativeint)
        (Caml.Int64.to_nativeint, with_exn int64_to_nativeint_exn)
        (Caml.Int64.of_nativeint, alwaysok nativeint_to_int64);
      [%expect {| |}];
      test
        [%here]
        (module Int64)
        (module Nativeint)
        (Caml.Int64.to_nativeint, optional int64_to_nativeint)
        (Caml.Int64.of_nativeint, alwaysok nativeint_to_int64);
      [%expect {| |}]
    ;;
  end)
;;
