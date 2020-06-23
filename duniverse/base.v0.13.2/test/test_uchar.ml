open! Import

let min_int = Int.min_value
let max_int = Int.max_value
let raises f v = Exn.does_raise (fun () -> f v)

let%test_module "test_constants" =
  (module struct
    let%test _ = Uchar.(to_scalar min_value) = 0x0000
    let%test _ = Uchar.(to_scalar max_value) = 0x10FFFF
  end)
;;

let%test_module "test_succ_exn" =
  (module struct
    let%test _ = raises Uchar.succ_exn Uchar.max_value
    let%test _ = Uchar.(to_scalar (succ_exn min_value)) = 0x0001
    let%test _ = Uchar.(to_scalar (succ_exn (of_scalar_exn 0xD7FF))) = 0xE000
    let%test _ = Uchar.(to_scalar (succ_exn (of_scalar_exn 0xE000))) = 0xE001
  end)
;;

let%test_module "test_pred_exn" =
  (module struct
    let%test _ = raises Uchar.pred_exn Uchar.min_value
    let%test _ = Uchar.(to_scalar (pred_exn (of_scalar_exn 0xD7FF))) = 0xD7FE
    let%test _ = Uchar.(to_scalar (pred_exn (of_scalar_exn 0xE000))) = 0xD7FF
    let%test _ = Uchar.(to_scalar (pred_exn max_value)) = 0x10FFFE
  end)
;;

let%test_module "test_int_is_scalar" =
  (module struct
    let%test _ = not (Uchar.int_is_scalar (-1))
    let%test _ = Uchar.int_is_scalar 0x0000
    let%test _ = Uchar.int_is_scalar 0xD7FF
    let%test _ = not (Uchar.int_is_scalar 0xD800)
    let%test _ = not (Uchar.int_is_scalar 0xDFFF)
    let%test _ = Uchar.int_is_scalar 0xE000
    let%test _ = Uchar.int_is_scalar 0x10FFFF
    let%test _ = not (Uchar.int_is_scalar 0x110000)
    let%test _ = not (Uchar.int_is_scalar min_int)
    let%test _ = not (Uchar.int_is_scalar max_int)
  end)
;;

let char_max = Uchar.of_scalar_exn 0x00FF

let%test_module "test_is_char" =
  (module struct
    let%test _ = Uchar.(is_char Uchar.min_value)
    let%test _ = Uchar.(is_char char_max)
    let%test _ = Uchar.(not (is_char (of_scalar_exn 0x0100)))
    let%test _ = not (Uchar.is_char Uchar.max_value)
  end)
;;

let%test_module "test_of_char" =
  (module struct
    let%test _ = Uchar.(equal (of_char '\xFF') char_max)
    let%test _ = Uchar.(equal (of_char '\x00') min_value)
  end)
;;

let%test_module "test_to_char_exn" =
  (module struct
    let%test _ = Char.equal Uchar.(to_char_exn min_value) '\x00'
    let%test _ = Char.equal Uchar.(to_char_exn char_max) '\xFF'
    let%test _ = raises Uchar.to_char_exn (Uchar.succ_exn char_max)
    let%test _ = raises Uchar.to_char_exn Uchar.max_value
  end)
;;

let%test_module "test_equal" =
  (module struct
    let%test _ = Uchar.(equal min_value min_value)
    let%test _ = Uchar.(equal max_value max_value)
    let%test _ = not Uchar.(equal min_value max_value)
  end)
;;

let%test_module "test_compare" =
  (module struct
    let%test _ = Uchar.(compare min_value min_value) = 0
    let%test _ = Uchar.(compare max_value max_value) = 0
    let%test _ = Uchar.(compare min_value max_value) = -1
    let%test _ = Uchar.(compare max_value min_value) = 1
  end)
;;
