open! Core

let int_to_string =
  Core_bench_js.Test.create ~name:"Int.to_string" (fun () -> Int.to_string 5)
;;

let float_to_string =
  Core_bench_js.Test.create ~name:"Float.to_string" (fun () -> Float.to_string 5.0)
;;

let sprintf_based =
  Core_bench_js.Test.create ~name:"sprintf %.12g" (fun () -> sprintf "%.12g" 5.0)
;;

let fts : float -> string =
  Js_of_ocaml.Js.Unsafe.pure_js_expr {| (function (f) {return f.toString();}) |}
;;

let c_fts : float -> string =
  Js_of_ocaml.Js.Unsafe.pure_js_expr {| (function (f) {return ""+f;}) |}
;;

let js_float_to_string =
  Core_bench_js.Test.create ~name:"float.toString()" (fun () -> fts 5.0)
;;

let c_js_float_to_string =
  Core_bench_js.Test.create ~name:"\"\" + float" (fun () -> c_fts 5.0)
;;

let () =
  Core_bench_js.bench
    [ int_to_string
    ; float_to_string
    ; sprintf_based
    ; js_float_to_string
    ; c_js_float_to_string
    ]
;;

let printf_int =
  Core_bench_js.Test.create ~name:"sprintf \"%dpx\" int" (fun () -> sprintf "%dpx" 5)
;;

let concat_to_int =
  Core_bench_js.Test.create ~name:"(Int.to_string int) ^ \"px\"" (fun () ->
    Int.to_string 5 ^ "px")
;;

let () = Core_bench_js.bench [ printf_int; concat_to_int ]
