open Base

let choose ~default body =
  let terminators = Lexer.extract_quoted_string_terminators body in
  let rec loop tag =
    if List.mem terminators tag ~equal:String.equal then
      loop (tag ^ "x")
    else
      tag
  in
  if List.mem terminators default ~equal:String.equal then
    loop (if String.is_empty default then "xxx" else default ^ "_xxx")
  else
    default
;;

let%test_module _ =
  (module struct
    let test body = [%test_result: string] (choose ~default:"" body)

    let%test_unit _ = test "nice text"                                ~expect:""
    let%test_unit _ = test "with embedded |} somewhere"               ~expect:"xxx"
    let%test_unit _ = test "with embedded |a} somewhere"              ~expect:""
    let%test_unit _ = test "with embedded |xxx} somewhere"            ~expect:""
    let%test_unit _ = test "double - |} and |xxx} - embedding"        ~expect:"xxxx"

    let testD body = [%test_result: string] (choose ~default:"default" body)

    let%test_unit _ = testD "nice text"                               ~expect:"default"
    let%test_unit _ = testD "with embedded |} somewhere"              ~expect:"default"
    let%test_unit _ = testD "with embedded |default} somewhere"       ~expect:"default_xxx"
    let%test_unit _ = testD "double - |default} and |default_xxx}"    ~expect:"default_xxxx"
  end)
