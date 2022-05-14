open Core
open Ppxlib

let loc = Location.none

let print_expr expr =
  Pprintast.string_of_structure [%str let () = [%e expr]] |> print_string
;;

let%expect_test "single let%sub " =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      let a = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR ~f:(fun a -> MY_BODY) |}]
;;

let%expect_test "single pattern sub with modul" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    [%expr
      let a = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      X.Let_syntax.Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR ~f:(fun a -> MY_BODY) |}]
;;

let assert_fails_with_syntax_error ~f =
  try
    ignore (f ());
    assert false
  with
  | ex ->
    Location.Error.of_exn ex
    |> (fun a -> Option.value_exn a)
    |> Location.Error.message
    |> print_endline
;;

let%expect_test "double pattern let%sub" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      Ppx_let_expander.sub
      Ppx_let_expander.Extension_kind.default
      ~modul:None
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub should not be used with 'and'. |}]
;;

let%expect_test "single pattern sub open" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      let a = MY_EXPR_1 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } MY_EXPR_1 ~f:(fun a -> MY_BODY) |}]
;;

let%expect_test "double pattern map open" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      Ppx_let_expander.sub
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      [%expr
        let a = MY_EXPR_1
        and b = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub should not be used with 'and'. |}]
;;

let%expect_test "while%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      Ppx_let_expander.sub
      Ppx_let_expander.Extension_kind.default_open
      ~modul:None
      [%expr
        while a = MY_EXPR_1 do
          MY_BODY
        done]);
  [%expect {|
    while%sub is not supported |}]
;;

let%expect_test "if%sub is supported" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr if MY_EXPR_1 then BODY_1 else BODY_2]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Let_syntax.return MY_EXPR_1)
        ~f:(fun __pattern_syntax__004_ ->
              ((Let_syntax.switch
                  ~match_:((Let_syntax.map __pattern_syntax__004_
                              ~f:(function | true -> 0 | false -> 1))
                  [@ocaml.warning "-26-27"]) ~branches:2
                  ~with_:(function
                          | 0 -> BODY_1
                          | 1 -> BODY_2
                          | _ -> assert false))
              [@merlin.hide ])) |}]
;;

let%expect_test "very simple match%sub" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      match MY_EXPR_1 with
      | a -> BODY_1]
  |> print_expr;
  [%expect
    {|
   let () =
     Let_syntax.sub
       ~here:{
               Ppx_here_lib.pos_fname = "_none_";
               pos_lnum = 1;
               pos_cnum = (-1);
               pos_bol = 0
             } (Let_syntax.return MY_EXPR_1) ~f:(fun a -> BODY_1) |}]
;;

let%expect_test "destructuring let%sub" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      let a, { b; c } = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
   let () =
     Let_syntax.sub
       ~here:{
               Ppx_here_lib.pos_fname = "_none_";
               pos_lnum = 1;
               pos_cnum = (-1);
               pos_bol = 0
             } MY_EXPR
       ~f:(fun __pattern_syntax__006_ ->
             Let_syntax.sub
               ~here:{
                       Ppx_here_lib.pos_fname = "_none_";
                       pos_lnum = 1;
                       pos_cnum = (-1);
                       pos_bol = 0
                     }
               (Let_syntax.return
                  ((Let_syntax.map __pattern_syntax__006_
                      ~f:(function
                          | (_, { b = _; c = __pattern_syntax__009_ }) ->
                              __pattern_syntax__009_))[@merlin.hide ]))
               ~f:(fun c ->
                     Let_syntax.sub
                       ~here:{
                               Ppx_here_lib.pos_fname = "_none_";
                               pos_lnum = 1;
                               pos_cnum = (-1);
                               pos_bol = 0
                             }
                       (Let_syntax.return
                          ((Let_syntax.map __pattern_syntax__006_
                              ~f:(function
                                  | (_, { b = __pattern_syntax__008_; c = _ })
                                      -> __pattern_syntax__008_))
                          [@merlin.hide ]))
                       ~f:(fun b ->
                             Let_syntax.sub
                               ~here:{
                                       Ppx_here_lib.pos_fname = "_none_";
                                       pos_lnum = 1;
                                       pos_cnum = (-1);
                                       pos_bol = 0
                                     }
                               (Let_syntax.return
                                  ((Let_syntax.map __pattern_syntax__006_
                                      ~f:(function
                                          | (__pattern_syntax__007_,
                                             { b = _; c = _ }) ->
                                              __pattern_syntax__007_))
                                  [@merlin.hide ])) ~f:(fun a -> MY_BODY))))
   |}]
;;

let%expect_test "destructuring match%sub" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      match MY_EXPR with
      | Choice_1 (a, b) -> CHOICE_1_BODY
      | Choice_2 _ -> CHOICE_2_BODY
      | Choice_3 -> CHOICE_3_BODY]
  |> print_expr;
  [%expect
    {|
   let () =
     Let_syntax.sub
       ~here:{
               Ppx_here_lib.pos_fname = "_none_";
               pos_lnum = 1;
               pos_cnum = (-1);
               pos_bol = 0
             } (Let_syntax.return MY_EXPR)
       ~f:(fun __pattern_syntax__010_ ->
             ((Let_syntax.switch
                 ~match_:((Let_syntax.map __pattern_syntax__010_
                             ~f:(function
                                 | Choice_1 (a, b) -> 0
                                 | Choice_2 _ -> 1
                                 | Choice_3 -> 2))[@ocaml.warning "-26-27"])
                 ~branches:3
                 ~with_:(function
                         | 0 ->
                             Let_syntax.sub
                               ~here:{
                                       Ppx_here_lib.pos_fname = "_none_";
                                       pos_lnum = 1;
                                       pos_cnum = (-1);
                                       pos_bol = 0
                                     }
                               (Let_syntax.return
                                  ((Let_syntax.map __pattern_syntax__010_
                                      ~f:((function
                                           | Choice_1
                                               (_, __pattern_syntax__012_) ->
                                               __pattern_syntax__012_
                                           | _ -> assert false)
                                      [@ocaml.warning "-11"]))[@merlin.hide ]))
                               ~f:(fun b ->
                                     Let_syntax.sub
                                       ~here:{
                                               Ppx_here_lib.pos_fname = "_none_";
                                               pos_lnum = 1;
                                               pos_cnum = (-1);
                                               pos_bol = 0
                                             }
                                       (Let_syntax.return
                                          ((Let_syntax.map
                                              __pattern_syntax__010_
                                              ~f:((function
                                                   | Choice_1
                                                       (__pattern_syntax__011_,
                                                        _)
                                                       -> __pattern_syntax__011_
                                                   | _ -> assert false)
                                              [@ocaml.warning "-11"]))
                                          [@merlin.hide ]))
                                       ~f:(fun a -> CHOICE_1_BODY))
                         | 1 -> CHOICE_2_BODY
                         | 2 -> CHOICE_3_BODY
                         | _ -> assert false))
             [@merlin.hide ]))
   |}]
;;

let%expect_test "single-case match%sub doesn't call switch" =
  let modul = Some { txt = lident "Module"; loc } in
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul
    [%expr
      match MY_EXPR with
      | Choice_1 x -> CHOICE_1_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Module.Let_syntax.Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } (Module.Let_syntax.Let_syntax.return MY_EXPR)
        ~f:(fun __pattern_syntax__013_ ->
              Module.Let_syntax.Let_syntax.sub
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 1;
                        pos_cnum = (-1);
                        pos_bol = 0
                      }
                (Module.Let_syntax.Let_syntax.return
                   ((Module.Let_syntax.Let_syntax.map __pattern_syntax__013_
                       ~f:(function
                           | Choice_1 __pattern_syntax__014_ ->
                               __pattern_syntax__014_))[@merlin.hide ]))
                ~f:(fun x -> CHOICE_1_BODY)) |}]
;;

let%expect_test "module-qualified match%sub" =
  let modul = Some { txt = lident "Module"; loc } in
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul
    [%expr
      match MY_EXPR with
      | Choice_1 x -> CHOICE_1_BODY
      | Choice_2 x -> CHOICE_2_BODY]
  |> print_expr;
  [%expect
    {|
   let () =
     Module.Let_syntax.Let_syntax.sub
       ~here:{
               Ppx_here_lib.pos_fname = "_none_";
               pos_lnum = 1;
               pos_cnum = (-1);
               pos_bol = 0
             } (Module.Let_syntax.Let_syntax.return MY_EXPR)
       ~f:(fun __pattern_syntax__015_ ->
             ((Module.Let_syntax.Let_syntax.switch
                 ~match_:((Module.Let_syntax.Let_syntax.map
                             __pattern_syntax__015_
                             ~f:(function | Choice_1 x -> 0 | Choice_2 x -> 1))
                 [@ocaml.warning "-26-27"]) ~branches:2
                 ~with_:(function
                         | 0 ->
                             Module.Let_syntax.Let_syntax.sub
                               ~here:{
                                       Ppx_here_lib.pos_fname = "_none_";
                                       pos_lnum = 1;
                                       pos_cnum = (-1);
                                       pos_bol = 0
                                     }
                               (Module.Let_syntax.Let_syntax.return
                                  ((Module.Let_syntax.Let_syntax.map
                                      __pattern_syntax__015_
                                      ~f:((function
                                           | Choice_1 __pattern_syntax__016_ ->
                                               __pattern_syntax__016_
                                           | _ -> assert false)
                                      [@ocaml.warning "-11"]))[@merlin.hide ]))
                               ~f:(fun x -> CHOICE_1_BODY)
                         | 1 ->
                             Module.Let_syntax.Let_syntax.sub
                               ~here:{
                                       Ppx_here_lib.pos_fname = "_none_";
                                       pos_lnum = 1;
                                       pos_cnum = (-1);
                                       pos_bol = 0
                                     }
                               (Module.Let_syntax.Let_syntax.return
                                  ((Module.Let_syntax.Let_syntax.map
                                      __pattern_syntax__015_
                                      ~f:((function
                                           | Choice_2 __pattern_syntax__017_ ->
                                               __pattern_syntax__017_
                                           | _ -> assert false)
                                      [@ocaml.warning "-11"]))[@merlin.hide ]))
                               ~f:(fun x -> CHOICE_2_BODY)
                         | _ -> assert false))
             [@merlin.hide ]))
   |}]
;;

let%expect_test "type annotations are preserved" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      let (_ : int) = EXPR in
      BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.sub
        ~here:{
                Ppx_here_lib.pos_fname = "_none_";
                pos_lnum = 1;
                pos_cnum = (-1);
                pos_bol = 0
              } EXPR
        ~f:(fun __pattern_syntax__018_ ->
              Let_syntax.sub
                ~here:{
                        Ppx_here_lib.pos_fname = "_none_";
                        pos_lnum = 1;
                        pos_cnum = (-1);
                        pos_bol = 0
                      }
                (Let_syntax.return
                   (Let_syntax.map __pattern_syntax__018_
                      ~f:(function | (_ : int) -> ()))) ~f:(fun _ -> BODY)) |}]
;;

let%expect_test "function%sub" =
  Ppx_let_expander.expand
    Ppx_let_expander.sub
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      function
      | Some a -> EXPR_SOME
      | None -> EXPR_NONE]
  |> print_expr;
  [%expect
    {|
    let () =
      fun __let_syntax__019_ ->
        Let_syntax.sub
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } (Let_syntax.return __let_syntax__019_)
          ~f:(fun __pattern_syntax__020_ ->
                ((Let_syntax.switch
                    ~match_:((Let_syntax.map __pattern_syntax__020_
                                ~f:(function | Some a -> 0 | None -> 1))
                    [@ocaml.warning "-26-27"]) ~branches:2
                    ~with_:(function
                            | 0 ->
                                Let_syntax.sub
                                  ~here:{
                                          Ppx_here_lib.pos_fname = "_none_";
                                          pos_lnum = 1;
                                          pos_cnum = (-1);
                                          pos_bol = 0
                                        }
                                  (Let_syntax.return
                                     ((Let_syntax.map __pattern_syntax__020_
                                         ~f:((function
                                              | Some __pattern_syntax__021_ ->
                                                  __pattern_syntax__021_
                                              | _ -> assert false)
                                         [@ocaml.warning "-11"]))[@merlin.hide ]))
                                  ~f:(fun a -> EXPR_SOME)
                            | 1 -> EXPR_NONE
                            | _ -> assert false))
                [@merlin.hide ])) |}]
;;

let%expect_test "function%arr" =
  Ppx_let_expander.expand
    Ppx_let_expander.arr
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      function
      | Some a -> EXPR_SOME
      | None -> EXPR_NONE]
  |> print_expr;
  [%expect
    {|
    let () =
      fun __let_syntax__022_ ->
        Let_syntax.arr
          ~here:{
                  Ppx_here_lib.pos_fname = "_none_";
                  pos_lnum = 1;
                  pos_cnum = (-1);
                  pos_bol = 0
                } __let_syntax__022_
          ~f:(function | Some a -> EXPR_SOME | None -> EXPR_NONE) |}]
;;
