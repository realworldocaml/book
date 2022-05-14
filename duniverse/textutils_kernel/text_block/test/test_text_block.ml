open! Core
open! Async
open! Import
open Text_block
module Expect_test_config = Core.Expect_test_config

let yoyoma : t list = [ text "yo"; text "yo"; text "ma" ]

let test t =
  invariant t;
  print_endline (render t)
;;

let example =
  let return_address =
    vcat
      [ text "Kel Varnsen"
      ; text "Vandelay Industries"
      ; text "67 Lantern Dr."
      ; text "Brooklyn, NY 11224"
      ; vsep
      ; text "August 3, 1998"
      ]
  in
  let salutation =
    vcat
      [ text "Sincerely,"
      ; vstrut 4
      ; text "Kel Varnsen"
      ; text "Chief Procurement Officer"
      ]
  in
  let [ return_address; salutation ] =
    With_static_lengths.halign `Left [ return_address; salutation ]
  in
  vcat
    ~align:`Right
    [ return_address
    ; vstrut 4
    ; vcat
        [ text "H.E. Pennypacker"
        ; text "Kramerica Industries"
        ; text "129 W 81st St, Apt 5B"
        ; text "Manhattan, NY 10024"
        ; vsep
        ; text "Dear Mr. Pennypacker:"
        ; vsep
        ; text
            "It has come to my attention that your revolutionary oil tanker\n\
             bladder system makes extensive use of latex and latex products."
        ; vsep
        ; text
            "We at Vandelay Industries are happy to supply you these materials\n\
             at a discounted rate. If you would like to pursue this matter,\n\
             please contact our head of sales, George Costanza at 555-6893."
        ]
    ; vsep
    ; salutation
    ]
;;

let%expect_test "example" =
  print_endline (render example);
  [%expect
    {|
                                            Kel Varnsen
                                            Vandelay Industries
                                            67 Lantern Dr.
                                            Brooklyn, NY 11224

                                            August 3, 1998




    H.E. Pennypacker
    Kramerica Industries
    129 W 81st St, Apt 5B
    Manhattan, NY 10024

    Dear Mr. Pennypacker:

    It has come to my attention that your revolutionary oil tanker
    bladder system makes extensive use of latex and latex products.

    We at Vandelay Industries are happy to supply you these materials
    at a discounted rate. If you would like to pursue this matter,
    please contact our head of sales, George Costanza at 555-6893.

                                            Sincerely,




                                            Kel Varnsen
                                            Chief Procurement Officer
  |}]
;;

let%expect_test _ =
  test (hcat yoyoma);
  [%expect {|
    yoyoma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 1) yoyoma);
  [%expect {|
    yo yo ma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 2) yoyoma);
  [%expect {|
    yo  yo  ma
  |}]
;;

let%expect_test _ =
  test (vcat yoyoma);
  [%expect {|
    yo
    yo
    ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 1) yoyoma);
  [%expect {|
    yo

    yo

    ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 2) yoyoma);
  [%expect {|
    yo


    yo


    ma
  |}]
;;

let sep = text "."

let%expect_test _ =
  test (hcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect {|
    yo.yoyoma
    yo
    ma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect {|
    yoyoma.yo
           yo
           ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect {|
    yo
    yo
    ma
    .
    yoyoma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect {|
    yoyoma
    .
    yo
    yo
    ma
  |}]
;;

let%expect_test "word wrap" =
  let test ~width str =
    let t = text ~max_width:width str in
    let vline = fill '|' ~width:1 ~height:(height t) in
    let hline = hcat [ text "+"; fill '-' ~width ~height:1; text "+" ] in
    test (vcat [ hline; hcat [ vline; vcat [ hstrut width; t ]; vline ]; hline ])
  in
  test
    ~width:30
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \
     deserunt mollit anim id est laborum.";
  [%expect
    {|
    +------------------------------+
    |Lorem ipsum dolor sit amet,   |
    |consectetur adipiscing elit,  |
    |sed do eiusmod tempor         |
    |incididunt ut labore et dolore|
    |magna aliqua. Ut enim ad minim|
    |veniam, quis nostrud          |
    |exercitation ullamco laboris  |
    |nisi ut aliquip ex ea commodo |
    |consequat. Duis aute irure    |
    |dolor in reprehenderit in     |
    |voluptate velit esse cillum   |
    |dolore eu fugiat nulla        |
    |pariatur. Excepteur sint      |
    |occaecat cupidatat non        |
    |proident, sunt in culpa qui   |
    |officia deserunt mollit anim  |
    |id est laborum.               |
    +------------------------------+
  |}];
  test
    ~width:33
    "(Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \
     deserunt mollit anim id est laborum.)";
  [%expect
    {|
    +---------------------------------+
    |(Lorem ipsum dolor sit amet,     |
    |consectetur adipiscing elit, sed |
    |do eiusmod tempor incididunt ut  |
    |labore et dolore magna aliqua. Ut|
    |enim ad minim veniam, quis       |
    |nostrud exercitation ullamco     |
    |laboris nisi ut aliquip ex ea    |
    |commodo consequat. Duis aute     |
    |irure dolor in reprehenderit in  |
    |voluptate velit esse cillum      |
    |dolore eu fugiat nulla pariatur. |
    |Excepteur sint occaecat cupidatat|
    |non proident, sunt in culpa qui  |
    |officia deserunt mollit anim id  |
    |est laborum.)                    |
    +---------------------------------+
  |}];
  test
    ~width:70
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua.\n\n\
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip \
     ex ea commodo consequat.";
  [%expect
    {|
    +----------------------------------------------------------------------+
    |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do       |
    |eiusmod tempor incididunt ut labore et dolore magna aliqua.           |
    |                                                                      |
    |Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris    |
    |nisi ut aliquip ex ea commodo consequat.                              |
    +----------------------------------------------------------------------+
  |}]
;;

(* lines with trailing whitespace used to tickle a bug *)

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text " " ]; hcat [ text "b" ] ]);
  [%expect {|
    a
    b
  |}]
;;

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text "    " ]; hcat [ text "b" ] ]);
  [%expect {|
    a
    b
  |}]
;;

let yellow = ansi_escape ~prefix:"[33m" ~suffix:"[39m"

let%expect_test _ =
  test (yellow (vcat yoyoma));
  [%expect {|
    [33myo[39m
    [33myo[39m
    [33mma[39m
  |}]
;;

let%expect_test "unicode" =
  test
    (let contents = vcat [ text "✓ yes"; text "x no" ] in
     let height = height contents in
     hcat
       [ fill '|' ~height ~width:1
       ; space ~height ~width:1
       ; contents
       ; space ~height ~width:1
       ; fill '|' ~height ~width:1
       ]);
  [%expect {|
    | ✓ yes |
    | x no  | |}]
;;

let%expect_test "fill_uchar" =
  test (fill_uchar (Uchar.of_scalar_exn 0x1f600) ~width:2 ~height:4);
  [%expect {|
    😀😀
    😀😀
    😀😀
    😀😀 |}]
;;

module _ = struct
  let dump x = boxed x |> render |> print_string
  let a = text "A"
  let b = text "B"
  let c = text "C"
  let d = text "D"
  let e = text "E"

  let%expect_test "Basics" =
    dump Boxed.(hcat [ cell a; cell b ]);
    [%expect {|
    ┌───┬───┐
    │ A │ B │
    └───┴───┘
  |}];
    dump Boxed.(hcat ~align:`Center [ vcat [ cell a; cell b ]; cell c ]);
    dump
      Boxed.(
        hcat ~align:`Center [ vcat [ cell a; cell b ]; cell c; vcat [ cell d; cell e ] ]);
    dump Boxed.(hcat ~align:`Center [ vcat [ cell a; cell b ]; vcat [ cell d; cell e ] ]);
    [%expect
      {|
    ┌───┬───┐
    │ A │   │
    ├───┤ C │
    │ B │   │
    └───┴───┘
    ┌───┬───┬───┐
    │ A │   │ D │
    ├───┤ C ├───┤
    │ B │   │ E │
    └───┴───┴───┘
    ┌───┬───┐
    │ A │ D │
    ├───┼───┤
    │ B │ E │
    └───┴───┘
  |}];
    dump Boxed.(vcat ~align:`Center [ hcat [ cell a; cell b ]; cell c ]);
    dump
      Boxed.(
        vcat ~align:`Center [ hcat [ cell a; cell b ]; cell c; hcat [ cell d; cell e ] ]);
    dump Boxed.(vcat ~align:`Center [ hcat [ cell a; cell b ]; hcat [ cell d; cell e ] ]);
    [%expect
      {|
    ┌───┬───┐
    │ A │ B │
    ├───┴───┤
    │   C   │
    └───────┘
    ┌───┬───┐
    │ A │ B │
    ├───┴───┤
    │   C   │
    ├───┬───┤
    │ D │ E │
    └───┴───┘
    ┌───┬───┐
    │ A │ B │
    ├───┼───┤
    │ D │ E │
    └───┴───┘
  |}]
  ;;

  let%expect_test "frills are correctly offset for padding" =
    dump
      Boxed.(
        hcat
          ~align:`Center
          [ vcat
              ~align:`Center
              [ hcat [ cell a; cell b ]; cell c; hcat [ cell d; cell e ] ]
          ; vcat ~align:`Center [ cell (text "Top right"); cell (text "Bottom right") ]
          ]);
    [%expect
      {|
    ┌───┬───┬──────────────┐
    │ A │ B │              │
    ├───┴───┤  Top right   │
    │   C   ├──────────────┤
    ├───┬───┤ Bottom right │
    │ D │ E │              │
    └───┴───┴──────────────┘ |}]
  ;;

  let%expect_test "align" =
    let addr1 = vcat [ text "2½ Devonshire Square"; text "London"; text "EC2M 4UJ" ] in
    let addr2 = vcat [ text "Windsor Castle"; text "Windsor"; text "SL4 1NJ" ] in
    let addr3 =
      vcat
        [ text "The White House"
        ; text "1600 Pennsylvania Av NW"
        ; text "Washington, DC"
        ; text "20500"
        ]
    in
    let test ~dir =
      vcat
        Boxed.(
          let cat1, cat2, cat3 =
            match dir with
            | `Horizontal -> hcat ~align:`Top, hcat ~align:`Center, hcat ~align:`Bottom
            | `Vertical -> vcat ~align:`Left, vcat ~align:`Center, vcat ~align:`Right
          in
          [ boxed (cat1 [ cell (text "A Streeter"); cell addr1 ])
          ; boxed (cat2 [ cell (text "Henry VIII"); cell addr2 ])
          ; boxed (cat3 [ cell (text "A Lincoln"); cell addr3 ])
          ])
      |> render
      |> print_string
    in
    test ~dir:`Horizontal;
    [%expect
      {|
    ┌────────────┬──────────────────────┐
    │ A Streeter │ 2½ Devonshire Square │
    │            │ London               │
    │            │ EC2M 4UJ             │
    └────────────┴──────────────────────┘
    ┌────────────┬────────────────┐
    │            │ Windsor Castle │
    │ Henry VIII │ Windsor        │
    │            │ SL4 1NJ        │
    └────────────┴────────────────┘
    ┌───────────┬─────────────────────────┐
    │           │ The White House         │
    │           │ 1600 Pennsylvania Av NW │
    │           │ Washington, DC          │
    │ A Lincoln │ 20500                   │
    └───────────┴─────────────────────────┘ |}];
    test ~dir:`Vertical;
    [%expect
      {|
      ┌──────────────────────┐
      │ A Streeter           │
      ├──────────────────────┤
      │ 2½ Devonshire Square │
      │ London               │
      │ EC2M 4UJ             │
      └──────────────────────┘
      ┌────────────────┐
      │   Henry VIII   │
      ├────────────────┤
      │ Windsor Castle │
      │ Windsor        │
      │ SL4 1NJ        │
      └────────────────┘
      ┌─────────────────────────┐
      │               A Lincoln │
      ├─────────────────────────┤
      │ The White House         │
      │ 1600 Pennsylvania Av NW │
      │ Washington, DC          │
      │ 20500                   │
      └─────────────────────────┘ |}]
  ;;

  let%expect_test "fib" =
    let square n =
      fill ' ' ~width:((4 * n) - 1) ~height:((2 * n) - 1) |> Boxed.cell ~hpadding:0
    in
    let rec nested_boxes ?(horizontal = true) = function
      | [] -> square 1
      | hd :: tl ->
        let cat =
          if horizontal
          then fun a b -> Boxed.hcat [ a; b ]
          else fun a b -> Boxed.vcat [ a; b ]
        in
        cat (square hd) (nested_boxes ~horizontal:(not horizontal) tl)
    in
    dump (nested_boxes [ 2; 1 ]);
    [%expect
      {|
    ┌───────┬───┐
    │       │   │
    │       ├───┤
    │       │   │
    └───────┴───┘
  |}];
    dump (nested_boxes [ 5; 3; 2; 1 ]);
    [%expect
      {|
    ┌───────────────────┬───────────┐
    │                   │           │
    │                   │           │
    │                   │           │
    │                   │           │
    │                   │           │
    │                   ├───────┬───┤
    │                   │       │   │
    │                   │       ├───┤
    │                   │       │   │
    └───────────────────┴───────┴───┘
  |}];
    dump (nested_boxes [ 8; 5; 3; 2; 1 ]);
    [%expect
      {|
    ┌───────────────────────────────┬───────────────────┐
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               │                   │
    │                               ├───────────┬───────┤
    │                               │           │       │
    │                               │           │       │
    │                               │           │       │
    │                               │           ├───┬───┤
    │                               │           │   │   │
    └───────────────────────────────┴───────────┴───┴───┘
  |}]
  ;;

  let%expect_test "padding" =
    vcat
      [ hcat
          (List.map [ `Left; `Center; `Right ] ~f:(fun align ->
             boxed
               Boxed.(
                 vcat
                   ~align
                   [ cell (sexp [%sexp_of: [ `Left | `Right | `Center ]] align)
                   ; vcat [ cell (text "A"); cell (text "B") ]
                   ])))
      ; hcat
          (List.map [ `Top; `Center; `Bottom ] ~f:(fun align ->
             boxed
               Boxed.(
                 hcat
                   ~align
                   [ cell
                       ~vpadding:1
                       (sexp [%sexp_of: [ `Top | `Bottom | `Center ]] align)
                   ; hcat [ cell (text "A"); cell (text "B") ]
                   ])))
      ]
    |> render
    |> print_string;
    [%expect
      {|
      ┌──────┐┌────────┐┌───────┐
      │ Left ││ Center ││ Right │
      ├──────┤├────────┤├───────┤
      │ A    ││   A    ││     A │
      ├──────┤├────────┤├───────┤
      │ B    ││   B    ││     B │
      └──────┘└────────┘└───────┘
      ┌─────┬───┬───┐┌────────┬───┬───┐┌────────┬───┬───┐
      │     │ A │ B ││        │   │   ││        │   │   │
      │ Top │   │   ││ Center │ A │ B ││ Bottom │   │   │
      │     │   │   ││        │   │   ││        │ A │ B │
      └─────┴───┴───┘└────────┴───┴───┘└────────┴───┴───┘ |}]
  ;;
end
