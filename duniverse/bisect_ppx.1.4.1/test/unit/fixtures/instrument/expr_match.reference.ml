module Bisect_visit___expr_match___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\0023\000\000\000j\000\000\001\165\000\000\001\165\b\000\001\164\000\160LC\160]@\160zA\160\000WB\160\000yG\160\001\000\134D\160\001\000\163E\160\001\000\192F\160\001\000\230N\160\001\000\247K\160\001\001\016H\160\001\001%L\160\001\001>I\160\001\001SM\160\001\001lJ\160\001\001\134U\160\001\001\147R\160\001\001\172O\160\001\001\193S\160\001\001\218P\160\001\001\239T\160\001\002\bQ\160\001\002@Z\160\001\002QX\160\001\002lV\160\001\002\129Y\160\001\002\156W\160\001\002\182_\160\001\002\195]\160\001\002\222[\160\001\002\243^\160\001\003\014\\\160\001\003,d\160\001\003:`\160\001\003Ka\160\001\003bb\160\001\003rc\160\001\003\144i\160\001\003\161g\160\001\003\192h\160\001\003\222e\160\001\004\002f\160\001\004-l\160\001\004>j\160\001\004Dk\160\001\004lo\160\001\004}m\160\001\004\136n\160\001\004\181t\160\001\004\199p\160\001\004\205s\160\001\004\212q\160\001\004\218r\160\001\005\003w\160\001\005\020u\160\001\0058v\160\001\005^z\160\001\005ox\160\001\005\141y\160\001\005\202|\160\001\005\219{\160\001\006\027~\160\001\006,}\160\001\006_\000A\160\001\006p\127\160\001\006v\000@\160\001\006\145\000F\160\001\006\162\000B\160\001\006\163\000C\160\001\006\169\000D\160\001\006\204\000E\160\001\006\243\000J\160\001\007\004\000G\160\001\007(\000H\160\001\007.\000I\160\001\007p\000O\160\001\007\135\000K\160\001\007\141\000N\160\001\007\152\000L\160\001\007\158\000M\160\001\007\200\000W\160\001\007\225\000P\160\001\007\249\000Q\160\001\007\252\000R\160\001\b\002\000U\160\001\b\t\000S\160\001\b\015\000T\160\001\b4\000V\160\001\bW\000Z\160\001\bn\000X\160\001\bt\000Y\160\001\b\177\000^\160\001\b\209\000[\160\001\b\215\000\\\160\001\b\247\000]\160\001\t\029\000a\160\001\t.\000_\160\001\t;\000`\160\001\tX\000d\160\001\ti\000b\160\001\tr\000c\160\001\t\138\000h\160\001\t\151\000f\160\001\t\166\000g\160\001\t\213\000e" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_match.ml" ~point_count:105
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_match___ml
let f x =
  ___bisect_visit___ 3;
  (match x with
   | 0 -> (___bisect_visit___ 0; print_endline "abc")
   | 1 -> (___bisect_visit___ 1; print_endline "def")
   | _ -> (___bisect_visit___ 2; print_endline "ghi"))
let f =
  ___bisect_visit___ 7;
  (function
   | 0 -> (___bisect_visit___ 4; print_endline "abc")
   | 1 -> (___bisect_visit___ 5; print_endline "def")
   | _ -> (___bisect_visit___ 6; print_endline "ghi"))
let f x =
  ___bisect_visit___ 14;
  (match x with
   | 0 ->
       (___bisect_visit___ 11;
        print_string "abc";
        ___bisect_visit___ 8;
        print_newline ())
   | 1 ->
       (___bisect_visit___ 12;
        print_string "def";
        ___bisect_visit___ 9;
        print_newline ())
   | _ ->
       (___bisect_visit___ 13;
        print_string "ghi";
        ___bisect_visit___ 10;
        print_newline ()))
let f =
  ___bisect_visit___ 21;
  (function
   | 0 ->
       (___bisect_visit___ 18;
        print_string "abc";
        ___bisect_visit___ 15;
        print_newline ())
   | 1 ->
       (___bisect_visit___ 19;
        print_string "def";
        ___bisect_visit___ 16;
        print_newline ())
   | _ ->
       (___bisect_visit___ 20;
        print_string "ghi";
        ___bisect_visit___ 17;
        print_newline ()))
type t =
  | Foo 
  | Bar 
let f x =
  ___bisect_visit___ 26;
  (match x with
   | Foo ->
       (___bisect_visit___ 24;
        print_string "foo";
        ___bisect_visit___ 22;
        print_newline ())
   | Bar ->
       (___bisect_visit___ 25;
        print_string "bar";
        ___bisect_visit___ 23;
        print_newline ()))
let f =
  ___bisect_visit___ 31;
  (function
   | Foo ->
       (___bisect_visit___ 29;
        print_string "foo";
        ___bisect_visit___ 27;
        print_newline ())
   | Bar ->
       (___bisect_visit___ 30;
        print_string "bar";
        ___bisect_visit___ 28;
        print_newline ()))
let f x =
  ___bisect_visit___ 36;
  ((function
    | Foo -> (___bisect_visit___ 32; "foo")
    | Bar -> (___bisect_visit___ 33; "bar")) x) |>
    ((___bisect_visit___ 34; print_string));
  ___bisect_visit___ 35;
  print_newline ()
let f x =
  ___bisect_visit___ 41;
  (match x with
   | Foo -> (___bisect_visit___ 39; print_endline "foo")
   | Bar ->
       (___bisect_visit___ 40;
        (match x with
         | Foo -> (___bisect_visit___ 37; print_endline "foobar")
         | Bar -> (___bisect_visit___ 38; print_endline "barbar"))))
let f x =
  ___bisect_visit___ 44;
  (match x with
   | Foo|Bar as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | Foo -> (___bisect_visit___ 42; ())
           | Bar -> (___bisect_visit___ 43; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo"))
let f x =
  ___bisect_visit___ 47;
  (match x with
   | (Foo, _)|(Bar, _) as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | (Foo, _) -> (___bisect_visit___ 45; ())
           | (Bar, _) -> (___bisect_visit___ 46; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo"))
let f x =
  ___bisect_visit___ 52;
  (match x with
   | ((Foo|Bar), (Foo|Bar)) as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | (Foo, Foo) -> (___bisect_visit___ 49; ___bisect_visit___ 48; ())
           | (Foo, Bar) -> (___bisect_visit___ 50; ___bisect_visit___ 48; ())
           | (Bar, Foo) -> (___bisect_visit___ 49; ___bisect_visit___ 51; ())
           | (Bar, Bar) -> (___bisect_visit___ 50; ___bisect_visit___ 51; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo"))
let f x =
  ___bisect_visit___ 55;
  (match x with
   | 'a'..'z' -> (___bisect_visit___ 53; print_endline "foo")
   | _ -> (___bisect_visit___ 54; print_endline "bar"))
let f x =
  ___bisect_visit___ 58;
  (match x with
   | `A -> (___bisect_visit___ 56; print_endline "foo")
   | `B -> (___bisect_visit___ 57; print_endline "bar"))
type u = [ `A  | `B ]
let f x =
  ___bisect_visit___ 60;
  (match x with | #u -> (___bisect_visit___ 59; print_endline "foo"))
module type S  = sig  end
let f x =
  ___bisect_visit___ 62;
  (match x with
   | ((module X)  : (module S)) ->
       (___bisect_visit___ 61; print_endline "foo"))
let f x =
  ___bisect_visit___ 65;
  (match x with
   | Foo|Bar as y as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | Foo as y -> (___bisect_visit___ 63; ())
           | Bar as y -> (___bisect_visit___ 64; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        y))
let f x =
  ___bisect_visit___ 70;
  (match x with
   | (Foo|Bar)::_ as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | (Foo)::_ -> (___bisect_visit___ 67; ___bisect_visit___ 66; ())
           | (Bar)::_ -> (___bisect_visit___ 68; ___bisect_visit___ 66; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo")
   | [] -> (___bisect_visit___ 69; print_endline "bar"))
let f x =
  ___bisect_visit___ 74;
  (match x with
   | `A _ -> (___bisect_visit___ 71; print_endline "foo")
   | `B (Foo|Bar) as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | `B (Foo) -> (___bisect_visit___ 72; ())
           | `B (Bar) -> (___bisect_visit___ 73; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "bar"))
type v = {
  a: t ;
  b: t }
let f x =
  ___bisect_visit___ 79;
  (match x with
   | { a = (Foo|Bar); b = (Foo|Bar) } as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | { a = Foo; b = Foo } ->
               (___bisect_visit___ 76; ___bisect_visit___ 75; ())
           | { a = Foo; b = Bar } ->
               (___bisect_visit___ 77; ___bisect_visit___ 75; ())
           | { a = Bar; b = Foo } ->
               (___bisect_visit___ 76; ___bisect_visit___ 78; ())
           | { a = Bar; b = Bar } ->
               (___bisect_visit___ 77; ___bisect_visit___ 78; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo"))
let f x =
  ___bisect_visit___ 87;
  (match x with
   | [||] -> (___bisect_visit___ 80; print_endline "foo")
   | [|(Foo|Bar);(Foo|Bar);_|] as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | [|Foo;Foo;_|] ->
               (___bisect_visit___ 83;
                ___bisect_visit___ 82;
                ___bisect_visit___ 81;
                ())
           | [|Foo;Bar;_|] ->
               (___bisect_visit___ 84;
                ___bisect_visit___ 82;
                ___bisect_visit___ 81;
                ())
           | [|Bar;Foo;_|] ->
               (___bisect_visit___ 83;
                ___bisect_visit___ 85;
                ___bisect_visit___ 81;
                ())
           | [|Bar;Bar;_|] ->
               (___bisect_visit___ 84;
                ___bisect_visit___ 85;
                ___bisect_visit___ 81;
                ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "bar")
   | _ -> (___bisect_visit___ 86; print_newline ()))
let f x =
  ___bisect_visit___ 90;
  (match x with
   | (lazy (Foo|Bar)) as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | (lazy Foo) -> (___bisect_visit___ 88; ())
           | (lazy Bar) -> (___bisect_visit___ 89; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo"))
exception Exn of t 
let f x =
  ___bisect_visit___ 94;
  (match x with
   | exception (Exn (Foo|Bar) as ___bisect_matched_value___) ->
       ((((match ___bisect_matched_value___ with
           | Exn (Foo) -> (___bisect_visit___ 91; ())
           | Exn (Bar) -> (___bisect_visit___ 92; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        print_endline "foo")
   | _ -> (___bisect_visit___ 93; print_endline "bar"))
let f x =
  ___bisect_visit___ 97;
  (match x with
   | Foo as x|Bar as x as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | Foo as x -> (___bisect_visit___ 95; ())
           | Bar as x -> (___bisect_visit___ 96; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        x))
let f x =
  ___bisect_visit___ 100;
  (match x with
   | `Foo x|`Bar x as ___bisect_matched_value___ ->
       ((((match ___bisect_matched_value___ with
           | `Foo x -> (___bisect_visit___ 98; ())
           | `Bar x -> (___bisect_visit___ 99; ())
           | _ -> ()))
        [@ocaml.warning "-4-8-9-11-26-27-28"]);
        x))
let last =
  ___bisect_visit___ 104;
  (function
   | [] -> (___bisect_visit___ 102; None)
   | _::_ as li ->
       (___bisect_visit___ 103;
        (match List.rev li with
         | last::_ -> (___bisect_visit___ 101; Some last)
         | _ -> assert false)))
