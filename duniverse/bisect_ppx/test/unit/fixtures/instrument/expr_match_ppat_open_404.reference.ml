module Bisect_visit___expr_match_ppat_open_404___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000j\000\000\000\022\000\000\000U\000\000\000U\b\000\000T\000\160\000WT\160\000_R\160\000jS\160\000}O\160\001\000\131P\160\001\000\155Q\160\001\000\163M\160\001\000\176N\160\001\000\192I\160\001\000\196J\160\001\000\202K\160\001\000\246L\160\001\000\254G\160\001\001\rH\160\001\001\"D\160\001\001(E\160\001\001VF\160\001\001^B\160\001\001tC\160\001\001\139@\160\001\001\143A" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_match_ppat_open_404.ml"
          ~point_count:21 ~point_definitions in
      cb
  end
open Bisect_visit___expr_match_ppat_open_404___ml
module M = struct type t =
                    | Foo 
                    | Bar 
                  type r = {
                    i: int ;
                    t: t } end
let f () =
  ___bisect_visit___ 20;
  (let s = ___bisect_visit___ 18; M.Bar in
   ___bisect_visit___ 19;
   (match s with
    | M.((Foo|Bar))  as ___bisect_matched_value___ ->
        ((((match ___bisect_matched_value___ with
            | M.(Foo)  -> (___bisect_visit___ 15; ())
            | M.(Bar)  -> (___bisect_visit___ 16; ())
            | _ -> ()))
         [@ocaml.warning "-4-8-9-11-26-27-28"]);
         assert true));
   ___bisect_visit___ 17;
   (let l = ___bisect_visit___ 13; (let open M in [Foo]) in
    ___bisect_visit___ 14;
    (match l with
     | M.((Foo|Bar)::[])  as ___bisect_matched_value___ ->
         ((((match ___bisect_matched_value___ with
             | M.((Foo)::[])  ->
                 (___bisect_visit___ 10; ___bisect_visit___ 9; ())
             | M.((Bar)::[])  ->
                 (___bisect_visit___ 11; ___bisect_visit___ 9; ())
             | _ -> ()))
          [@ocaml.warning "-4-8-9-11-26-27-28"]);
          assert true)
     | _ -> assert false);
    ___bisect_visit___ 12;
    (let a = ___bisect_visit___ 7; (let open M in [|Bar|]) in
     ___bisect_visit___ 8;
     (match a with
      | M.[|(Foo|Bar)|]  as ___bisect_matched_value___ ->
          ((((match ___bisect_matched_value___ with
              | M.[|Foo|]  -> (___bisect_visit___ 4; ())
              | M.[|Bar|]  -> (___bisect_visit___ 5; ())
              | _ -> ()))
           [@ocaml.warning "-4-8-9-11-26-27-28"]);
           assert true)
      | _ -> assert false);
     ___bisect_visit___ 6;
     (let r = ___bisect_visit___ 2; (let open M in { i = 3; t = Foo }) in
      ___bisect_visit___ 3;
      (match r with
       | M.{ i = (3|4);_}  as ___bisect_matched_value___ ->
           ((((match ___bisect_matched_value___ with
               | M.{ i = 3;_}  -> (___bisect_visit___ 0; ())
               | M.{ i = 4;_}  -> (___bisect_visit___ 1; ())
               | _ -> ()))
            [@ocaml.warning "-4-8-9-11-26-27-28"]);
            assert true)
       | _ -> assert false)))))
