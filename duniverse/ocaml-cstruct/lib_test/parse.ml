(* (c) 2016 Daniel C. Bünzli
   (c) 2020 Romain Calascibetta *)

open Cstruct

let cstruct =
  let pp ppf x = Format.fprintf ppf "%S" (Cstruct.to_string x) in
  let equal a b = String.equal (Cstruct.to_string a) (Cstruct.to_string b) in
  Alcotest.testable pp equal


module Alcotest = struct
  include Alcotest

  let string = Alcotest.testable (Fmt.fmt "%S") String.equal
end

let misc =
  Alcotest.test_case "misc" `Quick @@ fun () ->
  Alcotest.(check cstruct) "empty" empty (Cstruct.create 0) ;
  Alcotest.(check cstruct) "abc" (string "abc") (Cstruct.of_string "abc") ;
  Alcotest.(check cstruct) "abc" (string ~off:0 ~len:1 "abc") (Cstruct.of_string "a") ;
  Alcotest.(check cstruct) "abc" (string ~off:1 ~len:1 "abc") (Cstruct.of_string "b") ;
  Alcotest.(check cstruct) "abc" (string ~off:1 ~len:2 "abc") (Cstruct.of_string "bc") ;
  Alcotest.(check cstruct) "abc" (string ~off:2 ~len:1 "abc") (Cstruct.of_string "c") ;
  Alcotest.(check cstruct) "abc" (string ~off:3 ~len:0 "abc") (Cstruct.create 0) ;
  let sub = string ~off:2 ~len:1 "abc" in
  Alcotest.(check int) "start_pos" (start_pos sub) 2 ;
  Alcotest.(check int) "stop_pos"  (stop_pos sub)  3 ;
  Alcotest.(check int) "length"    (length sub)    1 ;
  let index_out_of_bounds = Invalid_argument "index out of bounds" in
  Alcotest.check_raises "get" index_out_of_bounds @@ fun () -> ignore @@ get sub 3 ;
  Alcotest.check_raises "get" index_out_of_bounds @@ fun () -> ignore @@ get sub 2 ;
  Alcotest.check_raises "get" index_out_of_bounds @@ fun () -> ignore @@ get sub 1 ;
  Alcotest.(check char) "get" (get sub 0) 'c' ;
  Alcotest.(check int)  "get_byte" (get_byte sub 0) 0x63 ;
;;

let head =
  Alcotest.test_case "head" `Quick @@ fun () ->
  let { Cstruct.buffer= v; _ } = Cstruct.of_string "abc" in
  let empty = buffer ~off:2 ~len:0 v in
  let bc = buffer ~off:1 ~len:2 v in
  Alcotest.(check (option char)) "empty" (head empty) None ;
  Alcotest.(check (option char)) "empty" (head ~rev:true empty) None ;
  Alcotest.(check (option char)) "bc" (head bc) (Some 'b') ;
  Alcotest.(check (option char)) "bc" (head ~rev:true bc) (Some 'c') ;
;;

let start =
  Alcotest.test_case "start" `Quick @@ fun () ->
  let empty_pos cs pos =
    Alcotest.(check int) "length" (length cs) 0 ;
    Alcotest.(check int) "start_pos" (start_pos cs) pos in
  let { Cstruct.buffer= abc; _ } = Cstruct.of_string "abc" in
  empty_pos (start @@ string "") 0 ;
  empty_pos (start @@ buffer ~off:0 ~len:0 abc) 0 ;
  empty_pos (start @@ buffer ~off:0 ~len:1 abc) 0 ;
  empty_pos (start @@ buffer ~off:0 ~len:2 abc) 0 ;
  empty_pos (start @@ buffer ~off:0 ~len:3 abc) 0 ;
  empty_pos (start @@ buffer ~off:1 ~len:0 abc) 1 ;
  empty_pos (start @@ buffer ~off:1 ~len:1 abc) 1 ;
  empty_pos (start @@ buffer ~off:1 ~len:2 abc) 1 ;
  empty_pos (start @@ buffer ~off:2 ~len:0 abc) 2 ;
  empty_pos (start @@ buffer ~off:2 ~len:1 abc) 2 ;
  empty_pos (start @@ buffer ~off:3 ~len:0 abc) 3 ;
;;

let stop =
  Alcotest.test_case "stop" `Quick @@ fun () ->
  let empty_pos cs pos =
    Alcotest.(check int) "length" (length cs) 0 ;
    Alcotest.(check int) "start_pos" (start_pos cs) pos in
  let { Cstruct.buffer= abc; _ } = Cstruct.of_string "abc" in
  empty_pos (stop @@ string "") 0 ;
  empty_pos (stop @@ buffer ~off:0 ~len:0 abc) 0 ;
  empty_pos (stop @@ buffer ~off:0 ~len:1 abc) 1 ;
  empty_pos (stop @@ buffer ~off:0 ~len:2 abc) 2 ;
  empty_pos (stop @@ buffer ~off:0 ~len:3 abc) 3 ;
  empty_pos (stop @@ buffer ~off:1 ~len:0 abc) 1 ;
  empty_pos (stop @@ buffer ~off:1 ~len:1 abc) 2 ;
  empty_pos (stop @@ buffer ~off:1 ~len:2 abc) 3 ;
  empty_pos (stop @@ buffer ~off:2 ~len:0 abc) 2 ;
  empty_pos (stop @@ buffer ~off:2 ~len:1 abc) 3 ;
  empty_pos (stop @@ buffer ~off:3 ~len:0 abc) 3 ;
;;

let tail =
  Alcotest.test_case "tail" `Quick @@ fun () ->
  let empty_pos cs pos =
    Alcotest.(check int) "length" (length cs) 0 ;
    Alcotest.(check int) "start_pos" (start_pos cs) pos in
  let { Cstruct.buffer= abc; _ } = Cstruct.of_string "abc" in
  empty_pos (tail @@ string "") 0 ;
  empty_pos (tail @@ buffer ~off:0 ~len:0 abc) 0 ;
  empty_pos (tail @@ buffer ~off:0 ~len:1 abc) 1 ;
  Alcotest.(check cstruct) "b" (tail @@ buffer ~off:0 ~len:2 abc) (string "b") ;
  Alcotest.(check cstruct) "bc" (tail @@ buffer ~off:0 ~len:3 abc) (string "bc") ;
  empty_pos (tail @@ buffer ~off:1 ~len:0 abc) 1 ;
  empty_pos (tail @@ buffer ~off:1 ~len:1 abc) 2 ;
  Alcotest.(check cstruct) "c" (tail @@ buffer ~off:1 ~len:2 abc) (string "c") ;
  empty_pos (tail @@ buffer ~off:2 ~len:0 abc) 2 ;
  empty_pos (tail @@ buffer ~off:2 ~len:1 abc) 3 ;
  empty_pos (tail @@ buffer ~off:3 ~len:0 abc) 3 ;
;;

let is_empty =
  Alcotest.test_case "is_empty" `Quick @@ fun () ->
  Alcotest.(check bool) "empty" (is_empty (string "")) true ;
  let { Cstruct.buffer= abcd; _ } = Cstruct.of_string "abcd" in
  let { Cstruct.buffer= huyi; _ } = Cstruct.of_string "huyi" in
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:4 ~len:0 abcd)) true ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:0 ~len:0 huyi)) true ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:0 ~len:1 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:0 ~len:2 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:0 ~len:3 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:0 ~len:4 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:1 ~len:0 abcd)) true ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:1 ~len:1 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:1 ~len:2 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:1 ~len:3 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:2 ~len:0 abcd)) true ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:2 ~len:1 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:2 ~len:2 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:3 ~len:0 abcd)) true ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:3 ~len:1 huyi)) false ;
  Alcotest.(check bool) "empty" (is_empty (buffer ~off:4 ~len:0 huyi)) true ;
;;

let is_prefix =
  Alcotest.test_case "is_prefix" `Quick @@ fun () ->
  let { Cstruct.buffer= ugoadfj; _ }      = Cstruct.of_string "ugoadf" in
  let { Cstruct.buffer= dfkdjf; _ }       = Cstruct.of_string "dfkdjf" in
  let { Cstruct.buffer= abhablablu; _ }   = Cstruct.of_string "abhablablu" in
  let { Cstruct.buffer= hadfdffdf; _ }    = Cstruct.of_string "hadfdffdf" in
  let { Cstruct.buffer= hadhabfdffdf; _ } = Cstruct.of_string "hadhabfdffdf" in
  let { Cstruct.buffer= iabla; _ }        = Cstruct.of_string "iabla" in
  let empty0 = buffer ~off:3 ~len:0 ugoadfj in
  let empty1 = buffer ~off:4 ~len:0 dfkdjf in
  let habla  = buffer ~off:2 ~len:5 abhablablu in
  let h      = buffer ~off:0 ~len:1 hadfdffdf in
  let ha     = buffer ~off:0 ~len:2 hadfdffdf in
  let hab    = buffer ~off:3 ~len:3 hadhabfdffdf in
  let abla   = buffer ~off:1 ~len:4 iabla in
  Alcotest.(check cstruct) "empty" empty0 (Cstruct.of_string "") ;
  Alcotest.(check cstruct) "empty" empty1 (Cstruct.of_string "") ;
  Alcotest.(check cstruct) "habla" habla  (Cstruct.of_string "habla") ;
  Alcotest.(check cstruct) "h"     h      (Cstruct.of_string "h") ;
  Alcotest.(check cstruct) "ha"    ha     (Cstruct.of_string "ha") ;
  Alcotest.(check cstruct) "hab"   hab    (Cstruct.of_string "hab") ;
  Alcotest.(check cstruct) "abla"  abla   (Cstruct.of_string "abla") ;
  Alcotest.(check bool) "is_prefix empty0 empty0" (is_prefix ~affix:empty0 empty1) true ;
  Alcotest.(check bool) "is_prefix empty0 habla"  (is_prefix ~affix:empty0 habla)  true ;
  Alcotest.(check bool) "is_prefix ha empty1"     (is_prefix ~affix:ha     empty1) false ;
  Alcotest.(check bool) "is_prefix ha h"          (is_prefix ~affix:ha     h)      false ;
  Alcotest.(check bool) "is_prefix ha ha"         (is_prefix ~affix:ha     ha)     true ;
  Alcotest.(check bool) "is_prefix ha hab"        (is_prefix ~affix:ha     hab)    true ;
  Alcotest.(check bool) "is_prefix ha habla"      (is_prefix ~affix:ha     habla)  true ;
  Alcotest.(check bool) "is_prefix ha abla"       (is_prefix ~affix:ha     abla)   false ;
;;

let is_infix =
  Alcotest.test_case "is_infix" `Quick @@ fun () ->
  let { Cstruct.buffer= ugoadfj; _ }      = Cstruct.of_string "ugoadfj" in
  let { Cstruct.buffer= dfkdjf; _ }       = Cstruct.of_string "dfkdjf" in
  let { Cstruct.buffer= aasdflablu; _ }   = Cstruct.of_string "aasdflablu" in
  let { Cstruct.buffer= cda; _ }          = Cstruct.of_string "cda" in
  let { Cstruct.buffer= h; _ }            = Cstruct.of_string "h" in
  let { Cstruct.buffer= uhadfdffdf; _ }   = Cstruct.of_string "uhadfdffdf" in
  let { Cstruct.buffer= ah; _ }           = Cstruct.of_string "ah" in
  let { Cstruct.buffer= aaaha; _ }        = Cstruct.of_string "aaaha" in
  let { Cstruct.buffer= ahaha; _ }        = Cstruct.of_string "ahaha" in
  let { Cstruct.buffer= hahbdfdf; _ }     = Cstruct.of_string "hahbdfdf" in
  let { Cstruct.buffer= blhahbdfdf; _ }   = Cstruct.of_string "blhahbdfdf" in
  let { Cstruct.buffer= fblhahbdfdfl; _ } = Cstruct.of_string "fblhahbdfdfl" in
  let empty0 = buffer ~off:1 ~len:0 ugoadfj in
  let empty1 = buffer ~off:2 ~len:0 dfkdjf in
  let asdf   = buffer ~off:1 ~len:4 aasdflablu in
  let a      = buffer ~off:2 ~len:1 cda in
  let h      = buffer ~off:0 ~len:1 h in
  let ha     = buffer ~off:1 ~len:2 uhadfdffdf in
  let ah     = buffer ~off:0 ~len:2 ah in
  let aha    = buffer ~off:2 ~len:3 aaaha in
  let haha   = buffer ~off:1 ~len:4 ahaha in
  let hahb   = buffer ~off:0 ~len:4 hahbdfdf in
  let blhahb = buffer ~off:0 ~len:6 blhahbdfdf in
  let blha   = buffer ~off:1 ~len:4 fblhahbdfdfl in
  let blh    = buffer ~off:1 ~len:3 fblhahbdfdfl in
  Alcotest.(check cstruct) "asdf"   asdf   (Cstruct.of_string "asdf") ;
  Alcotest.(check cstruct) "ha"     ha     (Cstruct.of_string "ha") ;
  Alcotest.(check cstruct) "h"      h      (Cstruct.of_string "h") ;
  Alcotest.(check cstruct) "a"      a      (Cstruct.of_string "a") ;
  Alcotest.(check cstruct) "aha"    aha    (Cstruct.of_string "aha") ;
  Alcotest.(check cstruct) "haha"   haha   (Cstruct.of_string "haha") ;
  Alcotest.(check cstruct) "hahb"   hahb   (Cstruct.of_string "hahb") ;
  Alcotest.(check cstruct) "blhahb" blhahb (Cstruct.of_string "blhahb") ;
  Alcotest.(check cstruct) "blha"   blha   (Cstruct.of_string "blha") ;
  Alcotest.(check cstruct) "blh"    blh    (Cstruct.of_string "blh") ;
  Alcotest.(check bool) "is_infix empty0 empty1" (is_infix ~affix:empty0 empty1) true ;
  Alcotest.(check bool) "is_infix empty0 asdf"   (is_infix ~affix:empty0 asdf)   true ;
  Alcotest.(check bool) "is_infix empty0 ha"     (is_infix ~affix:empty0 ha)     true ;
  Alcotest.(check bool) "is_infix ha empty1"     (is_infix ~affix:ha     empty1) false ;
  Alcotest.(check bool) "is_infix ha a"          (is_infix ~affix:ha     a)      false ;
  Alcotest.(check bool) "is_infix ha h"          (is_infix ~affix:ha     h)      false ;
  Alcotest.(check bool) "is_infix ha ah"         (is_infix ~affix:ha     ah)     false ;
  Alcotest.(check bool) "is_infix ha ha"         (is_infix ~affix:ha     ha)     true ;
  Alcotest.(check bool) "is_infix ha aha"        (is_infix ~affix:ha     aha)    true ;
  Alcotest.(check bool) "is_infix ha haha"       (is_infix ~affix:ha     haha)   true ;
  Alcotest.(check bool) "is_infix ha hahb"       (is_infix ~affix:ha     hahb)   true ;
  Alcotest.(check bool) "is_infix ha blhahb"     (is_infix ~affix:ha     blhahb) true ;
  Alcotest.(check bool) "is_infix ha blha"       (is_infix ~affix:ha     blha)   true ;
  Alcotest.(check bool) "is_infix ha blh"        (is_infix ~affix:ha     blh)    false ;
;;

let is_suffix =
  Alcotest.test_case "is_suffix" `Quick @@ fun () ->
  let { Cstruct.buffer= ugoadfj; _ }    = Cstruct.of_string "ugoadfj" in
  let { Cstruct.buffer= dfkdjf; _ }     = Cstruct.of_string "dfkdjf" in
  let { Cstruct.buffer= aasdflablu; _ } = Cstruct.of_string "aasdflablu" in
  let { Cstruct.buffer= cda; _ }        = Cstruct.of_string "cda" in
  let { Cstruct.buffer= h; _ }          = Cstruct.of_string "h" in
  let { Cstruct.buffer= uhadfdffdf; _ } = Cstruct.of_string "uhadfdffdf" in
  let { Cstruct.buffer= ah; _ }         = Cstruct.of_string "ah" in
  let { Cstruct.buffer= aaaha; _ }      = Cstruct.of_string "aaaha" in
  let { Cstruct.buffer= ahaha; _ }      = Cstruct.of_string "ahaha" in
  let { Cstruct.buffer= hahbdfdf; _ }   = Cstruct.of_string "hahbdfdf" in
  let empty0 = buffer ~off:1 ~len:0 ugoadfj in
  let empty1 = buffer ~off:2 ~len:0 dfkdjf in
  let asdf   = buffer ~off:1 ~len:4 aasdflablu in
  let a      = buffer ~off:2 ~len:1 cda in
  let h      = buffer ~off:0 ~len:1 h in
  let ha     = buffer ~off:1 ~len:2 uhadfdffdf in
  let ah     = buffer ~off:0 ~len:2 ah in
  let aha    = buffer ~off:2 ~len:3 aaaha in
  let haha   = buffer ~off:1 ~len:4 ahaha in
  let hahb   = buffer ~off:0 ~len:4 hahbdfdf in
  Alcotest.(check cstruct) "asdf"   asdf   (Cstruct.of_string "asdf") ;
  Alcotest.(check cstruct) "ha"     ha     (Cstruct.of_string "ha") ;
  Alcotest.(check cstruct) "h"      h      (Cstruct.of_string "h") ;
  Alcotest.(check cstruct) "a"      a      (Cstruct.of_string "a") ;
  Alcotest.(check cstruct) "aha"    aha    (Cstruct.of_string "aha") ;
  Alcotest.(check cstruct) "haha"   haha   (Cstruct.of_string "haha") ;
  Alcotest.(check cstruct) "hahb"   hahb   (Cstruct.of_string "hahb") ;
  Alcotest.(check bool) "is_suffix empty0 empty1" (is_suffix ~affix:empty0 empty1)     true ;
  Alcotest.(check bool) "is_suffix empty0 asdf"   (is_suffix ~affix:empty0 asdf)       true ;
  Alcotest.(check bool) "is_suffix ha empty1"     (is_suffix ~affix:ha     empty1)     false ;
  Alcotest.(check bool) "is_suffix ha a"          (is_suffix ~affix:ha     a)          false ;
  Alcotest.(check bool) "is_suffix ha h"          (is_suffix ~affix:ha     h)          false ;
  Alcotest.(check bool) "is_suffix ha ah"         (is_suffix ~affix:ha     ah)         false ;
  Alcotest.(check bool) "is_suffix ha ha"         (is_suffix ~affix:ha     ha)         true ;
  Alcotest.(check bool) "is_suffix ha aha"        (is_suffix ~affix:ha     aha)        true ;
  Alcotest.(check bool) "is_suffix ha haha"       (is_suffix ~affix:ha     haha)       true ;
  Alcotest.(check bool) "is_suffix ha hahb"       (is_suffix ~affix:ha     hahb)       false ;
;;

let () = Printexc.record_backtrace true

let for_all =
  Alcotest.test_case "for_all" `Quick @@ fun () ->
  let { Cstruct.buffer= asldfksaf; _ } = Cstruct.of_string "asldfksaf" in
  let { Cstruct.buffer= sf123df; _ } = Cstruct.of_string "sf123df" in
  let { Cstruct.buffer= _412; _ } = Cstruct.of_string "412" in
  let { Cstruct.buffer= aaa142; _ } = Cstruct.of_string "aaa142" in
  let { Cstruct.buffer= aad124; _ } = Cstruct.of_string "aad124" in
  let empty = buffer ~off:3 ~len:0 asldfksaf in
  let s123  = buffer ~off:2 ~len:3 sf123df in
  let s412  = buffer _412 in
  let s142  = buffer ~off:3 aaa142 in
  let s124  = buffer ~off:3 aad124 in
  Alcotest.(check cstruct) "empty" empty (Cstruct.of_string "") ;
  Alcotest.(check cstruct) "123"   s123 (Cstruct.of_string "123") ;
  Alcotest.(check cstruct) "412"   s412 (Cstruct.of_string "412") ;
  Alcotest.(check cstruct) "142"   s142 (Cstruct.of_string "142") ;
  Alcotest.(check cstruct) "124"   s124 (Cstruct.of_string "124") ;
  Alcotest.(check bool) "for_all" (for_all (fun _ -> false) empty) true ;
  Alcotest.(check bool) "for_all" (for_all (fun _ -> true)  empty) true ;
  Alcotest.(check bool) "for_all" (for_all (fun c -> Char.code c < 0x34) s123) true ;
  Alcotest.(check bool) "for_all" (for_all (fun c -> Char.code c < 0x34) s412) false ;
  Alcotest.(check bool) "for_all" (for_all (fun c -> Char.code c < 0x34) s142) false ;
  Alcotest.(check bool) "for_all" (for_all (fun c -> Char.code c < 0x34) s124) false ;
;;

let exists =
  Alcotest.test_case "exists" `Quick @@ fun () ->
  let { Cstruct.buffer= asldfksaf; _ } = Cstruct.of_string "asldfksaf" in
  let { Cstruct.buffer= a541; _ } = Cstruct.of_string "a541" in
  let { Cstruct.buffer= a154; _ } = Cstruct.of_string "a154" in
  let { Cstruct.buffer= _654adf; _ } = Cstruct.of_string "654adf" in
  let empty = buffer ~off:3 ~len:0 asldfksaf in
  let s541  = buffer ~off:1 ~len:3 a541 in
  let s154  = buffer ~off:1 a154 in
  let s654  = buffer ~len:3 _654adf in
  Alcotest.(check cstruct) "empty" empty (Cstruct.of_string "") ;
  Alcotest.(check cstruct) "541"   s541  (Cstruct.of_string "541") ;
  Alcotest.(check cstruct) "154"   s154  (Cstruct.of_string "154") ;
  Alcotest.(check cstruct) "654"   s654  (Cstruct.of_string "654") ;
  Alcotest.(check bool) "exists" (exists (fun _ -> false) empty) false ;
  Alcotest.(check bool) "exists" (exists (fun _ -> true)  empty) false ;
  Alcotest.(check bool) "exists" (exists (fun c -> Char.code c < 0x34) s541) true ;
  Alcotest.(check bool) "exists" (exists (fun c -> Char.code c < 0x34) s154) true ;
  Alcotest.(check bool) "exists" (exists (fun c -> Char.code c < 0x34) s654) false ;
;;

let trim =
  Alcotest.test_case "trim" `Quick @@ fun () ->
  let drop_a c = c = 'a' in
  let { Cstruct.buffer= base; _ } = Cstruct.of_string "00aaaabcdaaaa00" in
  let aaaabcdaaaa = buffer ~off:2 ~len:11 base in
  let aaaabcd = buffer ~off:2 ~len:7 base in
  let bcdaaaa = buffer ~off:6 ~len:7 base in
  let aaaa = buffer ~off:2 ~len:4 base in
  Alcotest.(check cstruct) "trim" (trim (string "\t abcd \t")) (Cstruct.of_string "abcd") ;
  Alcotest.(check cstruct) "trim" (trim aaaabcdaaaa) (Cstruct.of_string "aaaabcdaaaa") ;
  Alcotest.(check cstruct) "trim" (trim ~drop:drop_a aaaabcdaaaa) (Cstruct.of_string "bcd") ;
  Alcotest.(check cstruct) "trim" (trim ~drop:drop_a aaaabcd) (Cstruct.of_string "bcd") ;
  Alcotest.(check cstruct) "trim" (trim ~drop:drop_a bcdaaaa) (Cstruct.of_string "bcd") ;
  let empty_pos cs pos =
    Alcotest.(check int) "length" (length cs) 0 ;
    Alcotest.(check int) "start_pos" (start_pos cs) pos in
  empty_pos (trim ~drop:drop_a aaaa) 4 ;
  empty_pos (trim (string "    ")) 2 ;
;;

let span =
  Alcotest.test_case "span" `Quick @@ fun () ->
  (* XXX(dinosaure): clash of names between [start] and [Cstruct.start]. *)
  let open Cstruct in
  let test ?(rev= false) ?min ?max ?sat cs (cl, cr as expect) =
    let res = span ~rev ?min ?max ?sat cs in
    let t = take ~rev ?min ?max ?sat cs in
    let d = drop ~rev ?min ?max ?sat cs in
    Alcotest.(check (pair cstruct cstruct)) "span" res expect ;
    Alcotest.(check cstruct) "take" t (if rev then cr else cl) ;
    Alcotest.(check cstruct) "drop" d (if rev then cl else cr) in
  let invalid ?rev ?min ?max ?sat cs =
    Alcotest.check_raises "invalid" (Invalid_argument "span")
      (fun () -> try ignore @@ span ?rev ?min ?max ?sat cs
            with Invalid_argument _ -> invalid_arg "span") in
  let is_white = function ' ' | '\t' .. '\r' -> true | _ -> false in
  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
  let { Cstruct.buffer= base; _ } = Cstruct.of_string "0ab cd0" in
  let empty = buffer ~off:3 ~len:0 base in
  let ab_cd = buffer ~off:1 ~len:5 base in
  let ab    = buffer ~off:1 ~len:2 base in
  let _cd   = buffer ~off:3 ~len:3 base in
  let cd    = buffer ~off:4 ~len:2 base in
  let ab_   = buffer ~off:1 ~len:3 base in
  let a     = buffer ~off:1 ~len:1 base in
  let b_cd  = buffer ~off:2 ~len:4 base in
  let b     = buffer ~off:2 ~len:1  base in
  let d     = buffer ~off:5 ~len:1 base in
  let ab_c  = buffer ~off:1 ~len:4 base in
  test ~rev:false ~min:1 ~max:0 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~min:1 ~max:0 ab_cd (ab_cd, stop ab_cd) ;
  test ~sat:is_white  ab_cd (start ab_cd, ab_cd) ;
  test ~sat:is_letter ab_cd (ab, _cd) ;
  test ~max:1 ~sat:is_letter ab_cd (a, b_cd) ;
  test ~max:0 ~sat:is_letter ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~sat:is_white ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:true  ~sat:is_letter ab_cd (ab_, cd) ;
  test ~rev:true  ~max:1 ~sat:is_letter ab_cd (ab_c, d) ;
  test ~rev:true  ~max:0 ~sat:is_letter ab_cd (ab_cd, stop ab_cd) ;
  test ~sat:is_letter ab (ab, stop ab) ;
  test ~max:1 ~sat:is_letter ab (a, b) ;
  test ~sat:is_letter b (b, empty) ;
  test ~rev:true  ~max:1 ~sat:is_letter ab (a, b) ;
  test ~max:1 ~sat:is_white ab (start ab, ab) ;
  test ~rev:true  ~sat:is_white empty (empty, empty) ;
  test ~sat:is_white empty (empty, empty) ;
  (* TODO: invalid *)
  invalid ~rev:false ~min:(-1) empty ;
  invalid ~rev:true  ~min:(-1) empty ;
  invalid ~rev:false ~max:(-1) empty ;
  invalid ~rev:true  ~max:(-1) empty ;
  test ~rev:false empty (empty, empty) ;
  test ~rev:true  empty (empty, empty) ;
  test ~rev:false ~min:0 ~max:0 empty (empty, empty) ;
  test ~rev:true  ~min:0 ~max:0 empty (empty, empty) ;
  test ~rev:false ~min:1 ~max:0 empty (empty, empty) ;
  test ~rev:true  ~min:1 ~max:0 empty (empty, empty) ;
  test ~rev:false ~max:0 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~max:0 ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:false ~max:2 ab_cd (ab, _cd) ;
  test ~rev:true  ~max:2 ab_cd (ab_, cd) ;
  test ~rev:false ~min:6 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~min:6 ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:false ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:true  ab_cd (start ab_cd, ab_cd) ;
  test ~rev:false ~max:30 ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:true  ~max:30 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:false ~sat:is_white ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~sat:is_white ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:false ~sat:is_letter ab_cd (ab, _cd) ;
  test ~rev:true  ~sat:is_letter ab_cd (ab_, cd) ;
  test ~rev:false ~sat:is_letter ~max:0 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~sat:is_letter ~max:0 ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:false ~sat:is_letter ~max:1 ab_cd (a, b_cd) ;
  test ~rev:true  ~sat:is_letter ~max:1 ab_cd (ab_c, d) ;
  test ~rev:false ~sat:is_letter ~min:2 ~max:1 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~sat:is_letter ~min:2 ~max:1 ab_cd (ab_cd, stop ab_cd) ;
  test ~rev:false ~sat:is_letter ~min:3 ab_cd (start ab_cd, ab_cd) ;
  test ~rev:true  ~sat:is_letter ~min:3 ab_cd (ab_cd, stop ab_cd) ;
;;

let cut =
  Alcotest.test_case "cut" `Quick @@ fun () ->
  let s str = string ~off:1 ~len:(String.length str) (Fmt.str "\x00%s\x00" str) in
  let cut ?rev ~sep str = cut ?rev ~sep:(s sep) (s str) in
  let invalid_cut_argument = Invalid_argument "cut: empty separator" in
  Alcotest.check_raises "invalid" invalid_cut_argument
    (fun () -> ignore (cut ~sep:"" "")) ;
  Alcotest.check_raises "invalid" invalid_cut_argument
    (fun () -> ignore (cut ~sep:"" "123")) ;
  let opc = Alcotest.(option (pair cstruct cstruct)) in
  Alcotest.(check opc) "0" (cut ~sep:"," "") None ;
  Alcotest.(check opc) "1" (cut ~sep:"," ",") (Some (string "", string "")) ;
  Alcotest.(check opc) "2" (cut ~sep:"," ",,") (Some (string "", string ",")) ;
  Alcotest.(check opc) "3" (cut ~sep:"," ",,,") (Some (string "", string ",,")) ;
  Alcotest.(check opc) "4" (cut ~sep:"," "123") None ;
  Alcotest.(check opc) "5" (cut ~sep:"," ",123") (Some (string "", string "123")) ;
  Alcotest.(check opc) "6" (cut ~sep:"," "123,") (Some (string "123", string "")) ;
  Alcotest.(check opc) "7" (cut ~sep:"," "1,2,3") (Some (string "1", string "2,3")) ;
  Alcotest.(check opc) "8" (cut ~sep:"," " 1,2,3") (Some (string " 1", string "2,3")) ;
  Alcotest.(check opc) "9" (cut ~sep:"<>" "") None ;
  Alcotest.(check opc) "10" (cut ~sep:"<>" "<>") (Some (string "", string "")) ;
  Alcotest.(check opc) "11" (cut ~sep:"<>" "<><>") (Some (string "", string "<>")) ;
  Alcotest.(check opc) "12" (cut ~sep:"<>" "<><><>") (Some (string "", string "<><>")) ;
  Alcotest.(check opc) "13" (cut ~rev:true ~sep:"<>" "1") None ;
  Alcotest.(check opc) "14" (cut ~sep:"<>" "123") None ;
  Alcotest.(check opc) "15" (cut ~sep:"<>" "<>123") (Some (string "", string "123")) ;
  Alcotest.(check opc) "16" (cut ~sep:"<>" "123<>") (Some (string "123", string "")) ;
  Alcotest.(check opc) "17" (cut ~sep:"<>" "1<>2<>3") (Some (string "1", string "2<>3")) ;
  Alcotest.(check opc) "18" (cut ~sep:"<>" ">>><>>>><>>>><>>>>") (Some (string ">>>", string ">>><>>>><>>>>")) ;
  Alcotest.(check opc) "19" (cut ~sep:"<->" "<->>->") (Some (string "", string ">->")) ;
  Alcotest.(check opc) "20" (cut ~rev:true ~sep:"<->" "<-") None ;
  Alcotest.(check opc) "21" (cut ~sep:"aa" "aa") (Some (string "", string "")) ;
  Alcotest.(check opc) "22" (cut ~sep:"aa" "aaa") (Some (string "", string "a")) ;
  Alcotest.(check opc) "23" (cut ~sep:"aa" "aaaa") (Some (string "", string "aa")) ;
  Alcotest.(check opc) "24" (cut ~sep:"aa" "aaaaa") (Some (string "", string "aaa")) ;
  Alcotest.(check opc) "25" (cut ~sep:"aa" "aaaaaa") (Some (string "", string "aaaa")) ;
  Alcotest.(check opc) "26" (cut ~sep:"ab" "faaaa") None ;
  let rev = true in
  Alcotest.check_raises "invalid" invalid_cut_argument
    (fun () -> ignore (cut ~rev ~sep:"" "")) ;
  Alcotest.check_raises "invalid" invalid_cut_argument
    (fun () -> ignore (cut ~rev ~sep:"" "123")) ;
  Alcotest.(check opc) "27" (cut ~rev ~sep:"," "") None ;
  Alcotest.(check opc) "28" (cut ~rev ~sep:"," ",") (Some (string "", string "")) ;
  Alcotest.(check opc) "29" (cut ~rev ~sep:"," ",,") (Some (string ",", string "")) ;
  Alcotest.(check opc) "30" (cut ~rev ~sep:"," ",,,") (Some (string ",,", string "")) ;
  Alcotest.(check opc) "31" (cut ~rev ~sep:"," "123") None ;
  Alcotest.(check opc) "32" (cut ~rev ~sep:"," ",123") (Some (string "", string "123")) ;
  Alcotest.(check opc) "33" (cut ~rev ~sep:"," "123,") (Some (string "123", string "")) ;
  Alcotest.(check opc) "34" (cut ~rev ~sep:"," "1,2,3") (Some (string "1,2", string "3")) ;
  Alcotest.(check opc) "35" (cut ~rev ~sep:"," "1,2,3 ") (Some (string "1,2", string "3 ")) ;
  Alcotest.(check opc) "36" (cut ~rev ~sep:"<>" "") None ;
  Alcotest.(check opc) "37" (cut ~rev ~sep:"<>" "<>") (Some (string "", string "")) ;
  Alcotest.(check opc) "38" (cut ~rev ~sep:"<>" "<><>") (Some (string "<>", string "")) ;
  Alcotest.(check opc) "39" (cut ~rev ~sep:"<>" "<><><>") (Some (string "<><>", string "")) ;
  Alcotest.(check opc) "40" (cut ~rev ~sep:"<>" "1") None ;
  Alcotest.(check opc) "41" (cut ~rev ~sep:"<>" "123") None ;
  Alcotest.(check opc) "42" (cut ~rev ~sep:"<>" "<>123") (Some (string "", string "123")) ;
  Alcotest.(check opc) "43" (cut ~rev ~sep:"<>" "123<>") (Some (string "123", string "")) ;
  Alcotest.(check opc) "44" (cut ~rev ~sep:"<>" "1<>2<>3") (Some (string "1<>2", string "3")) ;
  Alcotest.(check opc) "45" (cut ~rev ~sep:"<>" "1<>2<>3 ") (Some (string "1<>2", string "3 ")) ;
  Alcotest.(check opc) "46" (cut ~rev ~sep:"<>" ">>><>>>><>>>><>>>>") (Some (string ">>><>>>><>>>>", string ">>>")) ;
  Alcotest.(check opc) "47" (cut ~rev ~sep:"<->" "<->>->") (Some (string "", string ">->")) ;
  Alcotest.(check opc) "48" (cut ~rev ~sep:"<->" "<-") None ;
  Alcotest.(check opc) "49" (cut ~rev ~sep:"aa" "aa") (Some (string "", string "")) ;
  Alcotest.(check opc) "50" (cut ~rev ~sep:"aa" "aaa") (Some (string "a", string "")) ;
  Alcotest.(check opc) "51" (cut ~rev ~sep:"aa" "aaaa") (Some (string "aa", string "")) ;
  Alcotest.(check opc) "52" (cut ~rev ~sep:"aa" "aaaaa") (Some (string "aaa", string "")) ;
  Alcotest.(check opc) "53" (cut ~rev ~sep:"aa" "aaaaaa") (Some (string "aaaa", string "")) ;
  Alcotest.(check opc) "54" (cut ~rev ~sep:"ab" "afaaaa") None ;
  (* TODO: incomplete, see [astring]. *)
;;

let cuts =
  Alcotest.test_case "cuts" `Quick @@ fun () ->
  let ls = Alcotest.(list string) in
  let invalid_cuts_argument = Invalid_argument "cuts: empty separator" in
  let s str = string ~off:1 ~len:(String.length str) (Fmt.str "\x00%s\x00" str) in
  let cuts ?empty ?rev ~sep str =
    let res = cuts ?empty ?rev ~sep:(s sep) (s str) in
    List.map Cstruct.to_string res in
  Alcotest.check_raises "invalid" invalid_cuts_argument
    (fun () -> ignore (cuts ~sep:"" "")) ;
  Alcotest.check_raises "invalid" invalid_cuts_argument
    (fun () -> ignore (cuts ~sep:"" "")) ;
  Alcotest.(check ls) "0" (cuts ~empty:true  ~sep:"," "") [""] ;
  Alcotest.(check ls) "1" (cuts ~empty:false ~sep:"," "") [] ;
  Alcotest.(check ls) "2" (cuts ~empty:true  ~sep:"," "") [""];
  Alcotest.(check ls) "3" (cuts ~empty:false ~sep:"," "") [];
  Alcotest.(check ls) "4" (cuts ~empty:true  ~sep:"," ",") [""; ""];
  Alcotest.(check ls) "5" (cuts ~empty:false ~sep:"," ",") [];
  Alcotest.(check ls) "6" (cuts ~empty:true  ~sep:"," ",,") [""; ""; ""];
  Alcotest.(check ls) "7" (cuts ~empty:false ~sep:"," ",,") [];
  Alcotest.(check ls) "8" (cuts ~empty:true  ~sep:"," ",,,") [""; ""; ""; ""];
  Alcotest.(check ls) "9" (cuts ~empty:false ~sep:"," ",,,") [];
  Alcotest.(check ls) "10" (cuts ~empty:true  ~sep:"," "123") ["123"];
  Alcotest.(check ls) "11" (cuts ~empty:false ~sep:"," "123") ["123"];
  Alcotest.(check ls) "12" (cuts ~empty:true  ~sep:"," ",123") [""; "123"];
  Alcotest.(check ls) "13" (cuts ~empty:false ~sep:"," ",123") ["123"];
  Alcotest.(check ls) "14" (cuts ~empty:true  ~sep:"," "123,") ["123"; ""];
  Alcotest.(check ls) "15" (cuts ~empty:false ~sep:"," "123,") ["123";];
  Alcotest.(check ls) "16" (cuts ~empty:true  ~sep:"," "1,2,3") ["1"; "2"; "3"];
  Alcotest.(check ls) "17" (cuts ~empty:false ~sep:"," "1,2,3") ["1"; "2"; "3"];
  Alcotest.(check ls) "18" (cuts ~empty:true  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  Alcotest.(check ls) "19" (cuts ~empty:false  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  Alcotest.(check ls) "20" (cuts ~empty:true ~sep:"," ",1,2,,3,") [""; "1"; "2"; ""; "3"; ""];
  Alcotest.(check ls) "21" (cuts ~empty:false ~sep:"," ",1,2,,3,") ["1"; "2"; "3";];
  Alcotest.(check ls) "22" (cuts ~empty:true ~sep:"," ", 1, 2,, 3,")
    [""; " 1"; " 2"; ""; " 3"; ""];
  Alcotest.(check ls) "23" (cuts ~empty:false ~sep:"," ", 1, 2,, 3,") [" 1"; " 2";" 3";];
  Alcotest.(check ls) "24" (cuts ~empty:true ~sep:"<>" "") [""];
  Alcotest.(check ls) "25" (cuts ~empty:false ~sep:"<>" "") [];
  Alcotest.(check ls) "26" (cuts ~empty:true ~sep:"<>" "<>") [""; ""];
  Alcotest.(check ls) "27" (cuts ~empty:false ~sep:"<>" "<>") [];
  Alcotest.(check ls) "28" (cuts ~empty:true ~sep:"<>" "<><>") [""; ""; ""];
  Alcotest.(check ls) "29" (cuts ~empty:false ~sep:"<>" "<><>") [];
  Alcotest.(check ls) "30" (cuts ~empty:true ~sep:"<>" "<><><>") [""; ""; ""; ""];
  Alcotest.(check ls) "31" (cuts ~empty:false ~sep:"<>" "<><><>") [];
  Alcotest.(check ls) "32" (cuts ~empty:true ~sep:"<>" "123") [ "123" ];
  Alcotest.(check ls) "33" (cuts ~empty:false ~sep:"<>" "123") [ "123" ];
  Alcotest.(check ls) "34" (cuts ~empty:true ~sep:"<>" "<>123") [""; "123"];
  Alcotest.(check ls) "35" (cuts ~empty:false ~sep:"<>" "<>123") ["123"];
  Alcotest.(check ls) "36" (cuts ~empty:true ~sep:"<>" "123<>") ["123"; ""];
  Alcotest.(check ls) "37" (cuts ~empty:false ~sep:"<>" "123<>") ["123"];
  Alcotest.(check ls) "38" (cuts ~empty:true ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  Alcotest.(check ls) "39" (cuts ~empty:false ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  Alcotest.(check ls) "40" (cuts ~empty:true ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  Alcotest.(check ls) "41" (cuts ~empty:false ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  Alcotest.(check ls) "42" (cuts ~empty:true ~sep:"<>" "<>1<>2<><>3<>")
    [""; "1"; "2"; ""; "3"; ""];
  Alcotest.(check ls) "43" (cuts ~empty:false ~sep:"<>" "<>1<>2<><>3<>") ["1"; "2";"3";];
  Alcotest.(check ls) "44" (cuts ~empty:true ~sep:"<>" "<> 1<> 2<><> 3<>")
    [""; " 1"; " 2"; ""; " 3";""];
  Alcotest.(check ls) "45" (cuts ~empty:false ~sep:"<>" "<> 1<> 2<><> 3<>")[" 1"; " 2"; " 3"];
  Alcotest.(check ls) "46" (cuts ~empty:true ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  Alcotest.(check ls) "47" (cuts ~empty:false ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  Alcotest.(check ls) "48" (cuts ~empty:true ~sep:"<->" "<->>->") [""; ">->"];
  Alcotest.(check ls) "49" (cuts ~empty:false ~sep:"<->" "<->>->") [">->"];
  Alcotest.(check ls) "50" (cuts ~empty:true ~sep:"aa" "aa") [""; ""];
  Alcotest.(check ls) "51" (cuts ~empty:false ~sep:"aa" "aa") [];
  Alcotest.(check ls) "52" (cuts ~empty:true ~sep:"aa" "aaa") [""; "a"];
  Alcotest.(check ls) "53" (cuts ~empty:false ~sep:"aa" "aaa") ["a"];
  Alcotest.(check ls) "54" (cuts ~empty:true ~sep:"aa" "aaaa") [""; ""; ""];
  Alcotest.(check ls) "55" (cuts ~empty:false ~sep:"aa" "aaaa") [];
  Alcotest.(check ls) "56" (cuts ~empty:true ~sep:"aa" "aaaaa") [""; ""; "a"];
  Alcotest.(check ls) "57" (cuts ~empty:false ~sep:"aa" "aaaaa") ["a"];
  Alcotest.(check ls) "58" (cuts ~empty:true ~sep:"aa" "aaaaaa") [""; ""; ""; ""];
  Alcotest.(check ls) "59" (cuts ~empty:false ~sep:"aa" "aaaaaa") [];
;;

let fields =
  Alcotest.test_case "fields" `Quick @@ fun () ->
  let ls = Alcotest.(list string) in
  let s str = string ~off:1 ~len:(String.length str) (Fmt.str "\x00%s\x00" str) in
  let fields ?empty ?is_sep str =
    let res = fields ?empty ?is_sep (s str) in
    List.map Cstruct.to_string res in
  let is_a chr = chr = 'a' in
  Alcotest.(check ls) "0" (fields ~empty:true "a") ["a"];
  Alcotest.(check ls) "1" (fields ~empty:false "a") ["a"];
  Alcotest.(check ls) "2" (fields ~empty:true "abc") ["abc"];
  Alcotest.(check ls) "3" (fields ~empty:false "abc") ["abc"];
  Alcotest.(check ls) "4" (fields ~empty:true ~is_sep:is_a "bcdf") ["bcdf"];
  Alcotest.(check ls) "5" (fields ~empty:false ~is_sep:is_a "bcdf") ["bcdf"];
  Alcotest.(check ls) "6" (fields ~empty:true "") [""];
  Alcotest.(check ls) "7" (fields ~empty:false "") [];
  Alcotest.(check ls) "8" (fields ~empty:true "\n\r") ["";"";""];
  Alcotest.(check ls) "9" (fields ~empty:false "\n\r") [];
  Alcotest.(check ls) "10" (fields ~empty:true " \n\rabc") ["";"";"";"abc"];
  Alcotest.(check ls) "11" (fields ~empty:false " \n\rabc") ["abc"];
  Alcotest.(check ls) "12" (fields ~empty:true " \n\racd de") ["";"";"";"acd";"de"];
  Alcotest.(check ls) "13" (fields ~empty:false " \n\racd de") ["acd";"de"];
  Alcotest.(check ls) "14" (fields ~empty:true " \n\racd de ") ["";"";"";"acd";"de";""];
  Alcotest.(check ls) "15" (fields ~empty:false " \n\racd de ") ["acd";"de"];
  Alcotest.(check ls) "16" (fields ~empty:true "\n\racd\nde \r") ["";"";"acd";"de";"";""];
  Alcotest.(check ls) "17" (fields ~empty:false "\n\racd\nde \r") ["acd";"de"];
  Alcotest.(check ls) "18" (fields ~empty:true ~is_sep:is_a "") [""];
  Alcotest.(check ls) "19" (fields ~empty:false ~is_sep:is_a "") [];
  Alcotest.(check ls) "20" (fields ~empty:true ~is_sep:is_a "abaac aaa")
    ["";"b";"";"c ";"";"";""];
  Alcotest.(check ls) "21" (fields ~empty:false ~is_sep:is_a "abaac aaa") ["b"; "c "];
  Alcotest.(check ls) "22" (fields ~empty:true ~is_sep:is_a "aaaa") ["";"";"";"";""];
  Alcotest.(check ls) "23" (fields ~empty:false ~is_sep:is_a "aaaa") [];
  Alcotest.(check ls) "24" (fields ~empty:true ~is_sep:is_a "aaaa ") ["";"";"";"";" "];
  Alcotest.(check ls) "25" (fields ~empty:false ~is_sep:is_a "aaaa ") [" "];
  Alcotest.(check ls) "26" (fields ~empty:true ~is_sep:is_a "aaaab") ["";"";"";"";"b"];
  Alcotest.(check ls) "27" (fields ~empty:false ~is_sep:is_a "aaaab") ["b"];
  Alcotest.(check ls) "28" (fields ~empty:true ~is_sep:is_a "baaaa") ["b";"";"";"";""];
  Alcotest.(check ls) "29" (fields ~empty:false ~is_sep:is_a "baaaa") ["b"];
  Alcotest.(check ls) "30" (fields ~empty:true ~is_sep:is_a "abaaaa") ["";"b";"";"";"";""];
  Alcotest.(check ls) "31" (fields ~empty:false ~is_sep:is_a "abaaaa") ["b"];
  Alcotest.(check ls) "32" (fields ~empty:true ~is_sep:is_a "aba") ["";"b";""];
  Alcotest.(check ls) "33" (fields ~empty:false ~is_sep:is_a "aba") ["b"];
  Alcotest.(check ls) "34" (fields ~empty:false "tokenize me please")
    ["tokenize"; "me"; "please"];
;;

let find =
  Alcotest.test_case "find" `Quick @@ fun () ->
  let { Cstruct.buffer= abcbd; _ } = Cstruct.of_string "abcbd" in
  let empty = buffer ~off:3 ~len:0 abcbd in
  let a = buffer ~off:0 ~len:1 abcbd in
  let ab = buffer ~off:0 ~len:2 abcbd in
  let c = buffer ~off:2 ~len:1 abcbd in
  let b0 = buffer ~off:1 ~len:1 abcbd in
  let b1 = buffer ~off:3 ~len:1 abcbd in
  let abcbd = buffer abcbd in
  Alcotest.(check (option cstruct)) "0" (find (fun c -> c = 'b') empty) None;
  Alcotest.(check (option cstruct)) "1" (find ~rev:true (fun c -> c = 'b') empty) None;
  Alcotest.(check (option cstruct)) "2" (find (fun c -> c = 'b') a) None;
  Alcotest.(check (option cstruct)) "3" (find ~rev:true (fun c -> c = 'b') a) None;
  Alcotest.(check (option cstruct)) "4" (find (fun c -> c = 'b') c) None;
  Alcotest.(check (option cstruct)) "5" (find ~rev:true (fun c -> c = 'b') c) None;
  Alcotest.(check (option cstruct)) "6" (find (fun c -> c = 'b') abcbd) (Some b0);
  Alcotest.(check (option cstruct)) "7" (find ~rev:true (fun c -> c = 'b') abcbd) (Some b1);
  Alcotest.(check (option cstruct)) "8" (find (fun c -> c = 'b') ab) (Some b0);
  Alcotest.(check (option cstruct)) "9" (find ~rev:true (fun c -> c = 'b') ab) (Some b0);
;;

let find_sub =
  Alcotest.test_case "find_sub" `Quick @@ fun () ->
  let { Cstruct.buffer= abcbd; _ } = Cstruct.of_string "abcbd" in
  let empty = buffer ~off:3 ~len:0 abcbd in
  let ab = buffer ~off:0 ~len:2 abcbd in
  let b0 = buffer ~off:1 ~len:1 abcbd in
  let b1 = buffer ~off:3 ~len:1 abcbd in
  let abcbd = buffer abcbd in
  Alcotest.(check (option cstruct)) "0" (find_sub ~sub:ab empty) None;
  Alcotest.(check (option cstruct)) "1" (find_sub ~rev:true ~sub:ab empty) None;
  Alcotest.(check (option cstruct)) "2" (find_sub ~sub:(Cstruct.of_string "") empty) (Some empty);
  Alcotest.(check (option cstruct)) "3" (find_sub ~rev:true ~sub:(Cstruct.of_string "") empty) (Some empty);
  Alcotest.(check (option cstruct)) "4" (find_sub ~sub:ab abcbd) (Some ab);
  Alcotest.(check (option cstruct)) "5" (find_sub ~rev:true ~sub:ab abcbd) (Some ab);
  Alcotest.(check (option cstruct)) "6" (find_sub ~sub:empty abcbd) (Some (Cstruct.start abcbd));
  Alcotest.(check (option cstruct)) "7" (find_sub ~rev:true ~sub:empty abcbd)
    (Some (Cstruct.stop abcbd));
  Alcotest.(check (option cstruct)) "8" (find_sub ~sub:(Cstruct.of_string "b") abcbd) (Some b0);
  Alcotest.(check (option cstruct)) "9" (find_sub ~rev:true ~sub:(Cstruct.of_string "b") abcbd) (Some b1);
  Alcotest.(check (option cstruct)) "10" (find_sub ~sub:b1 ab) (Some b0);
  Alcotest.(check (option cstruct)) "11" (find_sub ~rev:true ~sub:b1 ab) (Some b0);
;;

let () = Alcotest.run "cstruct.parse"
    [ "parse", [ misc; head; start; stop; tail
               ; is_empty; is_prefix; is_infix; is_suffix
               ; for_all; exists
               ; trim
               ; span
               ; cut; cuts
               ; fields
               ; find; find_sub ] ]
