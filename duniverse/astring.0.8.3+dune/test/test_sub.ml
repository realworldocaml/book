(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring

let pp_str_pair ppf (a, b) =
  Format.fprintf ppf "@[<1>(%a,%a)@]" String.dump a String.dump b

let pp_pair ppf (a, b) =
  Format.fprintf ppf "@[<1>(%a,%a)@]" String.Sub.dump a String.Sub.dump b

let eq_pair (l0, r0) (l1, r1) =
  String.Sub.equal l0 l1 && String.Sub.equal r0 r1

let eq_sub = eq ~eq:String.Sub.equal ~pp:String.Sub.dump
let eq_sub_raw = eq ~eq:String.Sub.equal ~pp:String.Sub.dump_raw

let eqs sub s = eq_str (String.Sub.to_string sub) s
let eqb sub s = eq_str (String.Sub.base_string sub) s
let eqs_opt sub os =
  let sub_to_str = function
  | Some sub -> Some (String.Sub.to_string sub)
  | None -> None
  in
  eq_option ~eq:(=) ~pp:pp_str (sub_to_str sub) os

let eqs_pair_opt subs_pair pair =
  let subs_to_str = function
  | None -> None
  | Some (sub, sub') ->
      Some (String.Sub.to_string sub, String.Sub.to_string sub')
  in
  eq_option ~eq:(=) ~pp:pp_str_pair (subs_to_str subs_pair) pair

let empty_pos s pos =
  eq_int (String.Sub.length s) 0;
  eq_int (String.Sub.start_pos s) pos

(* Base functions *)

let misc = test "String.Sub misc. base functions" @@ fun () ->
  eqs String.Sub.empty "";
  eqs (String.Sub.v "abc") "abc";
  eqs (String.Sub.v ~start:0 ~stop:1 "abc") "a";
  eqs (String.Sub.v ~start:1 ~stop:2 "abc") "b";
  eqs (String.Sub.v ~start:1 ~stop:3 "abc") "bc";
  eqs (String.Sub.v ~start:2 ~stop:3 "abc") "c";
  eqs (String.Sub.v ~start:3 ~stop:3 "abc") "";
  let sub = String.Sub.v ~start:2 ~stop:3 "abc" in
  eq_int (String.Sub.start_pos sub) 2;
  eq_int (String.Sub.stop_pos sub) 3;
  eq_int (String.Sub.length sub) 1;
  app_invalid ~pp:pp_char (String.Sub.get sub) 3;
  app_invalid ~pp:pp_char (String.Sub.get sub) 2;
  app_invalid ~pp:pp_char (String.Sub.get sub) 1;
  eq_char (String.Sub.get sub 0) 'c';
  eq_int (String.Sub.get_byte sub 0) 0x63;
  eqb (String.Sub.(rebase (v ~stop:2 "abc"))) "ab";
  ()

let head = test "String.[get_]head" @@ fun () ->
  let empty = String.Sub.v ~start:2 ~stop:2 "abc" in
  let bc = String.Sub.v ~start:1 ~stop:3 "abc" in
  let eq_ochar = eq_option ~eq:(=) ~pp:pp_char in
  eq_ochar (String.Sub.head empty) None;
  eq_ochar (String.Sub.head ~rev:true empty) None;
  eq_ochar (String.Sub.head bc) (Some 'b');
  eq_ochar (String.Sub.head ~rev:true bc) (Some 'c');
  eq_char (String.Sub.get_head bc) 'b';
  eq_char (String.Sub.get_head ~rev:true bc) 'c';
  app_invalid ~pp:pp_char String.Sub.get_head empty;
  ()

let to_string = test "String.Sub.to_string" @@ fun () ->
  let no_alloc s =
    let r = String.Sub.to_string s in
    eq_bool (r == (String.Sub.base_string s) || r == String.empty) true
  in
  no_alloc (String.Sub.v ~start:0 ~stop:0 "abc");
  eq_str (String.Sub.(to_string (v ~start:0 ~stop:1 "abc"))) "a";
  eq_str (String.Sub.(to_string (v ~start:0 ~stop:2 "abc"))) "ab";
  no_alloc (String.Sub.v ~start:0 ~stop:3 "abc");
  no_alloc (String.Sub.v ~start:1 ~stop:1 "abc");
  eq_str (String.Sub.(to_string (v ~start:1 ~stop:2 "abc"))) "b";
  eq_str (String.Sub.(to_string (v ~start:1 ~stop:3 "abc"))) "bc";
  no_alloc (String.Sub.v ~start:2 ~stop:2 "abc");
  eq_str (String.Sub.(to_string (v ~start:2 ~stop:3 "abc"))) "c";
  no_alloc (String.Sub.v ~start:3 ~stop:3 "abc");
  ()

(* Stretching substrings *)

let start = test "String.Sub.start" @@ fun () ->
  empty_pos String.Sub.(start @@ v "") 0;
  empty_pos String.Sub.(start @@ v ~start:0 ~stop:0 "abc") 0;
  empty_pos String.Sub.(start @@ v ~start:0 ~stop:1 "abc") 0;
  empty_pos String.Sub.(start @@ v ~start:0 ~stop:2 "abc") 0;
  empty_pos String.Sub.(start @@ v ~start:0 ~stop:3 "abc") 0;
  empty_pos String.Sub.(start @@ v ~start:1 ~stop:1 "abc") 1;
  empty_pos String.Sub.(start @@ v ~start:1 ~stop:2 "abc") 1;
  empty_pos String.Sub.(start @@ v ~start:1 ~stop:3 "abc") 1;
  empty_pos String.Sub.(start @@ v ~start:2 ~stop:2 "abc") 2;
  empty_pos String.Sub.(start @@ v ~start:2 ~stop:3 "abc") 2;
  empty_pos String.Sub.(start @@ v ~start:3 ~stop:3 "abc") 3;
  ()

let stop = test "String.Sub.stop" @@ fun () ->
  empty_pos String.Sub.(stop @@ v "") 0;
  empty_pos String.Sub.(stop @@ v ~start:0 ~stop:0 "abc") 0;
  empty_pos String.Sub.(stop @@ v ~start:0 ~stop:1 "abc") 1;
  empty_pos String.Sub.(stop @@ v ~start:0 ~stop:2 "abc") 2;
  empty_pos String.Sub.(stop @@ v ~start:0 ~stop:3 "abc") 3;
  empty_pos String.Sub.(stop @@ v ~start:1 ~stop:1 "abc") 1;
  empty_pos String.Sub.(stop @@ v ~start:1 ~stop:2 "abc") 2;
  empty_pos String.Sub.(stop @@ v ~start:1 ~stop:3 "abc") 3;
  empty_pos String.Sub.(stop @@ v ~start:2 ~stop:2 "abc") 2;
  empty_pos String.Sub.(stop @@ v ~start:2 ~stop:3 "abc") 3;
  empty_pos String.Sub.(stop @@ v ~start:3 ~stop:3 "abc") 3;
  ()

let tail = test "String.Sub.tail" @@ fun () ->
  empty_pos String.Sub.(tail @@ v "") 0;
  empty_pos String.Sub.(tail @@ v ~start:0 ~stop:0 "abc") 0;
  empty_pos String.Sub.(tail @@ v ~start:0 ~stop:1 "abc") 1;
  eqs String.Sub.(tail @@ v ~start:0 ~stop:2 "abc") "b";
  eqs String.Sub.(tail @@ v ~start:0 ~stop:3 "abc") "bc";
  empty_pos String.Sub.(tail @@ v ~start:1 ~stop:1 "abc") 1;
  empty_pos String.Sub.(tail @@ v ~start:1 ~stop:2 "abc") 2;
  eqs String.Sub.(tail @@ v ~start:1 ~stop:3 "abc") "c";
  empty_pos String.Sub.(tail @@ v ~start:2 ~stop:2 "abc") 2;
  empty_pos String.Sub.(tail @@ v ~start:2 ~stop:3 "abc") 3;
  empty_pos String.Sub.(tail @@ v ~start:3 ~stop:3 "abc") 3;
  ()

let base = test "String.Sub.base" @@ fun () ->
  eqs String.Sub.(base @@ v "") "";
  eqs String.Sub.(base @@ v ~start:0 ~stop:0 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:0 ~stop:1 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:0 ~stop:2 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:0 ~stop:3 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:1 ~stop:1 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:1 ~stop:2 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:1 ~stop:3 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:2 ~stop:2 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:2 ~stop:3 "abc") "abc";
  eqs String.Sub.(base @@ v ~start:3 ~stop:3 "abc") "abc";
  ()

let extend = test "String.Sub.extend" @@ fun () ->
  let empty = String.Sub.v ~start:2 ~stop:2 "abcdefg" in
  let abcd = String.Sub.v ~start:0 ~stop:4 "abcdefg" in
  let cd = String.Sub.v ~start:2 ~stop:4 "abcdefg" in
  app_invalid ~pp:String.Sub.pp (fun s -> String.Sub.extend ~max:(-1) s) abcd;
  eqs (String.Sub.extend empty) "cdefg";
  eqs (String.Sub.extend ~rev:true empty) "ab";
  eqs (String.Sub.extend ~max:2 empty) "cd";
  eqs (String.Sub.extend ~sat:(fun c -> c < 'f') empty) "cde";
  eqs (String.Sub.extend ~rev:true ~max:1 empty) "b";
  eqs (String.Sub.extend abcd) "abcdefg";
  eqs (String.Sub.extend ~max:2 abcd) "abcdef";
  eqs (String.Sub.extend ~max:2 ~sat:(fun c -> c < 'e') abcd) "abcd";
  eqs (String.Sub.extend ~max:2 ~sat:(fun c -> c < 'f') abcd) "abcde";
  eqs (String.Sub.extend ~max:2 ~sat:(fun c -> c < 'g') abcd) "abcdef";
  eqs (String.Sub.extend ~rev:true abcd) "abcd";
  eqs (String.Sub.extend ~rev:true ~max:2 ~sat:(fun c -> c > 'a') cd) "bcd";
  eqs (String.Sub.extend ~rev:true ~max:2 ~sat:(fun c -> c >= 'a') cd) "abcd";
  eqs (String.Sub.extend ~rev:true ~max:1 ~sat:(fun c -> c >= 'a') cd) "bcd";
  eqs (String.Sub.extend ~max:2 ~sat:(fun c -> c >= 'a') cd) "cdef";
  eqs (String.Sub.extend ~sat:(fun c -> c >= 'a') cd) "cdefg";
  ()

let reduce = test "String.Sub.reduce" @@ fun () ->
  let empty = String.Sub.v ~start:2 ~stop:2 "abcdefg" in
  let abcd = String.Sub.v ~start:0 ~stop:4 "abcdefg" in
  let cd = String.Sub.v ~start:2 ~stop:4 "abcdefg" in
  app_invalid ~pp:String.Sub.pp (fun s -> String.Sub.reduce ~max:(-1) s) abcd;
  empty_pos (String.Sub.reduce ~rev:false empty) 2;
  empty_pos (String.Sub.reduce ~rev:true  empty) 2;
  empty_pos (String.Sub.reduce ~rev:false ~max:2 empty) 2;
  empty_pos (String.Sub.reduce ~rev:true  ~max:2 empty) 2;
  empty_pos (String.Sub.reduce ~rev:false ~sat:(fun c -> c < 'f') empty) 2;
  empty_pos (String.Sub.reduce ~rev:true  ~sat:(fun c -> c < 'f') empty) 2;
  empty_pos (String.Sub.reduce ~rev:false ~max:1 empty) 2;
  empty_pos (String.Sub.reduce ~rev:true  ~max:1 empty) 2;
  empty_pos (String.Sub.reduce ~rev:false abcd) 0;
  empty_pos (String.Sub.reduce ~rev:true  abcd) 4;
  eqs (String.Sub.reduce ~rev:false ~max:0 abcd) "abcd";
  eqs (String.Sub.reduce ~rev:true  ~max:0 abcd) "abcd";
  eqs (String.Sub.reduce ~rev:false ~max:0 abcd) "abcd";
  eqs (String.Sub.reduce ~rev:true  ~max:0 abcd) "abcd";
  eqs (String.Sub.reduce ~rev:false ~max:2 abcd) "ab";
  eqs (String.Sub.reduce ~rev:true  ~max:2 abcd) "cd";
  eqs (String.Sub.reduce ~rev:false ~max:2 ~sat:(fun c -> c < 'c') abcd) "abcd";
  eqs (String.Sub.reduce ~rev:true  ~max:2 ~sat:(fun c -> c < 'c') abcd) "cd";
  eqs (String.Sub.reduce ~rev:false ~max:2 ~sat:(fun c -> c > 'b') abcd) "ab";
  eqs (String.Sub.reduce ~rev:true  ~max:2 ~sat:(fun c -> c < 'b') abcd) "bcd";
  eqs (String.Sub.reduce ~rev:false ~max:2 ~sat:(fun c -> c > 'a') abcd) "ab";
  eqs (String.Sub.reduce ~rev:true  ~max:2 ~sat:(fun c -> c < 'a') abcd) "abcd";
  empty_pos (String.Sub.reduce ~rev:false ~max:2 ~sat:(fun c -> c > 'a') cd) 2;
  empty_pos (String.Sub.reduce ~rev:true ~max:2 ~sat:(fun c -> c > 'a') cd) 4;
  eqs (String.Sub.reduce ~rev:false ~max:2 ~sat:(fun c -> c >= 'd') cd) "c";
  eqs (String.Sub.reduce ~rev:true ~max:2 ~sat:(fun c -> c >= 'd') cd) "cd";
  eqs (String.Sub.reduce ~rev:false ~max:1 ~sat:(fun c -> c > 'c') cd) "c";
  eqs (String.Sub.reduce ~rev:true ~max:1 ~sat:(fun c -> c > 'c') cd) "cd";
  eqs (String.Sub.reduce ~rev:false ~sat:(fun c -> c > 'c') cd) "c";
  eqs (String.Sub.reduce ~rev:true  ~sat:(fun c -> c > 'c') cd) "cd";
  ()

let extent = test "String.Sub.extent" @@ fun () ->
  app_invalid ~pp:String.Sub.dump_raw
    String.Sub.(extent (v "a")) (String.Sub.(v "b"));
  let abcd = "abcd" in
  let e0 = String.Sub.v ~start:0 ~stop:0 abcd in
  let a = String.Sub.v ~start:0 ~stop:1 abcd in
  let ab = String.Sub.v ~start:0 ~stop:2 abcd in
  let abc = String.Sub.v ~start:0 ~stop:3 abcd in
  let _abcd = String.Sub.v ~start:0 ~stop:4 abcd in
  let e1 = String.Sub.v ~start:1 ~stop:1 abcd in
  let b = String.Sub.v ~start:1 ~stop:2 abcd in
  let bc = String.Sub.v ~start:1 ~stop:3 abcd in
  let bcd = String.Sub.v ~start:1 ~stop:4 abcd in
  let e2 = String.Sub.v ~start:2 ~stop:2 abcd in
  let c = String.Sub.v ~start:2 ~stop:3 abcd in
  let cd = String.Sub.v ~start:2 ~stop:4 abcd in
  let e3 = String.Sub.v ~start:3 ~stop:3 abcd in
  let d = String.Sub.v ~start:3 ~stop:4 abcd in
  let e4 = String.Sub.v ~start:4 ~stop:4 abcd in
  empty_pos (String.Sub.extent e0 e0) 0;
  eqs (String.Sub.extent e0 e1) "a";
  eqs (String.Sub.extent e1 e0) "a";
  eqs (String.Sub.extent e0 e2) "ab";
  eqs (String.Sub.extent e2 e0) "ab";
  eqs (String.Sub.extent e0 e3) "abc";
  eqs (String.Sub.extent e3 e0) "abc";
  eqs (String.Sub.extent e0 e4) "abcd";
  eqs (String.Sub.extent e4 e0) "abcd";
  empty_pos (String.Sub.extent e1 e1) 1;
  eqs (String.Sub.extent e1 e2) "b";
  eqs (String.Sub.extent e2 e1) "b";
  eqs (String.Sub.extent e1 e3) "bc";
  eqs (String.Sub.extent e3 e1) "bc";
  eqs (String.Sub.extent e1 e4) "bcd";
  eqs (String.Sub.extent e4 e1) "bcd";
  empty_pos (String.Sub.extent e2 e2) 2;
  eqs (String.Sub.extent e2 e3) "c";
  eqs (String.Sub.extent e3 e2) "c";
  eqs (String.Sub.extent e2 e4) "cd";
  eqs (String.Sub.extent e4 e2) "cd";
  empty_pos (String.Sub.extent e3 e3) 3;
  eqs (String.Sub.extent e3 e4) "d";
  eqs (String.Sub.extent e4 e3) "d";
  empty_pos (String.Sub.extent e4 e4) 4;
  eqs (String.Sub.extent a d) "abcd";
  eqs (String.Sub.extent d a) "abcd";
  eqs (String.Sub.extent b d) "bcd";
  eqs (String.Sub.extent d b) "bcd";
  eqs (String.Sub.extent c cd) "cd";
  eqs (String.Sub.extent cd c) "cd";
  eqs (String.Sub.extent e0 _abcd) "abcd";
  eqs (String.Sub.extent _abcd e0) "abcd";
  eqs (String.Sub.extent ab c) "abc";
  eqs (String.Sub.extent c ab) "abc";
  eqs (String.Sub.extent bc c) "bc";
  eqs (String.Sub.extent c bc) "bc";
  eqs (String.Sub.extent abc d) "abcd";
  eqs (String.Sub.extent d abc) "abcd";
  eqs (String.Sub.extent d bcd) "bcd";
  eqs (String.Sub.extent bcd d) "bcd";
  ()

let overlap = test "String.Sub.overlap" @@ fun () ->
  let empty_pos sub pos = match sub with
  | None -> fail "no sub" | Some sub -> empty_pos sub pos
  in
  app_invalid ~pp:String.Sub.dump_raw
    String.Sub.(extent (v "a")) (String.Sub.(v "b"));
  let abcd = "abcd" in
  let e0 = String.Sub.v ~start:0 ~stop:0 abcd in
  let a = String.Sub.v ~start:0 ~stop:1 abcd in
  let ab = String.Sub.v ~start:0 ~stop:2 abcd in
  let abc = String.Sub.v ~start:0 ~stop:3 abcd in
  let _abcd = String.Sub.v ~start:0 ~stop:4 abcd in
  let e1 = String.Sub.v ~start:1 ~stop:1 abcd in
  let b = String.Sub.v ~start:1 ~stop:2 abcd in
  let bc = String.Sub.v ~start:1 ~stop:3 abcd in
  let bcd = String.Sub.v ~start:1 ~stop:4 abcd in
  let e2 = String.Sub.v ~start:2 ~stop:2 abcd in
  let c = String.Sub.v ~start:2 ~stop:3 abcd in
  let cd = String.Sub.v ~start:2 ~stop:4 abcd in
  let e3 = String.Sub.v ~start:3 ~stop:3 abcd in
  let d = String.Sub.v ~start:3 ~stop:4 abcd in
  let e4 = String.Sub.v ~start:4 ~stop:4 abcd in
  empty_pos (String.Sub.overlap e0 a) 0;
  empty_pos (String.Sub.overlap a e0) 0;
  eqs_opt (String.Sub.overlap e0 b) None;
  eqs_opt (String.Sub.overlap b e0) None;
  empty_pos (String.Sub.overlap a b) 1;
  empty_pos (String.Sub.overlap b a) 1;
  eqs_opt (String.Sub.overlap a a) (Some "a");
  eqs_opt (String.Sub.overlap a ab) (Some "a");
  eqs_opt (String.Sub.overlap ab ab) (Some "ab");
  eqs_opt (String.Sub.overlap ab abc) (Some "ab");
  eqs_opt (String.Sub.overlap abc ab) (Some "ab");
  eqs_opt (String.Sub.overlap b abc) (Some "b");
  eqs_opt (String.Sub.overlap abc b) (Some "b");
  empty_pos (String.Sub.overlap abc e3) 3;
  empty_pos (String.Sub.overlap e3 abc) 3;
  eqs_opt (String.Sub.overlap ab bc) (Some "b");
  eqs_opt (String.Sub.overlap bc ab) (Some "b");
  eqs_opt (String.Sub.overlap bcd bc) (Some "bc");
  eqs_opt (String.Sub.overlap bc bcd) (Some "bc");
  eqs_opt (String.Sub.overlap bcd d) (Some "d");
  eqs_opt (String.Sub.overlap d bcd) (Some "d");
  eqs_opt (String.Sub.overlap bcd cd) (Some "cd");
  eqs_opt (String.Sub.overlap cd bcd) (Some "cd");
  eqs_opt (String.Sub.overlap bcd c) (Some "c");
  eqs_opt (String.Sub.overlap c bcd) (Some "c");
  empty_pos (String.Sub.overlap e2 bcd) 2;
  empty_pos (String.Sub.overlap bcd e2) 2;
  empty_pos (String.Sub.overlap bcd e3) 3;
  empty_pos (String.Sub.overlap e3 bcd) 3;
  empty_pos (String.Sub.overlap bcd e4) 4;
  empty_pos (String.Sub.overlap e4 bcd) 4;
  empty_pos (String.Sub.overlap e1 e1) 1;
  eqs_opt (String.Sub.overlap e0 bcd) None;
  ()

(* Appending substrings. *)

let append = test "String.Sub.append" @@ fun () ->
  let no_allocl s s' =
    eq_bool (String.Sub.(base_string @@ append s s') ==
             String.Sub.(base_string s)) true
  in
  let no_allocr s s' =
    eq_bool (String.Sub.(base_string @@ append s s') ==
             String.Sub.(base_string s')) true
  in
  no_allocl String.Sub.empty String.Sub.empty;
  no_allocr String.Sub.empty String.Sub.empty;
  no_allocl (String.Sub.v "bla") (String.Sub.v ~start:0 ~stop:0 "abcd");
  no_allocr (String.Sub.v ~start:1 ~stop:1 "b") (String.Sub.v "bli");
  let a = String.sub_with_index_range ~first:0 ~last:0 "abcd" in
  let ab = String.sub_with_index_range ~first:0 ~last:1 "abcd" in
  let cd = String.sub_with_index_range ~first:2 ~last:3 "abcd" in
  let empty = String.Sub.v ~start:4 ~stop:4 "abcd" in
  eqb (String.Sub.append a empty) "a";
  eqb (String.Sub.append empty a) "a";
  eqb (String.Sub.append ab empty) "ab";
  eqb (String.Sub.append empty ab) "ab";
  eqb (String.Sub.append ab cd) "abcd";
  eqb (String.Sub.append cd ab) "cdab";
  ()

let concat = test "String.Sub.concat" @@ fun () ->
  let dash = String.sub_with_range ~first:2 ~len:1 "ab-d" in
  let ddash = String.sub_with_range ~first:1 ~len:2 "a--d" in
  let empty = String.sub_with_range ~first:2 ~len:0 "ab-d" in
  let no_alloc ?sep s =
    let r = String.Sub.(base_string (concat ?sep [s])) in
    eq_bool (r == String.Sub.base_string s || r == String.empty) true
  in
  no_alloc empty;
  no_alloc (String.Sub.v "hey");
  no_alloc ~sep:empty empty;
  no_alloc ~sep:dash empty;
  no_alloc ~sep:empty (String.Sub.v "abc");
  no_alloc ~sep:dash (String.Sub.v "abc");
  let sempty = String.Sub.v ~start:2 ~stop:2 "abc" in
  let a = String.Sub.v ~start:2 ~stop:3 "kka" in
  let b = String.Sub.v ~start:1 ~stop:2 "ubd" in
  let c = String.Sub.v ~start:0 ~stop:1 "cdd" in
  let ab = String.Sub.v ~start:1 ~stop:3 "gabi" in
  let abc = String.Sub.v ~start:5 ~stop:8 "zuuuuabcbb" in
  eqb (String.Sub.concat ~sep:empty []) "";
  eqb (String.Sub.concat ~sep:empty [sempty]) "";
  eqb (String.Sub.concat ~sep:empty [sempty;sempty]) "";
  eqb (String.Sub.concat ~sep:empty [a;b;]) "ab";
  eqb (String.Sub.concat ~sep:empty [a;b;sempty;c]) "abc";
  eqb (String.Sub.concat ~sep:dash []) "";
  eqb (String.Sub.concat ~sep:dash [sempty]) "";
  eqb (String.Sub.concat ~sep:dash [a]) "a";
  eqb (String.Sub.concat ~sep:dash [a;sempty]) "a-";
  eqb (String.Sub.concat ~sep:dash [sempty;a]) "-a";
  eqb (String.Sub.concat ~sep:dash [sempty;a;sempty]) "-a-";
  eqb (String.Sub.concat ~sep:dash [a;b;c]) "a-b-c";
  eqb (String.Sub.concat ~sep:ddash [a;b;c]) "a--b--c";
  eqb (String.Sub.concat ~sep:ab [a;b;c]) "aabbabc";
  eqb (String.Sub.concat ~sep:ab [abc;b;c]) "abcabbabc";
  ()

(* Predicates *)

let is_empty = test "String.Sub.is_empty" @@ fun () ->
  eq_bool (String.Sub.is_empty (String.Sub.v "")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:4 ~stop:4 "abcd")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:0 ~stop:0 "huiy")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:0 ~stop:1 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:0 ~stop:2 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:0 ~stop:3 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:0 ~stop:4 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:1 ~stop:1 "abcd")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:1 ~stop:2 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:1 ~stop:3 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:1 ~stop:4 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:2 ~stop:2 "abcd")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:2 ~stop:3 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:3 ~stop:3 "abcd")) true;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:3 ~stop:4 "huiy")) false;
  eq_bool (String.Sub.is_empty (String.Sub.v ~start:4 ~stop:4 "huiy")) true;
  ()

let is_prefix = test "String.Sub.is_prefix" @@ fun () ->
  let empty = String.sub_with_range ~first:3 ~len:0 "ugoadfj" in
  let sempty = String.sub_with_range ~first:4 ~len:0 "dfkdjf" in
  let habla = String.sub_with_range ~first:2 ~len:5 "abhablablu" in
  let h = String.sub_with_range ~first:0 ~len:1 "hadfdffdf" in
  let ha = String.sub_with_range ~first:0 ~len:2 "hadfdffdf" in
  let hab = String.sub_with_range ~first:3 ~len:3 "hadhabdffdf" in
  let abla = String.sub_with_range ~first:1 ~len:4 "iabla" in
  eqs empty ""; eqs sempty ""; eqs habla "habla"; eqs h "h"; eqs ha "ha";
  eqs hab "hab"; eqs abla "abla";
  eq_bool (String.Sub.is_prefix ~affix:empty sempty) true;
  eq_bool (String.Sub.is_prefix ~affix:empty habla) true;
  eq_bool (String.Sub.is_prefix ~affix:ha sempty) false;
  eq_bool (String.Sub.is_prefix ~affix:ha h) false;
  eq_bool (String.Sub.is_prefix ~affix:ha ha) true;
  eq_bool (String.Sub.is_prefix ~affix:ha hab) true;
  eq_bool (String.Sub.is_prefix ~affix:ha habla) true;
  eq_bool (String.Sub.is_prefix ~affix:ha abla) false;
  ()

let is_infix = test "String.Sub.is_infix" @@ fun () ->
  let empty = String.sub_with_range ~first:1 ~len:0 "ugoadfj" in
  let sempty = String.sub_with_range ~first:2 ~len:0 "dfkdjf" in
  let asdf = String.sub_with_range ~first:1 ~len:4 "aasdflablu" in
  let a = String.sub_with_range ~first:2 ~len:1 "cda" in
  let h = String.sub_with_range ~first:0 ~len:1 "h" in
  let ha = String.sub_with_range ~first:1 ~len:2 "uhadfdffdf" in
  let ah = String.sub_with_range ~first:0 ~len:2 "ah" in
  let aha = String.sub_with_range ~first:2 ~len:3 "aaaha" in
  let haha = String.sub_with_range ~first:1 ~len:4 "ahaha" in
  let hahb = String.sub_with_range ~first:0 ~len:4 "hahbdfdf" in
  let blhahb = String.sub_with_range ~first:0 ~len:6 "blhahbdfdf" in
  let blha = String.sub_with_range ~first:1 ~len:4 "fblhahbdfdf" in
  let blh = String.sub_with_range ~first:1 ~len:3 "fblhahbdfdf" in
  eqs asdf "asdf"; eqs ha "ha"; eqs h "h"; eqs a "a"; eqs aha "aha";
  eqs haha "haha"; eqs hahb "hahb"; eqs blhahb "blhahb"; eqs blha "blha";
  eqs blh "blh";
  eq_bool (String.Sub.is_infix ~affix:empty sempty) true;
  eq_bool (String.Sub.is_infix ~affix:empty asdf) true;
  eq_bool (String.Sub.is_infix ~affix:empty ha) true;
  eq_bool (String.Sub.is_infix ~affix:ha sempty) false;
  eq_bool (String.Sub.is_infix ~affix:ha a) false;
  eq_bool (String.Sub.is_infix ~affix:ha h) false;
  eq_bool (String.Sub.is_infix ~affix:ha ah) false;
  eq_bool (String.Sub.is_infix ~affix:ha ha) true;
  eq_bool (String.Sub.is_infix ~affix:ha aha) true;
  eq_bool (String.Sub.is_infix ~affix:ha haha) true;
  eq_bool (String.Sub.is_infix ~affix:ha hahb) true;
  eq_bool (String.Sub.is_infix ~affix:ha blhahb) true;
  eq_bool (String.Sub.is_infix ~affix:ha blha) true;
  eq_bool (String.Sub.is_infix ~affix:ha blh) false;
  ()

let is_suffix = test "String.Sub.is_suffix" @@ fun () ->
  let empty = String.sub_with_range ~first:1 ~len:0 "ugoadfj" in
  let sempty = String.sub_with_range ~first:2 ~len:0 "dfkdjf" in
  let asdf = String.sub_with_range ~first:1 ~len:4 "aasdflablu" in
  let ha = String.sub_with_range ~first:1 ~len:2 "uhadfdffdf" in
  let h = String.sub_with_range ~first:0 ~len:1 "h" in
  let a = String.sub_with_range ~first:2 ~len:1 "cda" in
  let ah = String.sub_with_range ~first:0 ~len:2 "ah" in
  let aha = String.sub_with_range ~first:2 ~len:3 "aaaha" in
  let haha = String.sub_with_range ~first:1 ~len:4 "ahaha" in
  let hahb = String.sub_with_range ~first:0 ~len:4 "hahbdfdf" in
  eqs asdf "asdf"; eqs ha "ha"; eqs h "h"; eqs a "a"; eqs aha "aha";
  eqs haha "haha"; eqs hahb "hahb";
  eq_bool (String.Sub.is_suffix ~affix:empty sempty) true;
  eq_bool (String.Sub.is_suffix ~affix:empty asdf) true;
  eq_bool (String.Sub.is_suffix ~affix:ha sempty) false;
  eq_bool (String.Sub.is_suffix ~affix:ha a) false;
  eq_bool (String.Sub.is_suffix ~affix:ha h) false;
  eq_bool (String.Sub.is_suffix ~affix:ha ah) false;
  eq_bool (String.Sub.is_suffix ~affix:ha ha) true;
  eq_bool (String.Sub.is_suffix ~affix:ha aha) true;
  eq_bool (String.Sub.is_suffix ~affix:ha haha) true;
  eq_bool (String.Sub.is_suffix ~affix:ha hahb) false;
  ()

let for_all = test "String.Sub.for_all" @@ fun () ->
  let empty = String.Sub.v ~start:3 ~stop:3 "asldfksaf" in
  let s123 = String.Sub.v ~start:2 ~stop:5 "sf123df" in
  let s412 = String.Sub.v "412" in
  let s142 = String.Sub.v ~start:3 "aaa142" in
  let s124 = String.Sub.v ~start:3 "aad124" in
  eqs empty ""; eqs s123 "123"; eqs s412 "412"; eqs s142 "142"; eqs s124 "124";
  eq_bool (String.Sub.for_all (fun _ -> false) empty) true;
  eq_bool (String.Sub.for_all (fun _ -> true) empty) true;
  eq_bool (String.Sub.for_all (fun c -> Char.to_int c < 0x34) s123) true;
  eq_bool (String.Sub.for_all (fun c -> Char.to_int c < 0x34) s412) false;
  eq_bool (String.Sub.for_all (fun c -> Char.to_int c < 0x34) s142) false;
  eq_bool (String.Sub.for_all (fun c -> Char.to_int c < 0x34) s124) false;
  ()

let exists = test "String.Sub.exists" @@ fun () ->
  let empty = String.Sub.v ~start:3 ~stop:3 "asldfksaf" in
  let s541 = String.sub_with_index_range ~first:1 ~last:3 "a541" in
  let s154 = String.sub_with_index_range ~first:1 "a154" in
  let s654 = String.sub_with_index_range ~last:2 "654adf" in
  eqs s541 "541"; eqs s154 "154"; eqs s654 "654";
  eq_bool (String.Sub.exists (fun _ -> false) empty) false;
  eq_bool (String.Sub.exists (fun _ -> true) empty) false;
  eq_bool (String.Sub.exists (fun c -> Char.to_int c < 0x34) s541) true;
  eq_bool (String.Sub.exists (fun c -> Char.to_int c < 0x34) s541) true;
  eq_bool (String.Sub.exists (fun c -> Char.to_int c < 0x34) s154) true;
  eq_bool (String.Sub.exists (fun c -> Char.to_int c < 0x34) s654) false;
  ()

let same_base = test "String.Sub.same_base" @@ fun () ->
  let abcd = "abcd" in
  let a = String.sub_with_index_range ~first:0 ~last:0 abcd in
  let ab = String.sub_with_index_range ~first:0 ~last:1 abcd in
  let abce = String.sub_with_index_range ~first:0 ~last:1 "abce" in
  eq_bool (String.Sub.same_base a ab) true;
  eq_bool (String.Sub.same_base ab a) true;
  eq_bool (String.Sub.same_base abce a) false;
  eq_bool (String.Sub.same_base abce a) false;
  ()

let equal_bytes = test "String.Sub.equal_bytes" @@ fun () ->
  let a = String.sub_with_index_range ~first:0 ~last:0 "abcd" in
  let ab = String.sub_with_index_range ~first:0 ~last:1 "abcd" in
  let ab' = String.sub_with_index_range ~first:2 ~last:3 "cdab" in
  let cd = String.sub_with_index_range ~first:2 ~last:3 "abcd" in
  let empty = String.Sub.v ~start:4 ~stop:4 "abcd" in
  eq_bool (String.Sub.equal_bytes empty empty) true;
  eq_bool (String.Sub.equal_bytes empty a) false;
  eq_bool (String.Sub.equal_bytes a empty) false;
  eq_bool (String.Sub.equal_bytes a a) true;
  eq_bool (String.Sub.equal_bytes ab ab') true;
  eq_bool (String.Sub.equal_bytes cd ab) false;
  ()

let compare_bytes = test "String.Sub.compare_bytes" @@ fun () ->
  let empty = String.Sub.v "" in
  let ab = String.sub_with_index_range ~first:0 ~last:1 "abcd" in
  let ab' = String.sub_with_index_range ~first:2 ~last:3 "cdab" in
  let abc = String.Sub.v ~start:3 ~stop:5 "adabcdd"  in
  eq_int (String.Sub.compare_bytes empty ab) (-1);
  eq_int (String.Sub.compare_bytes empty empty) (0);
  eq_int (String.Sub.compare_bytes ab ab') (0);
  eq_int (String.Sub.compare_bytes ab empty) (1);
  eq_int (String.Sub.compare_bytes ab abc) (-1);
  ()

let equal = test "String.Sub.equal" @@ fun () ->
  app_invalid ~pp:pp_bool (String.Sub.(equal (v "b"))) (String.Sub.v "a");
  let base = "abcd" in
  let a = String.Sub.v ~start:0 ~stop:1 base in
  let empty = String.Sub.v ~start:4 ~stop:4 base in
  let ab = String.sub_with_index_range ~first:0 ~last:1 base in
  let cd = String.sub_with_index_range ~first:2 ~last:3 base in
  eq_bool (String.Sub.equal empty empty) true;
  eq_bool (String.Sub.equal empty a) false;
  eq_bool (String.Sub.equal a empty) false;
  eq_bool (String.Sub.equal a a) true;
  eq_bool (String.Sub.equal ab ab) true;
  eq_bool (String.Sub.equal cd ab) false;
  eq_bool (String.Sub.equal ab cd) false;
  eq_bool (String.Sub.equal cd cd) true;
  ()

let compare = test "String.Sub.compare" @@ fun () ->
  app_invalid ~pp:pp_bool (String.Sub.(equal (v "b"))) (String.Sub.v "a");
  let base = "abcd" in
  let a = String.Sub.v ~start:0 ~stop:1 base in
  let empty = String.Sub.v ~start:4 ~stop:4 base in
  let ab = String.sub_with_index_range ~first:0 ~last:1 base in
  let cd = String.sub_with_index_range ~first:2 ~last:3 base in
  eq_int (String.Sub.compare empty empty) 0;
  eq_int (String.Sub.compare empty a) 1;
  eq_int (String.Sub.compare a empty) (-1);
  eq_int (String.Sub.compare a a) 0;
  eq_int (String.Sub.compare ab ab) 0;
  eq_int (String.Sub.compare cd ab) 1;
  eq_int (String.Sub.compare ab cd) (-1);
  eq_int (String.Sub.compare cd cd) 0;
  ()

(* Extracting substrings *)

let with_range = test "String.Sub.with_range" @@ fun () ->
  let invalid ?first ?len s =
    app_invalid ~pp:String.Sub.pp (String.Sub.with_range ?first ?len) s
  in
  let empty_pos ?first ?len s pos =
    empty_pos (String.Sub.with_range ?first ?len s) pos
  in
  let base = "00abc1234" in
  let abc = String.sub ~start:2 ~stop:5 base in
  let a = String.sub ~start:2 ~stop:3 base in
  let empty = String.sub ~start:2 ~stop:2 base in
  empty_pos empty ~first:1 ~len:0 2;
  empty_pos empty ~first:1 ~len:0 2;
  empty_pos empty ~first:0 ~len:1 2;
  empty_pos empty ~first:(-1) ~len:1 2;
  invalid empty ~first:0 ~len:(-1);
  eqs (String.Sub.with_range a ~first:0 ~len:0) "";
  eqs (String.Sub.with_range a ~first:1 ~len:0) "";
  empty_pos a ~first:1 ~len:1 3;
  empty_pos a ~first:(-1) ~len:1 2;
  eqs (String.Sub.with_range ~first:1 abc) "bc";
  eqs (String.Sub.with_range ~first:2 abc) "c";
  eqs (String.Sub.with_range ~first:3 abc) "";
  empty_pos ~first:4 abc 5;
  eqs (String.Sub.with_range abc ~first:0 ~len:0) "";
  eqs (String.Sub.with_range abc ~first:0 ~len:1) "a";
  eqs (String.Sub.with_range abc ~first:0 ~len:2) "ab";
  eqs (String.Sub.with_range abc ~first:0 ~len:4) "abc";
  eqs (String.Sub.with_range abc ~first:1 ~len:0) "";
  eqs (String.Sub.with_range abc ~first:1 ~len:1) "b";
  eqs (String.Sub.with_range abc ~first:1 ~len:2) "bc";
  eqs (String.Sub.with_range abc ~first:1 ~len:3) "bc";
  eqs (String.Sub.with_range abc ~first:2 ~len:0) "";
  eqs (String.Sub.with_range abc ~first:2 ~len:1) "c";
  eqs (String.Sub.with_range abc ~first:2 ~len:2) "c";
  eqs (String.Sub.with_range abc ~first:3 ~len:0) "";
  eqs (String.Sub.with_range abc ~first:1 ~len:4) "bc";
  empty_pos abc ~first:(-1) ~len:1 2;
  ()

let with_index_range = test "String.Sub.with_index_range" @@ fun () ->
 let empty_pos ?first ?last s pos =
    empty_pos (String.Sub.with_index_range ?first ?last s) pos
  in
  let base = "00abc1234" in
  let abc = String.sub ~start:2 ~stop:5 base in
  let a = String.sub ~start:2 ~stop:3 base in
  let empty = String.sub ~start:2 ~stop:2 base in
  empty_pos empty 2;
  empty_pos empty ~first:0 ~last:0 2;
  empty_pos empty ~first:1 ~last:0 2;
  empty_pos empty ~first:0 ~last:1 2;
  empty_pos empty ~first:(-1) ~last:1 2;
  empty_pos empty ~first:0 ~last:(-1) 2;
  eqs (String.Sub.with_index_range ~first:0 ~last:2 a) "a";
  empty_pos a ~first:0 ~last:(-1) 2;
  eqs (String.Sub.with_index_range ~first:0 ~last:2 a) "a";
  eqs (String.Sub.with_index_range ~first:(-1) ~last:0 a) "a";
  eqs (String.Sub.with_index_range ~first:1 abc) "bc";
  eqs (String.Sub.with_index_range ~first:2 abc) "c";
  empty_pos ~first:3 abc 5;
  empty_pos ~first:4 abc 5;
  eqs (String.Sub.with_index_range abc ~first:0 ~last:0) "a";
  eqs (String.Sub.with_index_range abc ~first:0 ~last:1) "ab";
  eqs (String.Sub.with_index_range abc ~first:0 ~last:3) "abc";
  eqs (String.Sub.with_index_range abc ~first:1 ~last:1) "b";
  eqs (String.Sub.with_index_range abc ~first:1 ~last:2) "bc";
  empty_pos abc ~first:1 ~last:0 3;
  eqs (String.Sub.with_index_range abc ~first:1 ~last:3) "bc";
  eqs (String.Sub.with_index_range abc ~first:2 ~last:2) "c";
  empty_pos abc ~first:2 ~last:0 4;
  empty_pos abc ~first:2 ~last:1 4;
  eqs (String.Sub.with_index_range abc ~first:2 ~last:3) "c";
  empty_pos abc ~first:3 ~last:0 5;
  empty_pos abc ~first:3 ~last:1 5;
  empty_pos abc ~first:3 ~last:2 5;
  empty_pos abc ~first:3 ~last:3 5;
  eqs (String.Sub.with_index_range abc ~first:(-1) ~last:0) "a";
  ()

let span = test "String.Sub.{span,take,drop}" @@ fun () ->
  let eq_pair (l0, r0) (l1, r1) = String.Sub.(equal l0 l1 && equal r0 r1) in
  let eq_pair = eq ~eq:eq_pair ~pp:pp_pair in
  let eq ?(rev = false) ?min ?max ?sat s (sl, sr as spec) =
    let (l, r as pair) = String.Sub.span ~rev ?min ?max ?sat s in
    let t = String.Sub.take ~rev ?min ?max ?sat s in
    let d = String.Sub.drop ~rev ?min ?max ?sat s in
    eq_pair pair spec;
    eq_sub t (if rev then sr else sl);
    eq_sub d (if rev then sl else sr);
  in
  let invalid ?rev ?min ?max ?sat s =
    app_invalid ~pp:pp_pair (String.Sub.span ?rev ?min ?max ?sat) s
  in
  let base = "0ab cd0" in
  let empty = String.sub ~start:3 ~stop:3 base in
  let ab_cd = String.sub ~start:1 ~stop:6 base in
  let ab = String.sub ~start:1 ~stop:3 base in
  let _cd = String.sub ~start:3 ~stop:6 base in
  let cd = String.sub ~start:4 ~stop:6 base in
  let ab_ = String.sub ~start:1 ~stop:4 base in
  let a = String.sub ~start:1 ~stop:2 base in
  let b_cd = String.sub ~start:2 ~stop:6 base in
  let b = String.sub ~start:2 ~stop:3 base in
  let d = String.sub ~start:5 ~stop:6 base in
  let ab_c = String.sub ~start:1 ~stop:5 base in
  eq ~rev:false ~min:1 ~max:0 ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true ~min:1 ~max:0 ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~sat:Char.Ascii.is_white ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~sat:Char.Ascii.is_letter ab_cd (ab, _cd);
  eq ~max:1 ~sat:Char.Ascii.is_letter ab_cd (a, b_cd);
  eq ~max:0 ~sat:Char.Ascii.is_letter ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true ~sat:Char.Ascii.is_white ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:true ~sat:Char.Ascii.is_letter ab_cd (ab_, cd);
  eq ~rev:true ~max:1 ~sat:Char.Ascii.is_letter ab_cd (ab_c, d);
  eq ~rev:true ~max:0 ~sat:Char.Ascii.is_letter ab_cd (ab_cd,
                                                       String.Sub.stop ab_cd);
  eq ~sat:Char.Ascii.is_letter ab (ab, String.Sub.stop ab);
  eq ~max:1 ~sat:Char.Ascii.is_letter ab (a, b);
  eq ~sat:Char.Ascii.is_letter b (b, empty);
  eq ~rev:true ~max:1 ~sat:Char.Ascii.is_letter ab (a, b);
  eq ~max:1 ~sat:Char.Ascii.is_white ab (String.Sub.start ab, ab);
  eq ~rev:true ~sat:Char.Ascii.is_white empty (empty, empty);
  eq ~sat:Char.Ascii.is_white empty (empty, empty);
  invalid ~rev:false ~min:(-1) empty;
  invalid ~rev:true  ~min:(-1) empty;
  invalid ~rev:false ~max:(-1) empty;
  invalid ~rev:true ~max:(-1) empty;
  eq ~rev:false empty (empty,empty);
  eq ~rev:true  empty (empty,empty);
  eq ~rev:false ~min:0 ~max:0 empty (empty,empty);
  eq ~rev:true  ~min:0 ~max:0 empty (empty,empty);
  eq ~rev:false ~min:1 ~max:0 empty (empty,empty);
  eq ~rev:true  ~min:1 ~max:0 empty (empty,empty);
  eq ~rev:false ~max:0 ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true  ~max:0 ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:false ~max:2 ab_cd (ab, _cd);
  eq ~rev:true  ~max:2 ab_cd (ab_, cd);
  eq ~rev:false ~min:6 ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true  ~min:6 ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:false ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:true  ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:false ~max:30 ab_cd (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:true  ~max:30 ab_cd (String.Sub.start ab_cd, ab_cd);
  eq ~rev:false ~sat:Char.Ascii.is_white ab_cd (String.Sub.start ab_cd,ab_cd);
  eq ~rev:true  ~sat:Char.Ascii.is_white ab_cd (ab_cd,String.Sub.stop ab_cd);
  eq ~rev:false ~sat:Char.Ascii.is_letter ab_cd (ab, _cd);
  eq ~rev:true  ~sat:Char.Ascii.is_letter ab_cd (ab_, cd);
  eq ~rev:false ~sat:Char.Ascii.is_letter ~max:0 ab_cd (String.Sub.start ab_cd,
                                                        ab_cd);
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~max:0 ab_cd (ab_cd,
                                                        String.Sub.stop ab_cd);
  eq ~rev:false ~sat:Char.Ascii.is_letter ~max:1 ab_cd (a, b_cd);
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~max:1 ab_cd (ab_c, d);
  eq ~rev:false ~sat:Char.Ascii.is_letter ~min:2 ~max:1 ab_cd
    (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~min:2 ~max:1 ab_cd
    (ab_cd, String.Sub.stop ab_cd);
  eq ~rev:false ~sat:Char.Ascii.is_letter ~min:3 ab_cd
    (String.Sub.start ab_cd, ab_cd);
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~min:3 ab_cd
    (ab_cd, String.Sub.stop ab_cd);
  ()

let trim = test "String.Sub.trim" @@ fun () ->
  let drop_a c = c = 'a' in
  let base = "00aaaabcdaaaa00" in
  let aaaabcdaaaa = String.sub ~start:2 ~stop:13 base in
  let aaaabcd = String.sub ~start:2 ~stop:9 base in
  let bcdaaaa = String.sub ~start:6 ~stop:13 base in
  let aaaa = String.sub ~start:2 ~stop:6 base in
  eqs (String.Sub.trim (String.sub "\t abcd \r ")) "abcd";
  eqs (String.Sub.trim aaaabcdaaaa) "aaaabcdaaaa";
  eqs (String.Sub.trim ~drop:drop_a aaaabcdaaaa) "bcd";
  eqs (String.Sub.trim ~drop:drop_a aaaabcd) "bcd";
  eqs (String.Sub.trim ~drop:drop_a bcdaaaa) "bcd";
  empty_pos (String.Sub.trim ~drop:drop_a aaaa) 4;
  empty_pos (String.Sub.trim (String.sub "     ")) 2;
  ()

let cut = test "String.Sub.cut" @@ fun () ->
  let ppp = pp_option pp_pair in
  let eqo = eqs_pair_opt in
  let s s =
    String.sub ~start:1 ~stop:(1 + (String.length s)) (strf "\x00%s\x00" s)
  in
  let cut ?rev ~sep str = String.Sub.cut ?rev ~sep:(s sep) (s str) in
  app_invalid ~pp:ppp (cut ~sep:"") "";
  app_invalid ~pp:ppp (cut ~sep:"") "123";
  eqo (cut "," "")  None;
  eqo (cut "," ",") (Some ("", ""));
  eqo (cut "," ",,") (Some ("", ","));
  eqo (cut "," ",,,") (Some ("", ",,"));
  eqo (cut "," "123") None;
  eqo (cut "," ",123") (Some ("", "123"));
  eqo (cut "," "123,") (Some ("123", ""));
  eqo (cut "," "1,2,3") (Some ("1", "2,3"));
  eqo (cut "," " 1,2,3") (Some (" 1", "2,3"));
  eqo (cut "<>" "") None;
  eqo (cut "<>" "<>") (Some ("", ""));
  eqo (cut "<>" "<><>") (Some ("", "<>"));
  eqo (cut "<>" "<><><>") (Some ("", "<><>"));
  eqo (cut ~rev:true ~sep:"<>" "1") None;
  eqo (cut "<>" "123") None;
  eqo (cut "<>" "<>123") (Some ("", "123"));
  eqo (cut "<>" "123<>") (Some ("123", ""));
  eqo (cut "<>" "1<>2<>3") (Some ("1", "2<>3"));
  eqo (cut "<>" " 1<>2<>3") (Some (" 1", "2<>3"));
  eqo (cut "<>" ">>><>>>><>>>><>>>>") (Some (">>>", ">>><>>>><>>>>"));
  eqo (cut "<->" "<->>->") (Some ("", ">->"));
  eqo (cut ~rev:true ~sep:"<->" "<-") None;
  eqo (cut "aa" "aa") (Some ("", ""));
  eqo (cut "aa" "aaa") (Some ("", "a"));
  eqo (cut "aa" "aaaa") (Some ("", "aa"));
  eqo (cut "aa" "aaaaa") (Some ("", "aaa";));
  eqo (cut "aa" "aaaaaa") (Some ("", "aaaa"));
  eqo (cut ~sep:"ab" "faaaa") None;
  eqo (String.Sub.cut ~sep:(String.sub "/") (String.sub ~start:2 "a/b/c"))
    (Some ("b", "c"));
  let rev = true in
  app_invalid ~pp:ppp (cut ~rev ~sep:"") "";
  app_invalid ~pp:ppp (cut ~rev ~sep:"") "123";
  eqo (cut ~rev ~sep:"," "") None;
  eqo (cut ~rev ~sep:"," ",") (Some ("", ""));
  eqo (cut ~rev ~sep:"," ",,") (Some (",", ""));
  eqo (cut ~rev ~sep:"," ",,,") (Some (",,", ""));
  eqo (cut ~rev ~sep:"," "123") None;
  eqo (cut ~rev ~sep:"," ",123") (Some ("", "123"));
  eqo (cut ~rev ~sep:"," "123,") (Some ("123", ""));
  eqo (cut ~rev ~sep:"," "1,2,3") (Some ("1,2", "3"));
  eqo (cut ~rev ~sep:"," "1,2,3 ") (Some ("1,2", "3 "));
  eqo (cut ~rev ~sep:"<>" "") None;
  eqo (cut ~rev ~sep:"<>" "<>") (Some ("", ""));
  eqo (cut ~rev ~sep:"<>" "<><>") (Some ("<>", ""));
  eqo (cut ~rev ~sep:"<>" "<><><>") (Some ("<><>", ""));
  eqo (cut ~rev ~sep:"<>" "1") None;
  eqo (cut ~rev ~sep:"<>" "123") None;
  eqo (cut ~rev ~sep:"<>" "<>123") (Some ("", "123"));
  eqo (cut ~rev ~sep:"<>" "123<>") (Some ("123", ""));
  eqo (cut ~rev ~sep:"<>" "1<>2<>3") (Some ("1<>2", "3"));
  eqo (cut ~rev ~sep:"<>" "1<>2<>3 ") (Some ("1<>2", "3 "));
  eqo (cut ~rev ~sep:"<>" ">>><>>>><>>>><>>>>")
    (Some (">>><>>>><>>>>", ">>>"));
  eqo (cut ~rev ~sep:"<->" "<->>->") (Some ("", ">->"));
  eqo (cut ~rev ~sep:"<->" "<-") None;
  eqo (cut ~rev ~sep:"aa" "aa") (Some ("", ""));
  eqo (cut ~rev ~sep:"aa" "aaa") (Some ("a", ""));
  eqo (cut ~rev ~sep:"aa" "aaaa") (Some ("aa", ""));
  eqo (cut ~rev ~sep:"aa" "aaaaa") (Some ("aaa", "";));
  eqo (cut ~rev ~sep:"aa" "aaaaaa") (Some ("aaaa", ""));
  eqo (cut ~rev ~sep:"ab" "afaaaa") None;
  eqo (String.Sub.cut ~sep:(String.sub "/") (String.sub ~stop:3 "a/b/c"))
    (Some ("a", "b"));
  ()

let cuts = test "String.Sub.cuts" @@ fun () ->
  let ppl = pp_list String.Sub.dump in
  let eql subs l =
    let subs = List.map String.Sub.to_string subs in
    eq_list ~eq:String.equal ~pp:String.dump subs l
  in
  let s s =
    String.sub ~start:1 ~stop:(1 + (String.length s)) (strf "\x00%s\x00" s)
  in
  let cuts ?rev ?empty ~sep str =
    String.Sub.cuts ?rev ?empty ~sep:(s sep) (s str)
  in
  app_invalid ~pp:ppl (cuts ~sep:"") "";
  app_invalid ~pp:ppl (cuts ~sep:"") "123";
  eql (cuts ~empty:true  ~sep:"," "") [""];
  eql (cuts ~empty:false ~sep:"," "") [];
  eql (cuts ~empty:true  ~sep:"," ",") [""; ""];
  eql (cuts ~empty:false ~sep:"," ",") [];
  eql (cuts ~empty:true  ~sep:"," ",,") [""; ""; ""];
  eql (cuts ~empty:false ~sep:"," ",,") [];
  eql (cuts ~empty:true  ~sep:"," ",,,") [""; ""; ""; ""];
  eql (cuts ~empty:false ~sep:"," ",,,") [];
  eql (cuts ~empty:true  ~sep:"," "123") ["123"];
  eql (cuts ~empty:false ~sep:"," "123") ["123"];
  eql (cuts ~empty:true  ~sep:"," ",123") [""; "123"];
  eql (cuts ~empty:false ~sep:"," ",123") ["123"];
  eql (cuts ~empty:true  ~sep:"," "123,") ["123"; ""];
  eql (cuts ~empty:false ~sep:"," "123,") ["123";];
  eql (cuts ~empty:true  ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (cuts ~empty:false ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (cuts ~empty:true  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (cuts ~empty:false  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (cuts ~empty:true ~sep:"," ",1,2,,3,") [""; "1"; "2"; ""; "3"; ""];
  eql (cuts ~empty:false ~sep:"," ",1,2,,3,") ["1"; "2"; "3";];
  eql (cuts ~empty:true ~sep:"," ", 1, 2,, 3,")
    [""; " 1"; " 2"; ""; " 3"; ""];
  eql (cuts ~empty:false ~sep:"," ", 1, 2,, 3,") [" 1"; " 2";" 3";];
  eql (cuts ~empty:true ~sep:"<>" "") [""];
  eql (cuts ~empty:false ~sep:"<>" "") [];
  eql (cuts ~empty:true ~sep:"<>" "<>") [""; ""];
  eql (cuts ~empty:false ~sep:"<>" "<>") [];
  eql (cuts ~empty:true ~sep:"<>" "<><>") [""; ""; ""];
  eql (cuts ~empty:false ~sep:"<>" "<><>") [];
  eql (cuts ~empty:true ~sep:"<>" "<><><>") [""; ""; ""; ""];
  eql (cuts ~empty:false ~sep:"<>" "<><><>") [];
  eql (cuts ~empty:true ~sep:"<>" "123") [ "123" ];
  eql (cuts ~empty:false ~sep:"<>" "123") [ "123" ];
  eql (cuts ~empty:true ~sep:"<>" "<>123") [""; "123"];
  eql (cuts ~empty:false ~sep:"<>" "<>123") ["123"];
  eql (cuts ~empty:true ~sep:"<>" "123<>") ["123"; ""];
  eql (cuts ~empty:false ~sep:"<>" "123<>") ["123"];
  eql (cuts ~empty:true ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (cuts ~empty:false ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (cuts ~empty:true ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (cuts ~empty:false ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (cuts ~empty:true ~sep:"<>" "<>1<>2<><>3<>")
    [""; "1"; "2"; ""; "3"; ""];
  eql (cuts ~empty:false ~sep:"<>" "<>1<>2<><>3<>") ["1"; "2";"3";];
  eql (cuts ~empty:true ~sep:"<>" "<> 1<> 2<><> 3<>")
    [""; " 1"; " 2"; ""; " 3";""];
  eql (cuts ~empty:false ~sep:"<>" "<> 1<> 2<><> 3<>")[" 1"; " 2"; " 3"];
  eql (cuts ~empty:true ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (cuts ~empty:false ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (cuts ~empty:true ~sep:"<->" "<->>->") [""; ">->"];
  eql (cuts ~empty:false ~sep:"<->" "<->>->") [">->"];
  eql (cuts ~empty:true ~sep:"aa" "aa") [""; ""];
  eql (cuts ~empty:false ~sep:"aa" "aa") [];
  eql (cuts ~empty:true ~sep:"aa" "aaa") [""; "a"];
  eql (cuts ~empty:false ~sep:"aa" "aaa") ["a"];
  eql (cuts ~empty:true ~sep:"aa" "aaaa") [""; ""; ""];
  eql (cuts ~empty:false ~sep:"aa" "aaaa") [];
  eql (cuts ~empty:true ~sep:"aa" "aaaaa") [""; ""; "a"];
  eql (cuts ~empty:false ~sep:"aa" "aaaaa") ["a"];
  eql (cuts ~empty:true ~sep:"aa" "aaaaaa") [""; ""; ""; ""];
  eql (cuts ~empty:false ~sep:"aa" "aaaaaa") [];
  let rev = true in
  app_invalid ~pp:ppl (cuts ~rev ~sep:"") "";
  app_invalid ~pp:ppl (cuts ~rev ~sep:"") "123";
  eql (cuts ~rev ~empty:true ~sep:"," "") [""];
  eql (cuts ~rev ~empty:false ~sep:"," "") [];
  eql (cuts ~rev ~empty:true ~sep:"," ",") [""; ""];
  eql (cuts ~rev ~empty:false ~sep:"," ",") [];
  eql (cuts ~rev ~empty:true ~sep:"," ",,") [""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"," ",,") [];
  eql (cuts ~rev ~empty:true ~sep:"," ",,,") [""; ""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"," ",,,") [];
  eql (cuts ~rev ~empty:true ~sep:"," "123") ["123"];
  eql (cuts ~rev ~empty:false ~sep:"," "123") ["123"];
  eql (cuts ~rev ~empty:true ~sep:"," ",123") [""; "123"];
  eql (cuts ~rev ~empty:false ~sep:"," ",123") ["123"];
  eql (cuts ~rev ~empty:true ~sep:"," "123,") ["123"; ""];
  eql (cuts ~rev ~empty:false ~sep:"," "123,") ["123";];
  eql (cuts ~rev ~empty:true ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:false ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:true ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (cuts ~rev ~empty:false ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (cuts ~rev ~empty:true ~sep:"," ",1,2,,3,")
    [""; "1"; "2"; ""; "3"; ""];
  eql (cuts ~rev ~empty:false ~sep:"," ",1,2,,3,") ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:true ~sep:"," ", 1, 2,, 3,")
    [""; " 1"; " 2"; ""; " 3"; ""];
  eql (cuts ~rev ~empty:false ~sep:"," ", 1, 2,, 3,") [" 1"; " 2"; " 3"];
  eql (cuts ~rev ~empty:true ~sep:"<>" "") [""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "") [];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<>") [""; ""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<>") [];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<><>") [""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<><>") [];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<><><>") [""; ""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<><><>") [];
  eql (cuts ~rev ~empty:true ~sep:"<>" "123") [ "123" ];
  eql (cuts ~rev ~empty:false ~sep:"<>" "123") [ "123" ];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<>123") [""; "123"];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<>123") ["123"];
  eql (cuts ~rev ~empty:true ~sep:"<>" "123<>") ["123"; ""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "123<>") ["123";];
  eql (cuts ~rev ~empty:true ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:false ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:true ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (cuts ~rev ~empty:false ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<>1<>2<><>3<>")
    [""; "1"; "2"; ""; "3"; ""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<>1<>2<><>3<>")
    ["1"; "2"; "3"];
  eql (cuts ~rev ~empty:true ~sep:"<>" "<> 1<> 2<><> 3<>")
                                  [""; " 1"; " 2"; ""; " 3";""];
  eql (cuts ~rev ~empty:false ~sep:"<>" "<> 1<> 2<><> 3<>")
                                  [" 1"; " 2"; " 3";];
  eql (cuts ~rev ~empty:true ~sep:"<>" ">>><>>>><>>>><>>>>")
                                  [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (cuts ~rev ~empty:false ~sep:"<>" ">>><>>>><>>>><>>>>")
                                  [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (cuts ~rev ~empty:true ~sep:"<->" "<->>->") [""; ">->"];
  eql (cuts ~rev ~empty:false ~sep:"<->" "<->>->") [">->"];
  eql (cuts ~rev ~empty:true ~sep:"aa" "aa") [""; ""];
  eql (cuts ~rev ~empty:false ~sep:"aa" "aa") [];
  eql (cuts ~rev ~empty:true ~sep:"aa" "aaa") ["a"; ""];
  eql (cuts ~rev ~empty:false ~sep:"aa" "aaa") ["a"];
  eql (cuts ~rev ~empty:true ~sep:"aa" "aaaa") [""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"aa" "aaaa") [];
  eql (cuts ~rev ~empty:true ~sep:"aa" "aaaaa") ["a"; ""; "";];
  eql (cuts ~rev ~empty:false ~sep:"aa" "aaaaa") ["a";];
  eql (cuts ~rev ~empty:true ~sep:"aa" "aaaaaa") [""; ""; ""; ""];
  eql (cuts ~rev ~empty:false ~sep:"aa" "aaaaaa") [];
  ()

let fields = test "String.Sub.fields" @@ fun () ->
  let eql subs l =
    let subs = List.map String.Sub.to_string subs in
    eq_list ~eq:String.equal ~pp:String.dump subs l
  in
  let s s =
    String.sub ~start:1 ~stop:(1 + (String.length s)) (strf "\x00%s\x00" s)
  in
  let fields ?empty ?is_sep str = String.Sub.fields ?empty ?is_sep (s str) in
  let is_a c = c = 'a' in
  eql (fields ~empty:true "a") ["a"];
  eql (fields ~empty:false "a") ["a"];
  eql (fields ~empty:true "abc") ["abc"];
  eql (fields ~empty:false "abc") ["abc"];
  eql (fields ~empty:true ~is_sep:is_a "bcdf") ["bcdf"];
  eql (fields ~empty:false ~is_sep:is_a "bcdf") ["bcdf"];
  eql (fields ~empty:true "") [""];
  eql (fields ~empty:false "") [];
  eql (fields ~empty:true "\n\r") ["";"";""];
  eql (fields ~empty:false "\n\r") [];
  eql (fields ~empty:true " \n\rabc") ["";"";"";"abc"];
  eql (fields ~empty:false " \n\rabc") ["abc"];
  eql (fields ~empty:true " \n\racd de") ["";"";"";"acd";"de"];
  eql (fields ~empty:false " \n\racd de") ["acd";"de"];
  eql (fields ~empty:true " \n\racd de ") ["";"";"";"acd";"de";""];
  eql (fields ~empty:false " \n\racd de ") ["acd";"de"];
  eql (fields ~empty:true "\n\racd\nde \r") ["";"";"acd";"de";"";""];
  eql (fields ~empty:false "\n\racd\nde \r") ["acd";"de"];
  eql (fields ~empty:true ~is_sep:is_a "") [""];
  eql (fields ~empty:false ~is_sep:is_a "") [];
  eql (fields ~empty:true ~is_sep:is_a "abaac aaa")
    ["";"b";"";"c ";"";"";""];
  eql (fields ~empty:false ~is_sep:is_a "abaac aaa") ["b"; "c "];
  eql (fields ~empty:true ~is_sep:is_a "aaaa") ["";"";"";"";""];
  eql (fields ~empty:false ~is_sep:is_a "aaaa") [];
  eql (fields ~empty:true ~is_sep:is_a "aaaa ") ["";"";"";"";" "];
  eql (fields ~empty:false ~is_sep:is_a "aaaa ") [" "];
  eql (fields ~empty:true ~is_sep:is_a "aaaab") ["";"";"";"";"b"];
  eql (fields ~empty:false ~is_sep:is_a "aaaab") ["b"];
  eql (fields ~empty:true ~is_sep:is_a "baaaa") ["b";"";"";"";""];
  eql (fields ~empty:false ~is_sep:is_a "baaaa") ["b"];
  eql (fields ~empty:true ~is_sep:is_a "abaaaa") ["";"b";"";"";"";""];
  eql (fields ~empty:false ~is_sep:is_a "abaaaa") ["b"];
  eql (fields ~empty:true ~is_sep:is_a "aba") ["";"b";""];
  eql (fields ~empty:false ~is_sep:is_a "aba") ["b"];
  eql (fields ~empty:false "tokenize me please")
    ["tokenize"; "me"; "please"];
  ()

(* Traversing *)

let find = test "String.Sub.find" @@ fun () ->
  let abcbd = "abcbd" in
  let empty = String.sub ~start:3 ~stop:3 abcbd in
  let a = String.sub ~start:0 ~stop:1 abcbd in
  let ab = String.sub ~start:0 ~stop:2 abcbd in
  let c = String.sub ~start:2 ~stop:3 abcbd in
  let b0 = String.sub ~start:1 ~stop:2 abcbd in
  let b1 = String.sub ~start:3 ~stop:4 abcbd in
  let abcbd = String.sub abcbd in
  let eq = eq_option ~eq:String.Sub.equal ~pp:String.Sub.dump_raw in
  eq (String.Sub.find (fun c -> c = 'b') empty) None;
  eq (String.Sub.find ~rev:true (fun c -> c = 'b') empty) None;
  eq (String.Sub.find (fun c -> c = 'b') a) None;
  eq (String.Sub.find ~rev:true (fun c -> c = 'b') a) None;
  eq (String.Sub.find (fun c -> c = 'b') c) None;
  eq (String.Sub.find ~rev:true (fun c -> c = 'b') c) None;
  eq (String.Sub.find (fun c -> c = 'b') abcbd) (Some b0);
  eq (String.Sub.find ~rev:true (fun c -> c = 'b') abcbd) (Some b1);
  eq (String.Sub.find (fun c -> c = 'b') ab) (Some b0);
  eq (String.Sub.find ~rev:true (fun c -> c = 'b') ab) (Some b0);
  ()

let find_sub = test "String.Sub.find_sub" @@ fun () ->
  let abcbd = "abcbd" in
  let empty = String.sub ~start:3 ~stop:3 abcbd in
  let ab = String.sub ~start:0 ~stop:2 abcbd in
  let b0 = String.sub ~start:1 ~stop:2 abcbd in
  let b1 = String.sub ~start:3 ~stop:4 abcbd in
  let abcbd = String.sub abcbd in
  let eq = eq_option ~eq:String.Sub.equal ~pp:String.Sub.dump_raw in
  eq (String.Sub.find_sub ~sub:ab empty) None;
  eq (String.Sub.find_sub ~rev:true ~sub:ab empty) None;
  eq (String.Sub.find_sub ~sub:(String.sub "") empty) (Some empty);
  eq (String.Sub.find_sub ~rev:true ~sub:(String.sub "") empty) (Some empty);
  eq (String.Sub.find_sub ~sub:ab abcbd) (Some ab);
  eq (String.Sub.find_sub ~rev:true ~sub:ab abcbd) (Some ab);
  eq (String.Sub.find_sub ~sub:empty abcbd) (Some (String.Sub.start abcbd));
  eq (String.Sub.find_sub ~rev:true ~sub:empty abcbd)
    (Some (String.Sub.stop abcbd));
  eq (String.Sub.find_sub ~sub:(String.sub "b") abcbd) (Some b0);
  eq (String.Sub.find_sub ~rev:true ~sub:(String.sub "b") abcbd) (Some b1);
  eq (String.Sub.find_sub ~sub:b1 ab) (Some b0);
  eq (String.Sub.find_sub ~rev:true ~sub:b1 ab) (Some b0);
  ()

let map = test "String.Sub.map[i]" @@ fun () ->
  let next_letter c = Char.(of_byte @@ to_int c + 1) in
  let base = "i34abcdbbb" in
  let abcd = String.Sub.v ~start:3 ~stop:7 base in
  let empty = String.Sub.v ~start:2 ~stop:2 base in
  eqs (String.Sub.map (fun c -> fail "invoked"; c) empty) "";
  eqs (String.Sub.map (fun c -> c) abcd) "abcd";
  eqs (String.Sub.map next_letter abcd) "bcde";
  eq_str String.Sub.(base_string (map next_letter abcd)) "bcde";
  eqs (String.Sub.mapi (fun _ c -> fail "invoked"; c) empty) "";
  eqs (String.Sub.mapi (fun i c -> Char.(of_byte @@ to_int c + i)) abcd) "aceg";
  ()

let fold = test "String.Sub.fold_{left,right}" @@ fun () ->
  let empty = String.Sub.v ~start:2 ~stop:2 "ab" in
  let abc = String.Sub.v ~start:3 ~stop:6 "i34abcdbbb" in
  let eql = eq_list ~eq:(=) ~pp:pp_char in
  String.Sub.fold_left (fun _ _ -> fail "invoked") () empty;
  eql (String.Sub.fold_left (fun acc c -> c :: acc) [] empty) [];
  eql (String.Sub.fold_left (fun acc c -> c :: acc) [] abc) ['c';'b';'a'];
  String.Sub.fold_right (fun _ _ -> fail "invoked") empty ();
  eql (String.Sub.fold_right (fun c acc -> c :: acc) empty []) [];
  eql (String.Sub.fold_right (fun c acc -> c :: acc) abc []) ['a';'b';'c'];
  ()

let iter = test "String.Sub.iter[i]" @@ fun () ->
  let empty = String.Sub.v ~start:2 ~stop:2 "ab" in
  let abc = String.Sub.v ~start:3 ~stop:6 "i34abcdbbb" in
  String.Sub.iter (fun _ -> fail "invoked") empty;
  String.Sub.iteri (fun _ _ -> fail "invoked") empty;
  (let i = ref 0 in
   String.Sub.iter (fun c -> eq_char (String.Sub.get abc !i) c; incr i) abc);
  String.Sub.iteri (fun i c -> eq_char (String.Sub.get abc i) c) abc;
  ()

(* Suite *)

let suite = suite "Base String functions"
    [ misc;
      head;
      to_string;
      start;
      stop;
      tail;
      base;
      extend;
      reduce;
      extent;
      overlap;
      append;
      concat;
      is_empty;
      is_prefix;
      is_infix;
      is_suffix;
      for_all;
      exists;
      same_base;
      equal_bytes;
      compare_bytes;
      equal;
      compare;
      with_range;
      with_index_range;
      span;
      trim;
      cut;
      cuts;
      fields;
      find;
      find_sub;
      map;
      fold;
      iter; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
