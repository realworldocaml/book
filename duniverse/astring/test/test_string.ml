(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring

let pp_pair ppf (a, b) =
  Format.fprintf ppf "@[<1>(%a,%a)@]" String.dump a String.dump b

let misc = test "Misc. base functions" @@ fun () ->
  eq_str String.empty  "";
  app_invalid ~pp:pp_str (String.v ~len:(-1)) (fun i -> 'a');
  app_invalid ~pp:pp_str (String.v ~len:(Sys.max_string_length + 1))
    (fun i -> 'a');
  eq_int (String.length "") 0;
  eq_int (String.length "1") 1;
  eq_int (String.length "12") 2;
  eq_char (String.get "12" 0) '1';
  eq_char (String.get "12" 1) '2';
  app_invalid ~pp:pp_char (String.get "12") 3;
  app_invalid ~pp:pp_char (String.get "12") (-1);
  eq_int (String.get_byte "12" 0) 0x31;
  eq_int (String.get_byte "12" 1) 0x32;
  app_invalid ~pp:pp_int (String.get_byte "12") 3;
  app_invalid ~pp:pp_int (String.get_byte "12") (-1);
  eq_str (String.v ~len:3 (fun i -> Char.of_byte (0x30 + i))) "012";
  eq_str (String.v ~len:0 (fun i -> Char.of_byte (0x30 + i))) "";
  ()

let head = test "String.[get_]head" @@ fun () ->
  let eq_ochar = eq_option ~eq:(=) ~pp:pp_char in
  eq_ochar (String.head "") None;
  eq_ochar (String.head ~rev:true "") None;
  eq_ochar (String.head "bc") (Some 'b');
  eq_ochar (String.head ~rev:true "bc") (Some 'c');
  eq_char (String.get_head "bc") 'b';
  eq_char (String.get_head ~rev:true "bc") 'c';
  app_invalid ~pp:pp_char String.get_head "";
  ()

(* Appending strings *)

let append = test "String.append" @@ fun () ->
  let no_allocl s s' = eq_bool (s ^ s' == s) true in
  let no_allocr s s' = eq_bool (s ^ s' == s') true in
  no_allocl String.empty String.empty;
  no_allocr String.empty String.empty;
  no_allocl "bla" "";
  no_allocr "" "bli";
  eq_str (String.append "a" "") "a";
  eq_str (String.append "" "a") "a";
  eq_str (String.append "ab" "") "ab";
  eq_str (String.append "" "ab") "ab";
  eq_str (String.append "ab" "cd") "abcd";
  eq_str (String.append "cd" "ab") "cdab";
  ()

let concat = test "String.concat" @@ fun () ->
  let no_alloc ~sep s = eq_bool (String.concat ~sep [s] == s) true in
  no_alloc ~sep:"" "";
  no_alloc ~sep:"-" "";
  no_alloc ~sep:"" "abc";
  no_alloc ~sep:"-" "abc";
  eq_str (String.concat ~sep:"" []) "";
  eq_str (String.concat ~sep:"" [""]) "";
  eq_str (String.concat ~sep:"" ["";""]) "";
  eq_str (String.concat ~sep:"" ["a";"b";]) "ab";
  eq_str (String.concat ~sep:"" ["a";"b";"";"c"]) "abc";
  eq_str (String.concat ~sep:"-" []) "";
  eq_str (String.concat ~sep:"-" [""]) "";
  eq_str (String.concat ~sep:"-" ["a"]) "a";
  eq_str (String.concat ~sep:"-" ["a";""]) "a-";
  eq_str (String.concat ~sep:"-" ["";"a"]) "-a";
  eq_str (String.concat ~sep:"-" ["";"a";""]) "-a-";
  eq_str (String.concat ~sep:"-" ["a";"b";"c"]) "a-b-c";
  eq_str (String.concat ~sep:"--" ["a";"b";"c"]) "a--b--c";
  eq_str (String.concat ~sep:"ab" ["a";"b";"c"]) "aabbabc";
  eq_str (String.concat ["a";"b";""; "c"]) "abc";
  ()

(* Predicates *)

let is_empty = test "String.is_empty" @@ fun () ->
  eq_bool (String.is_empty "") true;
  eq_bool (String.is_empty "heyho") false;
  ()

let is_prefix = test "String.is_prefix" @@ fun () ->
  eq_bool (String.is_prefix ~affix:"" "") true;
  eq_bool (String.is_prefix ~affix:"" "habla") true;
  eq_bool (String.is_prefix ~affix:"ha" "") false;
  eq_bool (String.is_prefix ~affix:"ha" "h") false;
  eq_bool (String.is_prefix ~affix:"ha" "ha") true;
  eq_bool (String.is_prefix ~affix:"ha" "hab") true;
  eq_bool (String.is_prefix ~affix:"ha" "habla") true;
  eq_bool (String.is_prefix ~affix:"ha" "abla") false;
  ()

let is_infix = test "String.is_infix" @@ fun () ->
  eq_bool (String.is_infix ~affix:"" "") true;
  eq_bool (String.is_infix ~affix:"" "habla") true;
  eq_bool (String.is_infix ~affix:"ha" "") false;
  eq_bool (String.is_infix ~affix:"ha" "h") false;
  eq_bool (String.is_infix ~affix:"ha" "ha") true;
  eq_bool (String.is_infix ~affix:"ha" "hab") true;
  eq_bool (String.is_infix ~affix:"ha" "hub") false;
  eq_bool (String.is_infix ~affix:"ha" "hubhab") true;
  eq_bool (String.is_infix ~affix:"ha" "hubh") false;
  eq_bool (String.is_infix ~affix:"ha" "hubha") true;
  eq_bool (String.is_infix ~affix:"ha" "hubhb") false;
  eq_bool (String.is_infix ~affix:"ha" "abla") false;
  eq_bool (String.is_infix ~affix:"ha" "ablah") false;
  ()

let is_suffix = test "String.is_suffix" @@ fun () ->
  eq_bool (String.is_suffix ~affix:"" "") true;
  eq_bool (String.is_suffix ~affix:"" "adsf") true;
  eq_bool (String.is_suffix ~affix:"ha" "") false;
  eq_bool (String.is_suffix ~affix:"ha" "a") false;
  eq_bool (String.is_suffix ~affix:"ha" "h") false;
  eq_bool (String.is_suffix ~affix:"ha" "ah") false;
  eq_bool (String.is_suffix ~affix:"ha" "ha") true;
  eq_bool (String.is_suffix ~affix:"ha" "aha") true;
  eq_bool (String.is_suffix ~affix:"ha" "haha") true;
  eq_bool (String.is_suffix ~affix:"ha" "hahb") false;
  ()

let for_all = test "String.for_all" @@ fun () ->
  eq_bool (String.for_all (fun _ -> false) "") true;
  eq_bool (String.for_all (fun _ -> true) "") true;
  eq_bool (String.for_all (fun c -> Char.to_int c < 0x34) "123") true;
  eq_bool (String.for_all (fun c -> Char.to_int c < 0x34) "412") false;
  eq_bool (String.for_all (fun c -> Char.to_int c < 0x34) "142") false;
  eq_bool (String.for_all (fun c -> Char.to_int c < 0x34) "124") false;
  ()

let exists = test "String.exists" @@ fun () ->
  eq_bool (String.exists (fun _ -> false) "") false;
  eq_bool (String.exists (fun _ -> true) "") false;
  eq_bool (String.exists (fun c -> Char.to_int c < 0x34) "541") true;
  eq_bool (String.exists (fun c -> Char.to_int c < 0x34) "541") true;
  eq_bool (String.exists (fun c -> Char.to_int c < 0x34) "154") true;
  eq_bool (String.exists (fun c -> Char.to_int c < 0x34) "654") false;
  ()

let equal = test "String.equal" @@ fun () ->
  eq_bool (String.equal "" "") true;
  eq_bool (String.equal "" "a") false;
  eq_bool (String.equal "a" "") false;
  eq_bool (String.equal "ab" "ab") true;
  eq_bool (String.equal "cd" "ab") false;
  ()

let compare = test "String.compare" @@ fun () ->
  eq_int (String.compare "" "ab") (-1);
  eq_int (String.compare "" "") (0);
  eq_int (String.compare "ab" "") (1);
  eq_int (String.compare "ab" "abc") (-1);
  ()

(* Extracting substrings *)

let with_range = test "String.with_range" @@ fun () ->
  let no_alloc ?first ?len s =
    eq_bool (String.with_range s ?first ?len == s ||
             String.(equal empty s)) true
  in
  let is_empty ?first ?len s =
    let s = String.with_range ?first ?len s in
    eq_str s String.empty;
    eq_bool (s == String.empty) true;
  in
  let invalid ?first ?len s =
    app_invalid ~pp:pp_str (String.with_range ?first ?len) s
  in
  let eq_range ?first ?len s s' = eq_str (String.with_range ?first ?len s) s' in
  no_alloc "";
  invalid  "" ~len:(-1);
  no_alloc "" ~len:0;
  no_alloc "" ~len:1;
  no_alloc "" ~len:2;
  no_alloc "" ~first:(-1);
  no_alloc "" ~first:0;
  no_alloc "" ~first:1;
  invalid  "" ~first:(-1) ~len:(-1);
  no_alloc "" ~first:(-1) ~len:0;
  no_alloc "" ~first:(-1) ~len:1;
  invalid  "" ~first:0 ~len:(-1);
  no_alloc "" ~first:0 ~len:0;
  no_alloc "" ~first:0 ~len:1;
  invalid  "" ~first:1 ~len:(-1);
  no_alloc "" ~first:1 ~len:0;
  no_alloc "" ~first:1 ~len:1;
  no_alloc "a";
  invalid  "a" ~len:(-1);
  is_empty "a" ~len:0;
  no_alloc "a" ~len:1;
  no_alloc "a" ~len:2;
  no_alloc "a" ~first:(-1);
  no_alloc "a" ~first:0;
  is_empty "a" ~first:1;
  invalid  "a" ~first:(-1) ~len:(-1);
  is_empty "a" ~first:(-1) ~len:0;
  is_empty "a" ~first:(-1) ~len:1;
  no_alloc "a" ~first:(-1) ~len:2;
  no_alloc "a" ~first:(-1) ~len:3;
  invalid  "a" ~first:0 ~len:(-1);
  is_empty "a" ~first:0 ~len:0;
  no_alloc "a" ~first:0 ~len:1;
  no_alloc "a" ~first:0 ~len:2;
  no_alloc "a" ~first:0 ~len:3;
  invalid  "a" ~first:1 ~len:(-1);
  is_empty "a" ~first:1 ~len:0;
  is_empty "a" ~first:1 ~len:1;
  is_empty "a" ~first:1 ~len:2;
  is_empty "a" ~first:1 ~len:3;
  no_alloc "ab";
  invalid  "ab" ~len:(-1);
  is_empty "ab" ~len:0;
  eq_range "ab" ~len:1 "a";
  no_alloc "ab" ~len:2;
  no_alloc "ab" ~len:3;
  no_alloc "ab" ~first:(-1);
  no_alloc "ab" ~first:0;
  eq_range "ab" ~first:1 "b";
  is_empty "ab" ~first:2;
  invalid  "ab" ~first:(-1) ~len:(-1);
  is_empty "ab" ~first:(-1) ~len:0;
  is_empty "ab" ~first:(-1) ~len:1;
  eq_range "ab" ~first:(-1) ~len:2 "a";
  no_alloc "ab" ~first:(-1) ~len:3;
  no_alloc "ab" ~first:(-1) ~len:4;
  invalid  "ab" ~first:0 ~len:(-1);
  is_empty "ab" ~first:0 ~len:0;
  eq_range "ab" ~first:0 ~len:1 "a";
  no_alloc "ab" ~first:0 ~len:2;
  no_alloc "ab" ~first:0 ~len:3;
  no_alloc "ab" ~first:0 ~len:4;
  invalid  "ab" ~first:1 ~len:(-1);
  is_empty "ab" ~first:1 ~len:0;
  eq_range "ab" ~first:1 ~len:1 "b";
  eq_range "ab" ~first:1 ~len:2 "b";
  eq_range "ab" ~first:1 ~len:3 "b";
  eq_range "ab" ~first:1 ~len:4 "b";
  invalid  "ab" ~first:2 ~len:(-1);
  is_empty "ab" ~first:2 ~len:0;
  is_empty "ab" ~first:2 ~len:1;
  is_empty "ab" ~first:2 ~len:2;
  is_empty "ab" ~first:2 ~len:3;
  is_empty "ab" ~first:2 ~len:4;
  no_alloc "abc";
  invalid  "abc" ~len:(-1);
  is_empty "abc" ~len:0;
  eq_range "abc" ~len:1 "a";
  eq_range "abc" ~len:2 "ab";
  no_alloc "abc" ~len:3;
  no_alloc "abc" ~len:4;
  no_alloc "abc" ~first:(-1);
  no_alloc "abc" ~first:0;
  eq_range "abc" ~first:1 "bc";
  eq_range "abc" ~first:2 "c";
  is_empty "abc" ~first:3;
  invalid  "abc" ~first:(-1) ~len:(-1);
  is_empty "abc" ~first:(-1) ~len:0;
  is_empty "abc" ~first:(-1) ~len:1;
  eq_range "abc" ~first:(-1) ~len:2 "a";
  eq_range "abc" ~first:(-1) ~len:3 "ab";
  eq_range "abc" ~first:(-1) ~len:4 "abc";
  no_alloc "abc" ~first:(-1) ~len:5;
  invalid  "abc" ~first:0 ~len:(-1);
  is_empty "abc" ~first:0 ~len:0;
  eq_range "abc" ~first:0 ~len:1 "a";
  eq_range "abc" ~first:0 ~len:2 "ab";
  no_alloc "abc" ~first:0 ~len:3;
  no_alloc "abc" ~first:0 ~len:4;
  no_alloc "abc" ~first:0 ~len:5;
  invalid  "abc" ~first:1 ~len:(-1);
  is_empty "abc" ~first:1 ~len:0;
  eq_range "abc" ~first:1 ~len:1 "b";
  eq_range "abc" ~first:1 ~len:2 "bc";
  eq_range "abc" ~first:1 ~len:3 "bc";
  eq_range "abc" ~first:1 ~len:4 "bc";
  eq_range "abc" ~first:1 ~len:5 "bc";
  invalid  "abc" ~first:2 ~len:(-1);
  is_empty "abc" ~first:2 ~len:0;
  eq_range "abc" ~first:2 ~len:1 "c";
  eq_range "abc" ~first:2 ~len:2 "c";
  eq_range "abc" ~first:2 ~len:3 "c";
  eq_range "abc" ~first:2 ~len:4 "c";
  eq_range "abc" ~first:2 ~len:5 "c";
  invalid  "abc" ~first:3 ~len:(-1);
  is_empty "abc" ~first:3 ~len:0;
  is_empty "abc" ~first:3 ~len:1;
  is_empty "abc" ~first:3 ~len:2;
  is_empty "abc" ~first:3 ~len:3;
  is_empty "abc" ~first:3 ~len:4;
  is_empty "abc" ~first:3 ~len:5;
  ()

let with_index_range = test "String.with_index_range" @@ fun () ->
  let no_alloc ?first ?last s =
    eq_bool (String.with_index_range s ?first ?last == s ||
             String.(equal empty s)) true
  in
  let is_empty ?first ?last s =
    let s = String.with_index_range ?first ?last s in
    eq_str s String.empty;
    eq_bool (s == String.empty) true;
  in
  let eq_range ?first ?last s s' =
    eq_str (String.with_index_range ?first ?last s) s'
  in
  no_alloc "";
  no_alloc "" ~first:(-1);
  no_alloc "" ~first:0;
  no_alloc "" ~first:1;
  no_alloc "" ~first:2;
  no_alloc "" ~last:(-1);
  no_alloc "" ~last:0;
  no_alloc "" ~last:1;
  no_alloc "" ~last:2;
  no_alloc "" ~first:(-1) ~last:(-1);
  no_alloc "" ~first:(-1) ~last:0;
  no_alloc "" ~first:(-1) ~last:1;
  no_alloc "" ~first:0 ~last:(-1);
  no_alloc "" ~first:0 ~last:0;
  no_alloc "" ~first:0 ~last:1;
  no_alloc "" ~first:1 ~last:(-1);
  no_alloc "" ~first:1 ~last:0;
  no_alloc "" ~first:1 ~last:1;
  no_alloc "a";
  no_alloc "a" ~first:(-1);
  no_alloc "a" ~first:0;
  is_empty "a" ~first:1;
  is_empty "a" ~first:2;
  is_empty "a" ~last:(-1);
  no_alloc "a" ~last:0;
  no_alloc "a" ~last:1;
  no_alloc "a" ~last:2;
  is_empty "a" ~first:(-1) ~last:(-1);
  no_alloc "a" ~first:(-1) ~last:0;
  no_alloc "a" ~first:(-1) ~last:1;
  no_alloc "a" ~first:(-1) ~last:2;
  no_alloc "a" ~first:(-1) ~last:3;
  is_empty "a" ~first:0 ~last:(-1);
  no_alloc "a" ~first:0 ~last:0;
  no_alloc "a" ~first:0 ~last:1;
  no_alloc "a" ~first:0 ~last:2;
  no_alloc "a" ~first:0 ~last:3;
  is_empty "a" ~first:1 ~last:(-1);
  is_empty "a" ~first:1 ~last:0;
  is_empty "a" ~first:1 ~last:1;
  is_empty "a" ~first:1 ~last:2;
  is_empty "a" ~first:1 ~last:3;
  no_alloc "ab";
  no_alloc "ab" ~first:(-1);
  no_alloc "ab" ~first:0;
  eq_range "ab" ~first:1 "b";
  is_empty "ab" ~first:2;
  is_empty "ab" ~last:(-1);
  eq_range "ab" ~last:0 "a";
  no_alloc "ab" ~last:1;
  no_alloc "ab" ~last:2;
  no_alloc "ab" ~last:3;
  is_empty "ab" ~first:(-1) ~last:(-1);
  eq_range "ab" ~first:(-1) ~last:0 "a";
  no_alloc "ab" ~first:(-1) ~last:1;
  no_alloc "ab" ~first:(-1) ~last:2;
  no_alloc "ab" ~first:(-1) ~last:3;
  no_alloc "ab" ~first:(-1) ~last:4;
  is_empty "ab" ~first:0 ~last:(-1);
  eq_range "ab" ~first:0 ~last:0 "a";
  no_alloc "ab" ~first:0 ~last:1;
  no_alloc "ab" ~first:0 ~last:2;
  no_alloc "ab" ~first:0 ~last:3;
  no_alloc "ab" ~first:0 ~last:4;
  is_empty "ab" ~first:1 ~last:(-1);
  is_empty "ab" ~first:1 ~last:0;
  eq_range "ab" ~first:1 ~last:1 "b";
  eq_range "ab" ~first:1 ~last:2 "b";
  eq_range "ab" ~first:1 ~last:3 "b";
  eq_range "ab" ~first:1 ~last:4 "b";
  is_empty "ab" ~first:2 ~last:(-1);
  is_empty "ab" ~first:2 ~last:0;
  is_empty "ab" ~first:2 ~last:1;
  is_empty "ab" ~first:2 ~last:2;
  is_empty "ab" ~first:2 ~last:3;
  is_empty "ab" ~first:2 ~last:4;
  no_alloc "abc";
  no_alloc "abc" ~first:(-1);
  no_alloc "abc" ~first:0;
  eq_range "abc" ~first:1 "bc";
  eq_range "abc" ~first:2 "c";
  is_empty "abc" ~first:3;
  is_empty "abc" ~last:(-1);
  eq_range "abc" ~last:0 "a";
  eq_range "abc" ~last:1 "ab";
  no_alloc "abc" ~last:2;
  no_alloc "abc" ~last:3;
  no_alloc "abc" ~last:4;
  is_empty "abc" ~first:(-1) ~last:(-1);
  eq_range "abc" ~first:(-1) ~last:0 "a";
  eq_range "abc" ~first:(-1) ~last:1 "ab";
  no_alloc "abc" ~first:(-1) ~last:2;
  no_alloc "abc" ~first:(-1) ~last:3;
  no_alloc "abc" ~first:(-1) ~last:4;
  no_alloc "abc" ~first:(-1) ~last:5;
  is_empty "abc" ~first:0 ~last:(-1);
  eq_range "abc" ~first:0 ~last:0 "a";
  eq_range "abc" ~first:0 ~last:1 "ab";
  no_alloc "abc" ~first:0 ~last:2;
  no_alloc "abc" ~first:0 ~last:3;
  no_alloc "abc" ~first:0 ~last:4;
  no_alloc "abc" ~first:0 ~last:5;
  is_empty "abc" ~first:1 ~last:(-1);
  is_empty "abc" ~first:1 ~last:0;
  eq_range "abc" ~first:1 ~last:1 "b";
  eq_range "abc" ~first:1 ~last:2 "bc";
  eq_range "abc" ~first:1 ~last:3 "bc";
  eq_range "abc" ~first:1 ~last:4 "bc";
  eq_range "abc" ~first:1 ~last:5 "bc";
  is_empty "abc" ~first:2 ~last:(-1);
  is_empty "abc" ~first:2 ~last:0;
  is_empty "abc" ~first:2 ~last:1;
  eq_range "abc" ~first:2 ~last:2 "c";
  eq_range "abc" ~first:2 ~last:3 "c";
  eq_range "abc" ~first:2 ~last:4 "c";
  eq_range "abc" ~first:2 ~last:5 "c";
  is_empty "abc" ~first:3 ~last:(-1);
  is_empty "abc" ~first:3 ~last:0;
  is_empty "abc" ~first:3 ~last:1;
  is_empty "abc" ~first:3 ~last:2;
  is_empty "abc" ~first:3 ~last:3;
  is_empty "abc" ~first:3 ~last:4;
  is_empty "abc" ~first:3 ~last:5;
  ()

let trim = test "String.trim" @@ fun () ->
  let drop_a c = c = 'a' in
  let no_alloc ?drop s = eq_bool (String.trim ?drop s == s) true in
  no_alloc "";
  no_alloc ~drop:drop_a "";
  no_alloc "bc";
  no_alloc ~drop:drop_a "bc";
  eq_str (String.trim "\t abcd \r ") "abcd";
  no_alloc ~drop:drop_a "\x00 abcd \x1F ";
  no_alloc "aaaabcdaaaa";
  eq_str (String.trim ~drop:drop_a "aaaabcdaaaa") "bcd";
  eq_str (String.trim ~drop:drop_a "aaaabcd") "bcd";
  eq_str (String.trim ~drop:drop_a "bcdaaaa") "bcd";
  eq_str (String.trim ~drop:drop_a "aaaa") "";
  eq_str (String.trim "     ") "";
  ()

let span = test "String.{span,take,drop}" @@ fun () ->
  let eq_pair (l0, r0) (l1, r1) = String.equal l0 l1 && String.equal r0 r1 in
  let eq_pair = eq ~eq:eq_pair ~pp:pp_pair in
  let eq ?(rev = false) ?min ?max ?sat s (sl, sr as spec) =
    let (l, r as pair) = String.span ~rev ?min ?max ?sat s in
    let t = String.take ~rev ?min ?max ?sat s in
    let d = String.drop ~rev ?min ?max ?sat s in
    eq_pair pair spec;
    eq_str t (if rev then sr else sl);
    eq_str d (if rev then sl else sr);
    if sl = "" then begin
      eq_bool (l == String.empty) true;
      eq_bool (r == s) true;
      eq_bool ((if rev then d else t) == String.empty) true;
    end;
    if sr = "" then begin
      eq_bool (r == String.empty) true;
      eq_bool (l == s) true;
      eq_bool ((if rev then t else d) == String.empty) true;
    end
  in
  let invalid ?rev ?min ?max ?sat s =
    app_invalid ~pp:pp_pair (String.span ?rev ?min ?max ?sat) s
  in
  invalid ~rev:false ~min:(-1) "";
  invalid ~rev:true  ~min:(-1) "";
  invalid ~rev:false ~max:(-1) "";
  invalid ~rev:true ~max:(-1) "";
  eq ~rev:false String.empty ("","");
  eq ~rev:true  String.empty ("","");
  eq ~rev:false ~min:0 ~max:0 String.empty ("","");
  eq ~rev:true  ~min:0 ~max:0 String.empty ("","");
  eq ~rev:false ~min:1 ~max:0 String.empty ("","");
  eq ~rev:true  ~min:1 ~max:0 String.empty ("","");
  eq ~rev:false ~max:0 "ab_cd" ("","ab_cd");
  eq ~rev:true  ~max:0 "ab_cd" ("ab_cd","");
  eq ~rev:false ~max:2 "ab_cd" ("ab", "_cd");
  eq ~rev:true  ~max:2 "ab_cd" ("ab_", "cd");
  eq ~rev:false ~min:6 "ab_cd" ("", "ab_cd");
  eq ~rev:true  ~min:6 "ab_cd" ("ab_cd", "");
  eq ~rev:false "ab_cd" ("ab_cd", "");
  eq ~rev:true  "ab_cd" ("", "ab_cd");
  eq ~rev:false ~max:30 "ab_cd" ("ab_cd", "");
  eq ~rev:true  ~max:30 "ab_cd" ("", "ab_cd");
  eq ~rev:false ~sat:Char.Ascii.is_white "ab_cd" ("","ab_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_white "ab_cd" ("ab_cd","");
  eq ~rev:false ~sat:Char.Ascii.is_letter "ab_cd" ("ab", "_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_letter "ab_cd" ("ab_", "cd");
  eq ~rev:false ~sat:Char.Ascii.is_letter ~max:0 "ab_cd" ("", "ab_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~max:0 "ab_cd" ("ab_cd", "");
  eq ~rev:false ~sat:Char.Ascii.is_letter ~max:1 "ab_cd" ("a", "b_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~max:1 "ab_cd" ("ab_c", "d");
  eq ~rev:false ~sat:Char.Ascii.is_letter ~min:2 ~max:1 "ab_cd" ("", "ab_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~min:2 ~max:1 "ab_cd" ("ab_cd", "");
  eq ~rev:false ~sat:Char.Ascii.is_letter ~min:3 "ab_cd" ("", "ab_cd");
  eq ~rev:true  ~sat:Char.Ascii.is_letter ~min:3 "ab_cd" ("ab_cd", "");
  ()

let cut = test "String.cut" @@ fun () ->
  let ppp = pp_option pp_pair in
  let eqo = eq_option ~eq:(=) ~pp:pp_pair in
  app_invalid ~pp:ppp (String.cut ~sep:"") "";
  app_invalid ~pp:ppp (String.cut ~sep:"") "123";
  eqo (String.cut "," "")  None;
  eqo (String.cut "," ",") (Some ("", ""));
  eqo (String.cut "," ",,") (Some ("", ","));
  eqo (String.cut "," ",,,") (Some ("", ",,"));
  eqo (String.cut "," "123") None;
  eqo (String.cut "," ",123") (Some ("", "123"));
  eqo (String.cut "," "123,") (Some ("123", ""));
  eqo (String.cut "," "1,2,3") (Some ("1", "2,3"));
  eqo (String.cut "," " 1,2,3") (Some (" 1", "2,3"));
  eqo (String.cut "<>" "") None;
  eqo (String.cut "<>" "<>") (Some ("", ""));
  eqo (String.cut "<>" "<><>") (Some ("", "<>"));
  eqo (String.cut "<>" "<><><>") (Some ("", "<><>"));
  eqo (String.cut ~rev:true ~sep:"<>" "1") None;
  eqo (String.cut "<>" "123") None;
  eqo (String.cut "<>" "<>123") (Some ("", "123"));
  eqo (String.cut "<>" "123<>") (Some ("123", ""));
  eqo (String.cut "<>" "1<>2<>3") (Some ("1", "2<>3"));
  eqo (String.cut "<>" " 1<>2<>3") (Some (" 1", "2<>3"));
  eqo (String.cut "<>" ">>><>>>><>>>><>>>>") (Some (">>>", ">>><>>>><>>>>"));
  eqo (String.cut "<->" "<->>->") (Some ("", ">->"));
  eqo (String.cut ~rev:true ~sep:"<->" "<-") None;
  eqo (String.cut "aa" "aa") (Some ("", ""));
  eqo (String.cut "aa" "aaa") (Some ("", "a"));
  eqo (String.cut "aa" "aaaa") (Some ("", "aa"));
  eqo (String.cut "aa" "aaaaa") (Some ("", "aaa";));
  eqo (String.cut "aa" "aaaaaa") (Some ("", "aaaa"));
  eqo (String.cut ~sep:"ab" "faaaa") None;
  let rev = true in
  app_invalid ~pp:ppp (String.cut ~rev ~sep:"") "";
  app_invalid ~pp:ppp (String.cut ~rev ~sep:"") "123";
  eqo (String.cut ~rev ~sep:"," "") None;
  eqo (String.cut ~rev ~sep:"," ",") (Some ("", ""));
  eqo (String.cut ~rev ~sep:"," ",,") (Some (",", ""));
  eqo (String.cut ~rev ~sep:"," ",,,") (Some (",,", ""));
  eqo (String.cut ~rev ~sep:"," "123") None;
  eqo (String.cut ~rev ~sep:"," ",123") (Some ("", "123"));
  eqo (String.cut ~rev ~sep:"," "123,") (Some ("123", ""));
  eqo (String.cut ~rev ~sep:"," "1,2,3") (Some ("1,2", "3"));
  eqo (String.cut ~rev ~sep:"," "1,2,3 ") (Some ("1,2", "3 "));
  eqo (String.cut ~rev ~sep:"<>" "") None;
  eqo (String.cut ~rev ~sep:"<>" "<>") (Some ("", ""));
  eqo (String.cut ~rev ~sep:"<>" "<><>") (Some ("<>", ""));
  eqo (String.cut ~rev ~sep:"<>" "<><><>") (Some ("<><>", ""));
  eqo (String.cut ~rev ~sep:"<>" "1") None;
  eqo (String.cut ~rev ~sep:"<>" "123") None;
  eqo (String.cut ~rev ~sep:"<>" "<>123") (Some ("", "123"));
  eqo (String.cut ~rev ~sep:"<>" "123<>") (Some ("123", ""));
  eqo (String.cut ~rev ~sep:"<>" "1<>2<>3") (Some ("1<>2", "3"));
  eqo (String.cut ~rev ~sep:"<>" "1<>2<>3 ") (Some ("1<>2", "3 "));
  eqo (String.cut ~rev ~sep:"<>" ">>><>>>><>>>><>>>>")
    (Some (">>><>>>><>>>>", ">>>"));
  eqo (String.cut ~rev ~sep:"<->" "<->>->") (Some ("", ">->"));
  eqo (String.cut ~rev ~sep:"<->" "<-") None;
  eqo (String.cut ~rev ~sep:"aa" "aa") (Some ("", ""));
  eqo (String.cut ~rev ~sep:"aa" "aaa") (Some ("a", ""));
  eqo (String.cut ~rev ~sep:"aa" "aaaa") (Some ("aa", ""));
  eqo (String.cut ~rev ~sep:"aa" "aaaaa") (Some ("aaa", "";));
  eqo (String.cut ~rev ~sep:"aa" "aaaaaa") (Some ("aaaa", ""));
  eqo (String.cut ~rev ~sep:"ab" "afaaaa") None;
  ()

let cuts = test "String.cuts" @@ fun () ->
  let ppl = pp_list String.dump in
  let eql = eq_list ~eq:String.equal ~pp:String.dump in
  let no_alloc ?rev ~sep s =
    eq_bool (List.hd (String.cuts ?rev ~sep s) == s) true
  in
  app_invalid ~pp:ppl (String.cuts ~sep:"") "";
  app_invalid ~pp:ppl (String.cuts ~sep:"") "123";
  no_alloc ~sep:"," "";
  no_alloc ~sep:"," "abcd";
  eql (String.cuts ~empty:true  ~sep:"," "") [""];
  eql (String.cuts ~empty:false ~sep:"," "") [];
  eql (String.cuts ~empty:true  ~sep:"," ",") [""; ""];
  eql (String.cuts ~empty:false ~sep:"," ",") [];
  eql (String.cuts ~empty:true  ~sep:"," ",,") [""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"," ",,") [];
  eql (String.cuts ~empty:true  ~sep:"," ",,,") [""; ""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"," ",,,") [];
  eql (String.cuts ~empty:true  ~sep:"," "123") ["123"];
  eql (String.cuts ~empty:false ~sep:"," "123") ["123"];
  eql (String.cuts ~empty:true  ~sep:"," ",123") [""; "123"];
  eql (String.cuts ~empty:false ~sep:"," ",123") ["123"];
  eql (String.cuts ~empty:true  ~sep:"," "123,") ["123"; ""];
  eql (String.cuts ~empty:false ~sep:"," "123,") ["123";];
  eql (String.cuts ~empty:true  ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (String.cuts ~empty:false ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (String.cuts ~empty:true  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~empty:false  ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~empty:true ~sep:"," ",1,2,,3,") [""; "1"; "2"; ""; "3"; ""];
  eql (String.cuts ~empty:false ~sep:"," ",1,2,,3,") ["1"; "2"; "3";];
  eql (String.cuts ~empty:true ~sep:"," ", 1, 2,, 3,")
    [""; " 1"; " 2"; ""; " 3"; ""];
  eql (String.cuts ~empty:false ~sep:"," ", 1, 2,, 3,") [" 1"; " 2";" 3";];
  eql (String.cuts ~empty:true ~sep:"<>" "") [""];
  eql (String.cuts ~empty:false ~sep:"<>" "") [];
  eql (String.cuts ~empty:true ~sep:"<>" "<>") [""; ""];
  eql (String.cuts ~empty:false ~sep:"<>" "<>") [];
  eql (String.cuts ~empty:true ~sep:"<>" "<><>") [""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"<>" "<><>") [];
  eql (String.cuts ~empty:true ~sep:"<>" "<><><>") [""; ""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"<>" "<><><>") [];
  eql (String.cuts ~empty:true ~sep:"<>" "123") [ "123" ];
  eql (String.cuts ~empty:false ~sep:"<>" "123") [ "123" ];
  eql (String.cuts ~empty:true ~sep:"<>" "<>123") [""; "123"];
  eql (String.cuts ~empty:false ~sep:"<>" "<>123") ["123"];
  eql (String.cuts ~empty:true ~sep:"<>" "123<>") ["123"; ""];
  eql (String.cuts ~empty:false ~sep:"<>" "123<>") ["123"];
  eql (String.cuts ~empty:true ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (String.cuts ~empty:false ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (String.cuts ~empty:true ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~empty:false ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~empty:true ~sep:"<>" "<>1<>2<><>3<>")
    [""; "1"; "2"; ""; "3"; ""];
  eql (String.cuts ~empty:false ~sep:"<>" "<>1<>2<><>3<>") ["1"; "2";"3";];
  eql (String.cuts ~empty:true ~sep:"<>" "<> 1<> 2<><> 3<>")
    [""; " 1"; " 2"; ""; " 3";""];
  eql (String.cuts ~empty:false ~sep:"<>" "<> 1<> 2<><> 3<>")[" 1"; " 2"; " 3"];
  eql (String.cuts ~empty:true ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (String.cuts ~empty:false ~sep:"<>" ">>><>>>><>>>><>>>>")
    [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (String.cuts ~empty:true ~sep:"<->" "<->>->") [""; ">->"];
  eql (String.cuts ~empty:false ~sep:"<->" "<->>->") [">->"];
  eql (String.cuts ~empty:true ~sep:"aa" "aa") [""; ""];
  eql (String.cuts ~empty:false ~sep:"aa" "aa") [];
  eql (String.cuts ~empty:true ~sep:"aa" "aaa") [""; "a"];
  eql (String.cuts ~empty:false ~sep:"aa" "aaa") ["a"];
  eql (String.cuts ~empty:true ~sep:"aa" "aaaa") [""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"aa" "aaaa") [];
  eql (String.cuts ~empty:true ~sep:"aa" "aaaaa") [""; ""; "a"];
  eql (String.cuts ~empty:false ~sep:"aa" "aaaaa") ["a"];
  eql (String.cuts ~empty:true ~sep:"aa" "aaaaaa") [""; ""; ""; ""];
  eql (String.cuts ~empty:false ~sep:"aa" "aaaaaa") [];
  let rev = true in
  app_invalid ~pp:ppl (String.cuts ~rev ~sep:"") "";
  app_invalid ~pp:ppl (String.cuts ~rev ~sep:"") "123";
  no_alloc ~rev ~sep:"," "";
  no_alloc ~rev ~sep:"," "abcd";
  eql (String.cuts ~rev ~empty:true ~sep:"," "") [""];
  eql (String.cuts ~rev ~empty:false ~sep:"," "") [];
  eql (String.cuts ~rev ~empty:true ~sep:"," ",") [""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," ",") [];
  eql (String.cuts ~rev ~empty:true ~sep:"," ",,") [""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," ",,") [];
  eql (String.cuts ~rev ~empty:true ~sep:"," ",,,") [""; ""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," ",,,") [];
  eql (String.cuts ~rev ~empty:true ~sep:"," "123") ["123"];
  eql (String.cuts ~rev ~empty:false ~sep:"," "123") ["123"];
  eql (String.cuts ~rev ~empty:true ~sep:"," ",123") [""; "123"];
  eql (String.cuts ~rev ~empty:false ~sep:"," ",123") ["123"];
  eql (String.cuts ~rev ~empty:true ~sep:"," "123,") ["123"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," "123,") ["123";];
  eql (String.cuts ~rev ~empty:true ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:false ~sep:"," "1,2,3") ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:true ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~rev ~empty:false ~sep:"," "1, 2, 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~rev ~empty:true ~sep:"," ",1,2,,3,")
    [""; "1"; "2"; ""; "3"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," ",1,2,,3,") ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:true ~sep:"," ", 1, 2,, 3,")
    [""; " 1"; " 2"; ""; " 3"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"," ", 1, 2,, 3,") [" 1"; " 2"; " 3"];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "") [""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "") [];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<>") [""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<>") [];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<><>") [""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<><>") [];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<><><>") [""; ""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<><><>") [];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "123") [ "123" ];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "123") [ "123" ];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<>123") [""; "123"];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<>123") ["123"];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "123<>") ["123"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "123<>") ["123";];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "1<>2<>3") ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "1<> 2<> 3") ["1"; " 2"; " 3"];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<>1<>2<><>3<>")
    [""; "1"; "2"; ""; "3"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<>1<>2<><>3<>")
    ["1"; "2"; "3"];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" "<> 1<> 2<><> 3<>")
                                  [""; " 1"; " 2"; ""; " 3";""];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" "<> 1<> 2<><> 3<>")
                                  [" 1"; " 2"; " 3";];
  eql (String.cuts ~rev ~empty:true ~sep:"<>" ">>><>>>><>>>><>>>>")
                                  [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (String.cuts ~rev ~empty:false ~sep:"<>" ">>><>>>><>>>><>>>>")
                                  [">>>"; ">>>"; ">>>"; ">>>" ];
  eql (String.cuts ~rev ~empty:true ~sep:"<->" "<->>->") [""; ">->"];
  eql (String.cuts ~rev ~empty:false ~sep:"<->" "<->>->") [">->"];
  eql (String.cuts ~rev ~empty:true ~sep:"aa" "aa") [""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"aa" "aa") [];
  eql (String.cuts ~rev ~empty:true ~sep:"aa" "aaa") ["a"; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"aa" "aaa") ["a"];
  eql (String.cuts ~rev ~empty:true ~sep:"aa" "aaaa") [""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"aa" "aaaa") [];
  eql (String.cuts ~rev ~empty:true ~sep:"aa" "aaaaa") ["a"; ""; "";];
  eql (String.cuts ~rev ~empty:false ~sep:"aa" "aaaaa") ["a";];
  eql (String.cuts ~rev ~empty:true ~sep:"aa" "aaaaaa") [""; ""; ""; ""];
  eql (String.cuts ~rev ~empty:false ~sep:"aa" "aaaaaa") [];
  ()

let fields = test "String.fields" @@ fun () ->
  let eql = eq_list ~eq:String.equal ~pp:String.dump in
  let no_alloc ?empty ?is_sep s =
    eq_bool (List.hd (String.fields ?empty ?is_sep s) == s) true
  in
  let is_a c = c = 'a' in
  no_alloc ~empty:true "a";
  no_alloc ~empty:false "a";
  no_alloc ~empty:true "abc";
  no_alloc ~empty:false "abc";
  no_alloc ~empty:true ~is_sep:is_a "bcdf";
  no_alloc ~empty:false ~is_sep:is_a "bcdf";
  eql (String.fields ~empty:true "") [""];
  eql (String.fields ~empty:false "") [];
  eql (String.fields ~empty:true "\n\r") ["";"";""];
  eql (String.fields ~empty:false "\n\r") [];
  eql (String.fields ~empty:true " \n\rabc") ["";"";"";"abc"];
  eql (String.fields ~empty:false " \n\rabc") ["abc"];
  eql (String.fields ~empty:true " \n\racd de") ["";"";"";"acd";"de"];
  eql (String.fields ~empty:false " \n\racd de") ["acd";"de"];
  eql (String.fields ~empty:true " \n\racd de ") ["";"";"";"acd";"de";""];
  eql (String.fields ~empty:false " \n\racd de ") ["acd";"de"];
  eql (String.fields ~empty:true "\n\racd\nde \r") ["";"";"acd";"de";"";""];
  eql (String.fields ~empty:false "\n\racd\nde \r") ["acd";"de"];
  eql (String.fields ~empty:true ~is_sep:is_a "") [""];
  eql (String.fields ~empty:false ~is_sep:is_a "") [];
  eql (String.fields ~empty:true ~is_sep:is_a "abaac aaa")
    ["";"b";"";"c ";"";"";""];
  eql (String.fields ~empty:false ~is_sep:is_a "abaac aaa") ["b"; "c "];
  eql (String.fields ~empty:true ~is_sep:is_a "aaaa") ["";"";"";"";""];
  eql (String.fields ~empty:false ~is_sep:is_a "aaaa") [];
  eql (String.fields ~empty:true ~is_sep:is_a "aaaa ") ["";"";"";"";" "];
  eql (String.fields ~empty:false ~is_sep:is_a "aaaa ") [" "];
  eql (String.fields ~empty:true ~is_sep:is_a "aaaab") ["";"";"";"";"b"];
  eql (String.fields ~empty:false ~is_sep:is_a "aaaab") ["b"];
  eql (String.fields ~empty:true ~is_sep:is_a "baaaa") ["b";"";"";"";""];
  eql (String.fields ~empty:false ~is_sep:is_a "baaaa") ["b"];
  eql (String.fields ~empty:true ~is_sep:is_a "abaaaa") ["";"b";"";"";"";""];
  eql (String.fields ~empty:false ~is_sep:is_a "abaaaa") ["b"];
  eql (String.fields ~empty:true ~is_sep:is_a "aba") ["";"b";""];
  eql (String.fields ~empty:false ~is_sep:is_a "aba") ["b"];
  eql (String.fields ~empty:false "tokenize me please")
    ["tokenize"; "me"; "please"];
  ()

(* Traversing strings *)

let find = test "String.find" @@ fun () ->
  let eq = eq_option ~eq:(=) ~pp:pp_int in
  let a c = c = 'a' in
  eq (String.find ~rev:false a "") None;
  eq (String.find ~rev:false ~start:(-1) a "") None;
  eq (String.find ~rev:false ~start:0 a "") None;
  eq (String.find ~rev:false ~start:1 a "") None;
  eq (String.find ~rev:true a "") None;
  eq (String.find ~rev:true ~start:(-1) a "") None;
  eq (String.find ~rev:true ~start:0 a "") None;
  eq (String.find ~rev:true ~start:1 a "") None;
  eq (String.find ~rev:false ~start:(-1) a "a") (Some 0);
  eq (String.find ~rev:false ~start:0 a "a") (Some 0);
  eq (String.find ~rev:false ~start:1 a "a") None;
  eq (String.find ~rev:true ~start:(-1) a "a") None;
  eq (String.find ~rev:true ~start:0 a "a") (Some 0);
  eq (String.find ~rev:true ~start:1 a "a") (Some 0);
  eq (String.find ~rev:false ~start:(-1) a "ba") (Some 1);
  eq (String.find ~rev:false ~start:0 a "ba") (Some 1);
  eq (String.find ~rev:false ~start:1 a "ba") (Some 1);
  eq (String.find ~rev:false ~start:2 a "ba") None;
  eq (String.find ~rev:true ~start:(-1) a "ba") None;
  eq (String.find ~rev:true ~start:0 a "ba") None;
  eq (String.find ~rev:true ~start:1 a "ba") (Some 1);
  eq (String.find ~rev:true ~start:2 a "ba") (Some 1);
  eq (String.find ~rev:true ~start:3 a "ba") (Some 1);
  eq (String.find ~rev:false a "aba") (Some 0);
  eq (String.find ~rev:false ~start:(-1) a "aba") (Some 0);
  eq (String.find ~rev:false ~start:0 a "aba") (Some 0);
  eq (String.find ~rev:false ~start:1 a "aba") (Some 2);
  eq (String.find ~rev:false ~start:2 a "aba") (Some 2);
  eq (String.find ~rev:false ~start:3 a "aba") None;
  eq (String.find ~rev:false ~start:4 a "aba") None;
  eq (String.find ~rev:true a "aba") (Some 2);
  eq (String.find ~rev:true ~start:(-1) a "aba") None;
  eq (String.find ~rev:true ~start:0 a "aba") (Some 0);
  eq (String.find ~rev:true ~start:1 a "aba") (Some 0);
  eq (String.find ~rev:true ~start:2 a "aba") (Some 2);
  eq (String.find ~rev:true ~start:3 a "aba") (Some 2);
  eq (String.find ~rev:true ~start:4 a "aba") (Some 2);
  eq (String.find ~rev:false a "bab") (Some 1);
  eq (String.find ~rev:false ~start:(-1) a "bab") (Some 1);
  eq (String.find ~rev:false ~start:0 a "bab") (Some 1);
  eq (String.find ~rev:false ~start:1 a "bab") (Some 1);
  eq (String.find ~rev:false ~start:2 a "bab") None;
  eq (String.find ~rev:false ~start:3 a "bab") None;
  eq (String.find ~rev:false ~start:4 a "bab") None;
  eq (String.find ~rev:true a "bab") (Some 1);
  eq (String.find ~rev:true ~start:(-1) a "bab") None;
  eq (String.find ~rev:true ~start:0 a "bab") None;
  eq (String.find ~rev:true ~start:1 a "bab") (Some 1);
  eq (String.find ~rev:true ~start:2 a "bab") (Some 1);
  eq (String.find ~rev:true ~start:3 a "bab") (Some 1);
  eq (String.find ~rev:true ~start:4 a "bab") (Some 1);
  eq (String.find ~rev:false a "baab") (Some 1);
  eq (String.find ~rev:false ~start:(-1) a "baab") (Some 1);
  eq (String.find ~rev:false ~start:0 a "baab") (Some 1);
  eq (String.find ~rev:false ~start:1 a "baab") (Some 1);
  eq (String.find ~rev:false ~start:2 a "baab") (Some 2);
  eq (String.find ~rev:false ~start:3 a "baab") None;
  eq (String.find ~rev:false ~start:4 a "baab") None;
  eq (String.find ~rev:false ~start:5 a "baab") None;
  eq (String.find ~rev:true ~start:(-1) a "baab") None;
  eq (String.find ~rev:true ~start:0 a "baab") None;
  eq (String.find ~rev:true ~start:1 a "baab") (Some 1);
  eq (String.find ~rev:true ~start:2 a "baab") (Some 2);
  eq (String.find ~rev:true ~start:3 a "baab") (Some 2);
  eq (String.find ~rev:true ~start:4 a "baab") (Some 2);
  eq (String.find ~rev:true ~start:5 a "baab") (Some 2);
  ()

let find_sub = test "String.find_sub" @@ fun () ->
  let eq = eq_option ~eq:(=) ~pp:pp_int in
  eq (String.find_sub ~rev:false ~sub:"" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~start:0 ~sub:"" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~start:1 ~sub:"" "ab") (Some 1);
  eq (String.find_sub ~rev:false ~start:2 ~sub:"" "ab") None;
  eq (String.find_sub ~rev:true ~sub:"" "ab") (Some 1);
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"" "ab") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"" "ab") (Some 0);
  eq (String.find_sub ~rev:true ~start:1 ~sub:"" "ab") (Some 1);
  eq (String.find_sub ~rev:true ~start:2 ~sub:"" "ab") (Some 1);
  eq (String.find_sub ~rev:false ~sub:"" "") None;
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"" "") None;
  eq (String.find_sub ~rev:false ~start:0 ~sub:"" "") None;
  eq (String.find_sub ~rev:false ~start:1 ~sub:"" "") None;
  eq (String.find_sub ~rev:true ~sub:"" "") None;
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"" "") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"" "") None;
  eq (String.find_sub ~rev:true ~start:1 ~sub:"" "") None;
  eq (String.find_sub ~rev:false ~sub:"ab" "") None;
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"ab" "") None;
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "") None;
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "") None;
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"ab" "") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "") None;
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "") None;
  eq (String.find_sub ~rev:false ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:false ~start:2 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:true ~start:2 ~sub:"ab" "a") None;
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "ab") None;
  eq (String.find_sub ~rev:false ~start:2 ~sub:"ab" "ab") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"ab" "ab") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:true ~start:2 ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:true ~start:3 ~sub:"ab" "ab") (Some 0);
  eq (String.find_sub ~rev:false ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "aba") None;
  eq (String.find_sub ~rev:false ~start:2 ~sub:"ab" "aba") None;
  eq (String.find_sub ~rev:false ~start:3 ~sub:"ab" "aba") None;
  eq (String.find_sub ~rev:false ~start:4 ~sub:"ab" "aba") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"ab" "aba") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:true ~start:2 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:true ~start:3 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:true ~start:4 ~sub:"ab" "aba") (Some 0);
  eq (String.find_sub ~rev:false ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:false ~start:2 ~sub:"ab" "bab") None;
  eq (String.find_sub ~rev:false ~start:3 ~sub:"ab" "bab") None;
  eq (String.find_sub ~rev:false ~start:4 ~sub:"ab" "bab") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"ab" "bab") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "bab") None;
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:true ~start:2 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:true ~start:3 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:true ~start:4 ~sub:"ab" "bab") (Some 1);
  eq (String.find_sub ~rev:false ~sub:"ab" "abab") (Some 0);
  eq (String.find_sub ~rev:false ~start:(-1) ~sub:"ab" "abab") (Some 0);
  eq (String.find_sub ~rev:false ~start:0 ~sub:"ab" "abab") (Some 0);
  eq (String.find_sub ~rev:false ~start:1 ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:false ~start:2 ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:false ~start:3 ~sub:"ab" "abab") None;
  eq (String.find_sub ~rev:false ~start:4 ~sub:"ab" "abab") None;
  eq (String.find_sub ~rev:false ~start:5 ~sub:"ab" "abab") None;
  eq (String.find_sub ~rev:true ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:true ~start:(-1) ~sub:"ab" "abab") None;
  eq (String.find_sub ~rev:true ~start:0 ~sub:"ab" "abab") (Some 0);
  eq (String.find_sub ~rev:true ~start:1 ~sub:"ab" "abab") (Some 0);
  eq (String.find_sub ~rev:true ~start:2 ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:true ~start:3 ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:true ~start:4 ~sub:"ab" "abab") (Some 2);
  eq (String.find_sub ~rev:true ~start:5 ~sub:"ab" "abab") (Some 2);
  ()

let filter = test "String.filter[_map]" @@ fun () ->
  let no_alloc k f s = eq_bool (k f s == s) true in
  no_alloc String.filter (fun _ -> true) "";
  no_alloc String.filter (fun _ -> true) "abcd";
  no_alloc String.filter_map (fun c -> Some c) "";
  no_alloc String.filter_map (fun c -> Some c) "abcd";
  let gen_filter :
    'a. ('a -> string -> string) -> 'a -> unit =
  fun filter a ->
    no_alloc filter a "";
    no_alloc filter a "a";
    no_alloc filter a "aa";
    no_alloc filter a "aaa";
    eq_str (filter a "ab") "a";
    eq_str (filter a "ba") "a";
    eq_str (filter a "abc") "a";
    eq_str (filter a "bac") "a";
    eq_str (filter a "bca") "a";
    eq_str (filter a "aba") "aa";
    eq_str (filter a "aab") "aa";
    eq_str (filter a "baa") "aa";
    eq_str (filter a "aabc") "aa";
    eq_str (filter a "abac") "aa";
    eq_str (filter a "abca") "aa";
    eq_str (filter a "baca") "aa";
    eq_str (filter a "bcaa") "aa";
  in
  gen_filter String.filter (fun c -> c = 'a');
  gen_filter String.filter_map (fun c -> if c = 'a' then Some c else None);
  let subst_a = function 'a' -> Some 'z' | c -> Some c in
  no_alloc String.filter_map subst_a "";
  no_alloc String.filter_map subst_a "b";
  no_alloc String.filter_map subst_a "bcd";
  eq_str (String.filter_map subst_a "a") "z";
  eq_str (String.filter_map subst_a "aa") "zz";
  eq_str (String.filter_map subst_a "aaa") "zzz";
  eq_str (String.filter_map subst_a "ab") "zb";
  eq_str (String.filter_map subst_a "ba") "bz";
  eq_str (String.filter_map subst_a "abc") "zbc";
  eq_str (String.filter_map subst_a "bac") "bzc";
  eq_str (String.filter_map subst_a "bca") "bcz";
  eq_str (String.filter_map subst_a "aba") "zbz";
  eq_str (String.filter_map subst_a "aab") "zzb";
  eq_str (String.filter_map subst_a "baa") "bzz";
  eq_str (String.filter_map subst_a "aabc") "zzbc";
  eq_str (String.filter_map subst_a "abac") "zbzc";
  eq_str (String.filter_map subst_a "abca") "zbcz";
  eq_str (String.filter_map subst_a "baca") "bzcz";
  eq_str (String.filter_map subst_a "bcaa") "bczz";
  let subst_a_del_b = function 'a' -> Some 'z' | 'b' -> None | c -> Some c in
  no_alloc String.filter_map subst_a_del_b "";
  no_alloc String.filter_map subst_a_del_b "c";
  no_alloc String.filter_map subst_a_del_b "cd";
  eq_str (String.filter_map subst_a_del_b "a") "z";
  eq_str (String.filter_map subst_a_del_b "aa") "zz";
  eq_str (String.filter_map subst_a_del_b "aaa") "zzz";
  eq_str (String.filter_map subst_a_del_b "ab") "z";
  eq_str (String.filter_map subst_a_del_b "ba") "z";
  eq_str (String.filter_map subst_a_del_b "abc") "zc";
  eq_str (String.filter_map subst_a_del_b "bac") "zc";
  eq_str (String.filter_map subst_a_del_b "bca") "cz";
  eq_str (String.filter_map subst_a_del_b "aba") "zz";
  eq_str (String.filter_map subst_a_del_b "aab") "zz";
  eq_str (String.filter_map subst_a_del_b "baa") "zz";
  eq_str (String.filter_map subst_a_del_b "aabc") "zzc";
  eq_str (String.filter_map subst_a_del_b "abac") "zzc";
  eq_str (String.filter_map subst_a_del_b "abca") "zcz";
  eq_str (String.filter_map subst_a_del_b "baca") "zcz";
  eq_str (String.filter_map subst_a_del_b "bcaa") "czz";
  ()

let map = test "String.map[i]" @@ fun () ->
  let next_letter c = Char.(of_byte @@ to_int c + 1) in
  let no_alloc map f s = eq_bool (map f s == s) true in
  no_alloc String.map (fun c -> c) String.empty;
  no_alloc String.map (fun c -> c) "abcd";
  eq_str (String.map (fun c -> fail "invoked"; c) "") "";
  eq_str (String.map next_letter "abcd") "bcde";
  no_alloc String.mapi (fun _ c -> c) String.empty;
  no_alloc String.mapi (fun _ c -> c) "abcd";
  eq_str (String.mapi (fun _ c -> fail "invoked"; c) "") "";
  eq_str (String.mapi (fun i c -> Char.(of_byte @@ to_int c + i)) "abcd")
    "aceg";
  ()

let fold = test "String.fold_{left,right}" @@ fun () ->
  let eql = eq_list ~eq:(=) ~pp:pp_char in
  String.fold_left (fun _ _ -> fail "invoked") () "";
  eql (String.fold_left (fun acc c -> c :: acc) [] "") [];
  eql (String.fold_left (fun acc c -> c :: acc) [] "abc") ['c';'b';'a'];
  String.fold_right (fun _ _ -> fail "invoked") "" ();
  eql (String.fold_right (fun c acc -> c :: acc) "" []) [];
  eql (String.fold_right (fun c acc -> c :: acc) "abc" []) ['a';'b';'c'];
  ()

let iter = test "String.iter[i]" @@ fun () ->
  let s = "abcd" in
  String.iter (fun _ -> fail "invoked") "";
  String.iteri (fun _ _ -> fail "invoked") "";
  (let i = ref 0 in String.iter (fun c -> eq_char s.[!i] c; incr i) s);
  String.iteri (fun i c -> eq_char s.[i] c) s;
  ()

(* Ascii support *)

let ascii_is_valid = test "String.Ascii.is_valid" @@ fun () ->
  eq_bool (String.Ascii.is_valid "") true;
  eq_bool (String.Ascii.is_valid "a") true;
  eq_bool (String.(Ascii.is_valid (v ~len:(0x7F + 1)
                                     (fun i -> Char.of_byte i)))) true;
  ()

let ascii_casing =
  test "String.Ascii.{uppercase,lowercase,capitalize,uncapitalize}"
  @@ fun () ->
  let no_alloc f s = eq_bool (f s == s) true in
  no_alloc String.Ascii.uppercase "";
  no_alloc String.Ascii.uppercase "HEHEY \x7F\xFF\x00\x0A";
  eq_str (String.Ascii.uppercase "HeHey \x7F\xFF\x00\x0A")
    "HEHEY \x7F\xFF\x00\x0A";
  eq_str (String.Ascii.uppercase "abcdefghijklmnopqrstuvwxyz")
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  no_alloc String.Ascii.lowercase "";
  no_alloc String.Ascii.lowercase "hehey \x7F\xFF\x00\x0A";
  eq_str (String.Ascii.lowercase "hEhEY \x7F\xFF\x00\x0A")
    "hehey \x7F\xFF\x00\x0A";
  eq_str (String.Ascii.lowercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    "abcdefghijklmnopqrstuvwxyz";
  no_alloc String.Ascii.capitalize "";
  no_alloc String.Ascii.capitalize "Hehey";
  no_alloc String.Ascii.capitalize "\x00hehey";
  eq_str (String.Ascii.capitalize "hehey") "Hehey";
  no_alloc String.Ascii.uncapitalize "";
  no_alloc String.Ascii.uncapitalize "hehey";
  no_alloc String.Ascii.uncapitalize "\x00hehey";
  eq_str (String.Ascii.uncapitalize "Hehey") "hehey";
  ()

let ascii_escapes = test "String.Ascii.escape[_string]" @@ fun () ->
  let no_alloc s = eq_bool ((String.Ascii.escape s) == s) true in
  no_alloc "";
  no_alloc "abcd";
  no_alloc "~";
  no_alloc " ";
  eq_str (String.Ascii.escape "\x00abc") "\\x00abc";
  eq_str (String.Ascii.escape "\nabc") "\\x0Aabc";
  eq_str (String.Ascii.escape "\nab\xFFc") "\\x0Aab\\xFFc";
  eq_str (String.Ascii.escape "\nab\xFF") "\\x0Aab\\xFF";
  eq_str (String.Ascii.escape "\nab\\") "\\x0Aab\\\\";
  eq_str (String.Ascii.escape "\\") "\\\\";
  eq_str (String.Ascii.escape "\\\x00\x1F\x7F\xFF") "\\\\\\x00\\x1F\\x7F\\xFF";
  let no_alloc s =
    eq_bool ((String.Ascii.escape_string s) == s) true
  in
  no_alloc "";
  no_alloc "abcd";
  no_alloc "~";
  no_alloc " ";
  eq_str (String.Ascii.escape_string "\x00abc") "\\x00abc";
  eq_str (String.Ascii.escape_string "\nabc") "\\nabc";
  eq_str (String.Ascii.escape_string "\nab\xFFc") "\\nab\\xFFc";
  eq_str (String.Ascii.escape_string "\nab\xFF") "\\nab\\xFF";
  eq_str (String.Ascii.escape_string "\nab\\") "\\nab\\\\";
  eq_str (String.Ascii.escape_string "\\") "\\\\";
  eq_str (String.Ascii.escape_string "\b\t\n\r\"\\\x00\x1F\x7F\xFF")
    "\\b\\t\\n\\r\\\"\\\\\\x00\\x1F\\x7F\\xFF";
  ()

let ascii_unescapes = test "String.Ascii.unescape[_string]" @@ fun () ->
  let no_alloc unescape s = match unescape s with
  | None -> fail "expected (Some %S)" s
  | Some s' -> eq_bool (s == s') true
  in
  let eq_o = eq_option ~eq:String.equal ~pp:pp_str in
  no_alloc String.Ascii.unescape "";
  no_alloc String.Ascii.unescape "abcd";
  no_alloc String.Ascii.unescape "~";
  no_alloc String.Ascii.unescape " ";
  eq_o (String.Ascii.unescape "\\x00abc") (Some "\x00abc");
  eq_o (String.Ascii.unescape "\\x0Aabc") (Some "\nabc");
  eq_o (String.Ascii.unescape "\\x0Aab\\xFFc") (Some "\nab\xFFc");
  eq_o (String.Ascii.unescape "\\x0Aab\\xFF") (Some "\nab\xFF");
  eq_o (String.Ascii.unescape "\\x0Aab\\\\") (Some "\nab\\");
  eq_o (String.Ascii.unescape "a\\\\") (Some "a\\");
  eq_o (String.Ascii.unescape "\\\\") (Some "\\");
  eq_o (String.Ascii.unescape "a\\\\\\x00\\x1F\\x7F\\xFF")
    (Some "a\\\x00\x1F\x7F\xFF");
  eq_o (String.Ascii.unescape "\\x61") (Some "a");
  eq_o (String.Ascii.unescape "\\x20") (Some " ");
  eq_o (String.Ascii.unescape "\\x2") None;
  eq_o (String.Ascii.unescape "\\x") None;
  eq_o (String.Ascii.unescape "\\") None;
  eq_o (String.Ascii.unescape "a\\b") None;
  eq_o (String.Ascii.unescape "a\\t") None;
  eq_o (String.Ascii.unescape "b\\n") None;
  eq_o (String.Ascii.unescape "b\\r") None;
  eq_o (String.Ascii.unescape "b\\\"") None;
  eq_o (String.Ascii.unescape "b\\z") None;
  eq_o (String.Ascii.unescape "b\\1") None;
  no_alloc String.Ascii.unescape_string "";
  no_alloc String.Ascii.unescape_string "abcd";
  no_alloc String.Ascii.unescape_string "~";
  no_alloc String.Ascii.unescape_string " ";
  eq_o (String.Ascii.unescape_string "\\x00abc") (Some "\x00abc");
  eq_o (String.Ascii.unescape_string "\\nabc") (Some "\nabc");
  eq_o (String.Ascii.unescape_string "\\nab\\xFFc") (Some "\nab\xFFc");
  eq_o (String.Ascii.unescape_string "\\nab\\xFF") (Some "\nab\xFF");
  eq_o (String.Ascii.unescape_string "\\nab\\\\") (Some "\nab\\");
  eq_o (String.Ascii.unescape_string "a\\\\") (Some "a\\");
  eq_o (String.Ascii.unescape_string "\\\\") (Some "\\");
  eq_o (String.Ascii.unescape_string
          "\\b\\t\\n\\r\\\"\\\\\\x00\\x1F\\x7F\\xFF")
    (Some "\b\t\n\r\"\\\x00\x1F\x7F\xFF");
  eq_o (String.Ascii.unescape_string "\\x61") (Some "a");
  eq_o (String.Ascii.unescape_string "\\x20") (Some " ");
  eq_o (String.Ascii.unescape_string "\\x2") None;
  eq_o (String.Ascii.unescape_string "\\x") None;
  eq_o (String.Ascii.unescape_string "\\") None;
  eq_o (String.Ascii.unescape_string "a\\b") (Some "a\b");
  eq_o (String.Ascii.unescape_string "a\\t") (Some "a\t");
  eq_o (String.Ascii.unescape_string "b\\n") (Some "b\n");
  eq_o (String.Ascii.unescape_string "b\\r") (Some "b\r");
  eq_o (String.Ascii.unescape_string "b\\\"") (Some "b\"");
  eq_o (String.Ascii.unescape_string "b\\\'") (Some "b'");
  eq_o (String.Ascii.unescape_string "b\\z") None;
  eq_o (String.Ascii.unescape_string "b\\1") None;
  ()

(* Uniqueness *)

let uniquify = test "String.uniquify" @@ fun () ->
  let eq = eq_list ~eq:(=) ~pp:pp_str in
  eq (String.uniquify []) [];
  eq (String.uniquify ["a";"b";"c"]) ["a";"b";"c"];
  eq (String.uniquify ["a";"a";"b";"c"]) ["a";"b";"c"];
  eq (String.uniquify ["a";"b";"a";"c"]) ["a";"b";"c"];
  eq (String.uniquify ["a";"b";"c";"a"]) ["a";"b";"c"];
  eq (String.uniquify ["b";"a";"b";"c"]) ["b";"a";"c"];
  eq (String.uniquify ["a";"b";"b";"c"]) ["a";"b";"c"];
  eq (String.uniquify ["a";"b";"c";"b"]) ["a";"b";"c"];
  ()

let suite = suite "String functions"
    [ misc;
      head;
      append;
      concat;
      is_empty;
      is_prefix;
      is_infix;
      is_suffix;
      for_all;
      exists;
      equal;
      compare;
      with_range;
      with_index_range;
      trim;
      span;
      cut;
      cuts;
      fields;
      find;
      find_sub;
      filter;
      map;
      iter;
      fold;
      ascii_is_valid;
      ascii_casing;
      ascii_escapes;
      ascii_unescapes;
      uniquify; ]

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
