(* Auto-generated from "test_abstract.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type int_assoc_list = Testj.int_assoc_list

type any_items = Test_abstract_t.any_items

type any = Test_abstract_t.any

type 'x abs2 = 'x Testj.abs2

type 'x abs1 = 'x Testj.abs1

let write_int_assoc_list = (
  Testj.write_int_assoc_list
)
let string_of_int_assoc_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write_int_assoc_list ob x;
  Buffer.contents ob
let read_int_assoc_list = (
  Testj.read_int_assoc_list
)
let int_assoc_list_of_string s =
  read_int_assoc_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_json
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Buffer.create len in
  write__1 ob x;
  Buffer.contents ob
let read__1 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_json
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_any_items = (
  write__1
)
let string_of_any_items ?(len = 1024) x =
  let ob = Buffer.create len in
  write_any_items ob x;
  Buffer.contents ob
let read_any_items = (
  read__1
)
let any_items_of_string s =
  read_any_items (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs2 write__x = (
  Testj.write_abs2 write__x
)
let string_of_abs2 write__x ?(len = 1024) x =
  let ob = Buffer.create len in
  write_abs2 write__x ob x;
  Buffer.contents ob
let read_abs2 read__x = (
  Testj.read_abs2 read__x
)
let abs2_of_string read__x s =
  read_abs2 read__x (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs1 write__x = (
  Testj.write_abs1 write__x
)
let string_of_abs1 write__x ?(len = 1024) x =
  let ob = Buffer.create len in
  write_abs1 write__x ob x;
  Buffer.contents ob
let read_abs1 read__x = (
  Testj.read_abs1 read__x
)
let abs1_of_string read__x s =
  read_abs1 read__x (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
