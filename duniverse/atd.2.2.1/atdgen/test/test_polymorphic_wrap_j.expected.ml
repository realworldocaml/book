(* Auto-generated from "test_polymorphic_wrap.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type 'a t = 'a Array_wrap.t

let write__1 write__a = (
  Atdgen_runtime.Oj_run.write_list (
    write__a
  )
)
let string_of__1 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 write__a ob x;
  Bi_outbuf.contents ob
let read__1 read__a = (
  Atdgen_runtime.Oj_run.read_list (
    read__a
  )
)
let _1_of_string read__a s =
  read__1 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 write__a = (
  fun ob x -> (
    let x = ( Array_wrap.unwrap ) x in (
      write__1 write__a
    ) ob x)
)
let string_of__2 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 write__a ob x;
  Bi_outbuf.contents ob
let read__2 read__a = (
  fun p lb ->
    let x = (
      read__1 read__a
    ) p lb in
    ( Array_wrap.wrap ) x
)
let _2_of_string read__a s =
  read__2 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_t write__a = (
  write__2 write__a
)
let string_of_t write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t write__a ob x;
  Bi_outbuf.contents ob
let read_t read__a = (
  read__2 read__a
)
let t_of_string read__a s =
  read_t read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
