(* -*- encoding: iso-8859-1 -*- *)

(** ocaml lexical conventions
    (http://caml.inria.fr/pub/docs/manual-ocaml/lex.html)
*)

(*
 *** literals ***
*)

(* identifiers *)
let _id,
    iD',
    I9,
    _'i,
    A_',
    u',
    éçèæùà (* this file must be iso-8859-1 *)
  =
  _
;;

(* intergers *)
let _ =
  -1
  + 0
  + 10_
  + -0xAFfe_0 + 0X1_
  + 0O7_0_1_2 + -0o12__
  - 0B0_1_0 + -0b111_
;;

(* floats *)
let _ =
  0. +.
  0.0 +.
  0e12 +.
  0.e1_ +.
  999e+1 +.
  -9_99_E-0 +.
  -.12. +.
  0_._e-1_2
;;

(* chars *)
[ 'a';
  '&';
  'Ç';
  '§';
  '\\';
  '\"';
  '\'';
  '\b';
  '\234';
  '\999'; (* wrong, but yet... *)
  '\xAF' ]
;;

(* strings *)
let _ = "'a';\n\
        \ '&';\
         'Ç';\
         '§';\
         '\\';\
         '\"';\
         '\'';\
         '\b';\
         '\234';\
         '\999'; (* wrong, but yet... *)\
         '\xAF'"
;;

(* naming labels *)
val f :
  _l1 : int ->
  ? _' : float ->
  'a
let rec f
    ~ _l1 : int
    ? _' : float =
  f ~_l1: 0 ?_': 0e1
;;

(* prefix and infix symbols *)
_ = _
<:~ _
> _
  @ _
  ^$ _
     %%
;;

!! ( ????: _ )

(* keywords *)
(* don't care about indentation, just should'nt crash :) *)
and as assert asr begin class
constraint do done downto else end
exception external false for fun function
    functor if in include inherit initializer
  land lazy let lor lsl lsr
  lxor match method mod module mutable
  new object of open or private
  rec sig struct then to true
    try type val virtual when while
with
;;

(* line number directives *)
(* should be ignored and not indented: we may still want to indent generated
   code for readability *)
#9999 "bla.ml\ \n\999"
let _ =
#9999 "bla.ml\ \n\999"
  0
