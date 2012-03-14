open Core.Std

type t =  { bar: string;
            foo: int sexp_option;
            stuff : string list sexp_opaque;
          }
with sexp

let pr sexp_of_t t =
  Exn.handle_uncaught ~exit:true (fun () ->
    t |! sexp_of_t |! Sexp.to_string_hum |! print_endline)

let () =
  pr sexp_of_t
    { bar = "hello"; foo = None; stuff = [] }

let () =
  pr sexp_of_t 
    { bar = "hello"; foo = Some 7; stuff = [] }

type z = { zog: int -> int } with sexp

let () =
  pr sexp_of_z
    { zog = Fn.id }

type v =
| Foo of int sexp_opaque
| Bar of string
with sexp

let () =
  pr sexp_of_v (Foo 3)

let () =
  Exn.handle_uncaught ~exit:true
    (fun () ->
      "((bar hello)(stuff ()))"
      |! Sexp.of_string
      |! t_of_sexp
      |! ignore
    )
