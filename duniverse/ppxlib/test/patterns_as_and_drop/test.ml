[@@@ocamlformat "disable"]

open Ppxlib

(* Simple demo without [as__] and [drop] *)
let ___1 =
  let loc = Location.none in
  let ast = [%expr List.length xs = 0] in
  let pat =
    let open Ast_pattern in
    let length () =
      pexp_apply
        (pexp_ident (ldot (lident (string "List")) (string "length")))
        ((nolabel ** __) ^:: nil)
    in
    let zero () = pexp_constant (pconst_integer (string "0") none) in

    pexp_apply
      (pexp_ident (lident (string "=")))
      ((nolabel ** length ()) ^:: (nolabel ** zero ()) ^:: nil)
  in

  Ast_pattern.parse pat loc ast
    ~on_error:(fun () -> "Error")
    (fun _length_argument -> "Success. As expected")

[%%expect{|
val ___1 : string = "Success. As expected"
|}]

(* We could use [as__] to capture whole [List.length ...] expression,
   and use [drop] to ignore length's argument *)
let ___2 =
  let loc = Location.none in
  let ast = [%expr List.length xs = 0] in

  let pat =
    let open Ast_pattern in
    let length () =
      as__
        (pexp_apply
           (pexp_ident (ldot (lident (string "List")) (string "length")))
           ((nolabel ** drop) ^:: nil))
    in
    let zero () = as__ (pexp_constant (pconst_integer (string "0") none)) in

    pexp_apply
      (pexp_ident (lident (string "=")))
      ((nolabel ** length ()) ^:: (nolabel ** zero ()) ^:: nil)
  in

  Ast_pattern.parse pat loc ast
    ~on_error:(fun () -> "error")
    (fun l r ->
      Format.asprintf "Success with '%a' and '%a'. As expected" Pprintast.expression l Pprintast.expression r
      )

[%%expect{|
val ___2 : string = "Success with 'List.length xs' and '0'. As expected"
|}]

(* Pitfall. If we forget unit argument and will use [as__], the success case
   will be fired before the error case. *)
let ___3 =
  let loc = Location.none in
  let ast = [%expr 1] in

  let pat () =
    let open Ast_pattern in
    as__ (pexp_constant (pconst_integer (string "0") none))
  in
  let rez = Buffer.create 100 in
  Ast_pattern.parse (pat ()) loc ast
    ~on_error:(fun () -> Printf.bprintf rez "An error")
    (fun zero_expr -> Buffer.add_string rez
      (Format.asprintf "Successfully got '%a' but error right after that (NOT EXPECTED). " Pprintast.expression zero_expr));
  Buffer.contents rez

[%%expect{|
val ___3 : string =
  "Successfully got '1' but error right after that (NOT EXPECTED). An error"
|}]

(* To avoid the pitfall above we could add extra () to delay evaluation *)
let ___4 =
  let loc = Location.none in
  let ast = [%expr 1] in

  let pat () =
    let open Ast_pattern in
    as__ (pexp_constant (pconst_integer (string "0") none))
  in
  Ast_pattern.parse (pat ()) loc ast
    ~on_error:(fun () () -> "Error, as expected")
    (fun _zero_expr () -> "Success and error after that\n%!")
    ()

[%%expect{|
val ___4 : string = "Error, as expected"
|}]

(* But this pitfall is not introduced by [as__], it existed before too. *)
let ___5 =
  let loc = Location.none in
  let ast = [%expr string_of_int 43] in
  let pat =
    let open Ast_pattern in
    pexp_apply
      (pexp_ident (lident __))
      ((nolabel ** eint (int 42)) ^::
       (nolabel ** (pexp_ident (lident __))) ^::
       nil)
  in

  let b = Buffer.create 10 in
  let () = Ast_pattern.parse pat loc ast
    ~on_error:(fun () -> Buffer.add_string b "It's an error")
    (fun s ->
        Printf.bprintf b "Partial success with '%s', but actually not, because... " s;
        (fun _ -> Printf.bprintf b "no, it's total success"))
    in
  Buffer.contents b

[%%expect{|
val ___5 : string =
  "Partial success with 'string_of_int', but actually not, because... It's an error"
|}]
