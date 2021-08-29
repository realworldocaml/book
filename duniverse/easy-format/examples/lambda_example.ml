type lambda =
    Lambda of string * lambda
  | Var of string
  | Apply of lambda * lambda


(* We make the page more narrow for illustration purposes *)
let () = Format.set_margin 20



(* \n f x. n (\g h. h (g f)) (\u. x) (\u. u) *)
(* \n. \f. \x. n (\g. \h. h (g f)) (\u. x) (\u. u) *)

let sample_data =
  Lambda (
    "n",
    (Lambda (
       "f",
       (Lambda (
	  "x",
	  Apply (
	    Apply (
	      Apply (
		Var "n",
		(Lambda (
		   "g",
		   (Lambda (
		      "h",
		      Apply (Var "h", Apply (Var "g", Var "f"))
		    )
		   )
		 )
		)
	      ),
	      (Lambda ("u", Var "x"))
	    ),
	    (Lambda ("u", Var "u"))
	  )
	)
       )
     )
    )
  )


(****************************************************************************)
(* Example from http://caml.inria.fr/resources/doc/guides/format.html
   using Format directly. *)

open Format

let ident = pp_print_string;;
let kwd = pp_print_string;;

let rec pr_exp0 ppf = function
  | Var s ->  ident ppf s
  | lam -> fprintf ppf "@[<1>(%a)@]" pr_lambda lam

and pr_app ppf = function
  | e ->  fprintf ppf "@[<2>%a@]" pr_other_applications e

and pr_other_applications ppf f =
  match f with
  | Apply (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
  | f -> pr_exp0 ppf f

and pr_lambda ppf = function
 | Lambda (s, lam) ->
     fprintf ppf "@[<1>%a%a%a@ %a@]" kwd "\\" ident s kwd "." pr_lambda lam
 | e -> pr_app ppf e;;

let print_lambda x =
  pr_lambda std_formatter x;
  pp_print_flush std_formatter ()


let () =
  print_endline
    "Example from \
     http://caml.inria.fr/resources/doc/guides/format.html#example";
  print_lambda sample_data;
  print_newline ()



(***************************************************************************)
(* Same example, using Easy_format *)

open Printf
open Easy_format


let p1 = { label with indent_after_label = 1 }
let p2 = { label with indent_after_label = 2 }

let paren_style =
  { list with
      space_after_opening = false;
      space_before_closing = false;
      align_closing = false
  }

let rec exp0_node = function
    Var s -> Atom (s, atom)
  | lam -> List (("(", "", ")", paren_style), [lambda_node lam])

and app_node = function
    Apply (f, arg) -> Label ((app_node f, p2), exp0_node arg)
  | f -> exp0_node f

and lambda_node = function
    Lambda (s, lam) ->
      Label ((Atom (sprintf "\\%s." s, atom), p1), lambda_node lam)
  | e -> app_node e


let () =
  print_endline "Same, using Easy_format:";
  Pretty.to_stdout (lambda_node sample_data);
  print_newline ()
