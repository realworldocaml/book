open Base

type 'a test_pred
  = ?here:Lexing.position list
  -> ?message:string
  -> ('a -> bool)
  -> 'a
  -> unit

type 'a test_eq
  = ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> 'a
  -> 'a
  -> unit

type 'a test_result
   = ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> expect:'a
  -> 'a
  -> unit

exception E of string * Sexp.t [@@deriving sexp]

let failwith message sexp = raise (E (message, sexp))

let fail_in_sexp_style ~message ~pos ~here ~tag body =
  let message =
    match message with
    | None -> tag
    | Some s -> s ^ ": " ^ tag
  in
  let sexp =
    Sexp.List (
      body
      @ [ Sexp.List [ Sexp.Atom "Loc"; Sexp.Atom pos ] ]
      @ begin match here with
        | [] -> []
        | _ -> [ Sexp.List [ Sexp.Atom "Stack"
                           ; [%sexp_of: Source_code_position.t list] here
                           ] ]
        end
    )
  in
  failwith message sexp

let [@inline never] test_pred_failed ~message ~pos ~here ~sexpifier t =
  fail_in_sexp_style ~message ~pos ~here ~tag:"predicate failed" [
    Sexp.List [Sexp.Atom "Value"; sexpifier t]
  ]

let test_pred ~pos ~sexpifier ~here ?message predicate t =
  if not (predicate t) then
    test_pred_failed ~message ~pos ~here ~sexpifier t

let r_diff : (from_:string -> to_:string -> unit) option ref = ref None
let set_diff_function f = r_diff := f

let [@inline never] test_result_or_eq_failed ~sexpifier ~expect ~got =
  let got = sexpifier got in
  let expect = sexpifier expect in
  begin match !r_diff with
  | None -> ()
  | Some diff ->
    let from_ = Sexp.to_string_hum expect in
    let to_   = Sexp.to_string_hum got in
    diff ~from_ ~to_
  end;
  `Fail (expect, got)

let test_result_or_eq ~sexpifier ~comparator ?equal ~expect ~got =
  let pass =
    match equal with
    | None -> comparator got expect = 0
    | Some f -> f got expect
  in
  if pass
  then `Pass
  else test_result_or_eq_failed ~sexpifier ~expect ~got

let [@inline never] test_eq_failed ~message ~pos ~here ~t1 ~t2 =
  fail_in_sexp_style ~message ~pos ~here ~tag:"comparison failed" [
    t1;
    Sexp.Atom "vs";
    t2;
  ]

let test_eq ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2 =
  match test_result_or_eq ~sexpifier ~comparator ?equal ~expect:t1 ~got:t2 with
  | `Pass -> ()
  | `Fail (t1, t2) -> test_eq_failed ~message ~pos ~here ~t1 ~t2

let [@inline never] test_result_failed ~message ~pos ~here ~expect ~got =
  fail_in_sexp_style ~message ~pos ~here ~tag:"got unexpected result" [
    Sexp.List [Sexp.Atom "expected"; expect];
    Sexp.List [Sexp.Atom "got"; got];
  ]

let test_result ~pos ~sexpifier ~comparator ~here ?message ?equal ~expect ~got =
  match test_result_or_eq ~sexpifier ~comparator ?equal ~expect ~got with
  | `Pass -> ()
  | `Fail (expect, got) -> test_result_failed ~message ~pos ~here ~expect ~got
