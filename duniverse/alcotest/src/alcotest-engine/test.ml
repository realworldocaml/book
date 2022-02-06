(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type TESTABLE = sig
  type t

  val pp : t Fmt.t
  val equal : t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

let pp (type a) (t : a testable) =
  let (module T) = t in
  T.pp

let equal (type a) (t : a testable) =
  let (module T) = t in
  T.equal

let isnan f = FP_nan = classify_float f

let testable (type a) (pp : a Fmt.t) (equal : a -> a -> bool) : a testable =
  let module M = struct
    type t = a

    let pp = pp
    let equal = equal
  end in
  (module M)

let int32 = testable Fmt.int32 ( = )
let int64 = testable Fmt.int64 ( = )
let int = testable Fmt.int ( = )

let float eps =
  let same x y =
    (isnan x && isnan y)
    (* compare infinities *)
    || x = y
    || abs_float (x -. y) <= eps
  in
  testable Fmt.float same

let char =
  let pp_char ppf x = Fmt.pf ppf "%C" x in
  testable pp_char ( = )

let string =
  let pp_string ppf x = Fmt.pf ppf "%S" x in
  testable pp_string ( = )

let bytes =
  testable (fun fmt bytes -> Fmt.fmt "%S" fmt (Bytes.to_string bytes)) ( = )

let bool = testable Fmt.bool ( = )
let unit = testable (Fmt.any "()") ( = )

let list e =
  let rec eq l1 l2 =
    match (l1, l2) with
    | x :: xs, y :: ys -> equal e x y && eq xs ys
    | [], [] -> true
    | _ -> false
  in
  testable (Fmt.Dump.list (pp e)) eq

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable (pp l) eq

let array e =
  let eq a1 a2 =
    let m, n = Array.(length a1, length a2) in
    let rec go i = i = m || (equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0
  in
  testable (Fmt.Dump.array (pp e)) eq

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable (Fmt.Dump.pair (pp a) (pp b)) eq

let triple a b c =
  let eq (a1, b1, c1) (a2, b2, c2) =
    equal a a1 a2 && equal b b1 b2 && equal c c1 c2
  in
  let pp =
    Fmt.(
      parens
        (using (fun (x, _, _) -> x) (box (pp a))
        ++ comma
        ++ using (fun (_, x, _) -> x) (box (pp b))
        ++ comma
        ++ using (fun (_, _, x) -> x) (box (pp c))))
  in
  testable pp eq

let option e =
  let eq x y =
    match (x, y) with
    | Some a, Some b -> equal e a b
    | None, None -> true
    | _ -> false
  in
  testable (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y =
    match (x, y) with
    | Ok x, Ok y -> equal a x y
    | Error x, Error y -> equal e x y
    | _ -> false
  in
  testable (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable pp ( = )

let pass (type a) =
  let module M = struct
    type t = a

    let pp fmt _ = Fmt.string fmt "Alcotest.pass"
    let equal _ _ = true
  end in
  (module M : TESTABLE with type t = M.t)

let reject (type a) =
  let module M = struct
    type t = a

    let pp fmt _ = Fmt.string fmt "Alcotest.reject"
    let equal _ _ = false
  end in
  (module M : TESTABLE with type t = M.t)

let show_assert = function
  | "" -> ()
  | msg ->
      Fmt.(flush stdout) () (* Flush any test stdout preceding the assert *);
      Format.eprintf "%a %s\n%!" Pp.tag `Assert msg

let check_err fmt = raise (Core.Check_error fmt)

module Source_code_position = struct
  type here = Lexing.position
  type pos = string * int * int * int
end

type 'a extra_info =
  ?here:Source_code_position.here -> ?pos:Source_code_position.pos -> 'a

let pp_location =
  let pp =
    Fmt.styled `Bold (fun ppf (f, l, c) ->
        Fmt.pf ppf "File \"%s\", line %d, character %d:@," f l c)
  in
  fun ?here ?pos ppf ->
    match (here, pos) with
    | Some (here : Source_code_position.here), _ ->
        pp ppf (here.pos_fname, here.pos_lnum, here.pos_cnum - here.pos_bol)
    | _, Some (fname, lnum, cnum, _) -> pp ppf (fname, lnum, cnum)
    | None, None -> ()

let check (type a) ?here ?pos (t : a testable) msg (expected : a) (actual : a) =
  show_assert msg;
  if not (equal t expected actual) then
    let open Fmt in
    let s = const string in
    let pp_error =
      match msg with
      | "" -> nop
      | _ -> const Pp.tag `Fail ++ s (" " ^ msg) ++ cut
    and pp_expected ppf () =
      Fmt.pf ppf "   Expected: `%a'" (styled `Green (pp t)) expected;
      Format.pp_print_if_newline ppf ();
      Fmt.cut ppf ();
      ()
    and pp_actual ppf () =
      Fmt.pf ppf "   Received: `%a'" (styled `Red (pp t)) actual
    in
    raise
      (Core.Check_error
         Fmt.(
           vbox
             ((fun ppf () -> pp_location ?here ?pos ppf)
             ++ pp_error
             ++ cut
             ++ pp_expected
             ++ cut
             ++ pp_actual)
           ++ cut))

let check' ?here ?pos t ~msg ~expected ~actual =
  check ?here ?pos t msg expected actual

let fail ?here ?pos msg =
  show_assert msg;
  check_err (fun ppf () ->
      Fmt.pf ppf "%t%a %s" (pp_location ?here ?pos) Pp.tag `Fail msg)

let failf ?here ?pos fmt = Fmt.kstr (fun msg -> fail ?here ?pos msg) fmt
let neg t = testable (pp t) (fun x y -> not (equal t x y))

let collect_exception f =
  try
    f ();
    None
  with e -> Some e

let check_raises ?here ?pos msg exn f =
  show_assert msg;
  match collect_exception f with
  | None ->
      check_err (fun ppf () ->
          Fmt.pf ppf "%t%a %s: expecting %s, got nothing."
            (pp_location ?here ?pos) Pp.tag `Fail msg (Printexc.to_string exn))
  | Some e ->
      if e <> exn then
        check_err (fun ppf () ->
            Fmt.pf ppf "%t%a %s: expecting %s, got %s." (pp_location ?here ?pos)
              Pp.tag `Fail msg (Printexc.to_string exn) (Printexc.to_string e))

let () = at_exit (Format.pp_print_flush Format.err_formatter)
