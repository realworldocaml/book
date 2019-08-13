(* packaging of annotated sexp functions *)

open Sexplib

module M = Sexp.With_layout

let%test_module "forget" =
  (module struct

    (* dummies *)
    let dumb_pos = {M.row = 0; col = 0}
    let dumb_comment = M.Plain_comment (dumb_pos, "comment")

    let atom x = M.Sexp (M.Atom (dumb_pos, x, None))
    let list ts = M.Sexp (M.List (dumb_pos, ts, dumb_pos))
    let comment = M.Comment dumb_comment

    let a1 = comment
    let a2 = atom "hello"
    let a3 = list [comment; atom "yo"; comment; atom "yo"; comment; atom "ma"; comment]
    let a4 = list [comment; a1; comment; a2; comment; a3; comment; a3]
    let a5 = list [comment; a1; comment; a2; comment; a3; comment; a4]

    let b2 = Sexp.Atom "hello"
    let b3 = Sexp.List [Sexp.Atom "yo"; Sexp.Atom "yo"; Sexp.Atom "ma"]
    let b4 = Sexp.List [b2; b3; b3]
    let b5 = Sexp.List [b2; b3; b4]

    let%test _ = M.Forget.t_or_comment a1 = None
    let%test _ = M.Forget.t_or_comment a2 = Some b2
    let%test _ = M.Forget.t_or_comment a3 = Some b3
    let%test _ = M.Forget.t_or_comment a4 = Some b4
    let%test _ = M.Forget.t_or_comment a5 = Some b5

    module Simple_forget = struct
      let rec t = function
        | M.Atom (_, x, _) -> Sexp.Atom x
        | M.List (_, ts, _) -> Sexp.List (t_or_comments ts)
      and t_or_comments = function
        | [] -> []
        | t :: ts ->
          match t_or_comment t with
          | None -> t_or_comments ts
          | Some s -> s :: t_or_comments ts
      and t_or_comment = function
        | M.Sexp x -> Some (t x)
        | M.Comment _ -> None
    end

    let same_as_simple x = M.Forget.t_or_comment x = Simple_forget.t_or_comment x

    let%test _ = same_as_simple a1
    let%test _ = same_as_simple a2
    let%test _ = same_as_simple a3
    let%test _ = same_as_simple a4
    let%test _ = same_as_simple a5

  end)

