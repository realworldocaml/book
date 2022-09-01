open! Import

type t =
  | Atom of
      { loc : Positions.range
      ; atom : string
      ; unescaped : string option
      }
  | List of
      { loc : Positions.range
      ; elements : t_or_comment list
      }

and t_or_comment =
  | Sexp of t
  | Comment of comment

and comment =
  | Plain_comment of
      { loc : Positions.range
      ; comment : string
      }
  | Sexp_comment of
      { hash_semi_pos : Positions.pos
      ; comments : comment list
      ; sexp : t
      }
[@@deriving_inline sexp_of]

let rec sexp_of_t =
  (function
    | Atom { loc = loc__002_; atom = atom__004_; unescaped = unescaped__006_ } ->
      let bnds__001_ = [] in
      let bnds__001_ =
        let arg__007_ = sexp_of_option sexp_of_string unescaped__006_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "unescaped"; arg__007_ ] :: bnds__001_
      in
      let bnds__001_ =
        let arg__005_ = sexp_of_string atom__004_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "atom"; arg__005_ ] :: bnds__001_
      in
      let bnds__001_ =
        let arg__003_ = Positions.sexp_of_range loc__002_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__003_ ] :: bnds__001_
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Atom" :: bnds__001_)
    | List { loc = loc__009_; elements = elements__011_ } ->
      let bnds__008_ = [] in
      let bnds__008_ =
        let arg__012_ = sexp_of_list sexp_of_t_or_comment elements__011_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "elements"; arg__012_ ] :: bnds__008_
      in
      let bnds__008_ =
        let arg__010_ = Positions.sexp_of_range loc__009_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__010_ ] :: bnds__008_
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "List" :: bnds__008_)
      : t -> Sexplib0.Sexp.t)

and sexp_of_t_or_comment =
  (function
    | Sexp arg0__013_ ->
      let res0__014_ = sexp_of_t arg0__013_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Sexp"; res0__014_ ]
    | Comment arg0__015_ ->
      let res0__016_ = sexp_of_comment arg0__015_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Comment"; res0__016_ ]
      : t_or_comment -> Sexplib0.Sexp.t)

and sexp_of_comment =
  (function
    | Plain_comment { loc = loc__018_; comment = comment__020_ } ->
      let bnds__017_ = [] in
      let bnds__017_ =
        let arg__021_ = sexp_of_string comment__020_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "comment"; arg__021_ ] :: bnds__017_
      in
      let bnds__017_ =
        let arg__019_ = Positions.sexp_of_range loc__018_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__019_ ] :: bnds__017_
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Plain_comment" :: bnds__017_)
    | Sexp_comment
        { hash_semi_pos = hash_semi_pos__023_
        ; comments = comments__025_
        ; sexp = sexp__027_
        } ->
      let bnds__022_ = [] in
      let bnds__022_ =
        let arg__028_ = sexp_of_t sexp__027_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "sexp"; arg__028_ ] :: bnds__022_
      in
      let bnds__022_ =
        let arg__026_ = sexp_of_list sexp_of_comment comments__025_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "comments"; arg__026_ ] :: bnds__022_
      in
      let bnds__022_ =
        let arg__024_ = Positions.sexp_of_pos hash_semi_pos__023_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "hash_semi_pos"; arg__024_ ] :: bnds__022_
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Sexp_comment" :: bnds__022_)
      : comment -> Sexplib0.Sexp.t)
;;

[@@@end]

let compare = Caml.compare
let compare_t_or_comment = Caml.compare
let compare_comment = Caml.compare

module Forget = struct
  (* In cps to prevent non-tail recursion.
     The polymorphism in the signature ensures that each function returns
     only through the continuation. *)
  module Cps : sig
    val forget_t : t -> (Sexp.t -> 'r) -> 'r
    val forget_toc : t_or_comment -> (Sexp.t option -> 'r) -> 'r
    val forget_tocs : t_or_comment list -> (Sexp.t list -> 'r) -> 'r
  end = struct
    let rec forget_t t k =
      match t with
      | Atom { atom; _ } -> k (Sexp.Atom atom)
      | List { elements; _ } -> forget_tocs elements (fun xs -> k (Sexp.List xs))

    and forget_tocs tocs k =
      match tocs with
      | [] -> k []
      | toc :: tocs ->
        forget_toc toc (function
          | None -> forget_tocs tocs k
          | Some x -> forget_tocs tocs (fun xs -> k (x :: xs)))

    and forget_toc toc k =
      match toc with
      | Comment _ -> k None
      | Sexp t -> forget_t t (fun x -> k (Some x))
    ;;
  end

  let t x = Cps.forget_t x (fun y -> y)
  let t_or_comment x = Cps.forget_toc x (fun y -> y)
  let t_or_comments x = Cps.forget_tocs x (fun y -> y)
end
