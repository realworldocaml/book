(** Type of annotated S-expressions *)

module List = struct
  let map t ~f = List.rev (List.rev_map f t)
end

module Make (Pos : sig type t val sexp_of_t : t -> Type.t end) = struct

  module T = struct
    type t =
      | Atom of Pos.t * string * string option (* second is quoted representation *)
      | List of Pos.t * t_or_comment list * Pos.t (* positions of left and right parens *)
    and t_or_comment =
      | Sexp of t
      | Comment of comment
    and comment =
      | Plain_comment of Pos.t * string
      | Sexp_comment of Pos.t * comment list * t
  end

  include T

  module type S = sig
    include module type of T
    val sexp_of_t : t -> Type.t
    val sexp_of_comment : comment -> Type.t
    val sexp_of_t_or_comment : t_or_comment -> Type.t
  end

  module To_sexp : sig
    val of_t : t -> Type.t
    val of_comment : comment -> Type.t
    val of_t_or_comment : t_or_comment -> Type.t
  end = struct

    (* maybe we can actually use conv here, instead of inlining it *)

    let of_pos = Pos.sexp_of_t

    let of_string x = Type.Atom x
    let of_list of_a xs = Type.List (List.map ~f:of_a xs)

    let of_option of_a = function
      | Some x -> Type.List [of_a x]
      | None -> Type.List []

    let rec of_t = function
      | Atom (v1, v2, v3) ->
        Type.List [Type.Atom "Atom"; of_pos v1; of_string v2; of_option of_string v3]
      | List (v1, v2, v3) ->
        Type.List [Type.Atom "List"; of_pos v1; of_list of_t_or_comment v2; of_pos v3]

    and of_t_or_comment = function
      | Sexp t -> Type.List [Type.Atom "Sexp"; of_t t]
      | Comment c -> Type.List [Type.Atom "Comment"; of_comment c]

    and of_comment = function
      | Plain_comment (v1, v2) ->
        Type.List [Type.Atom "Plain_comment"; of_pos v1; of_string v2]
      | Sexp_comment (v1, v2, v3) ->
        Type.List [Type.Atom "Sexp_comment"; of_pos v1; of_list of_comment v2; of_t v3]
  end

  let sexp_of_t            = To_sexp.of_t
  let sexp_of_comment      = To_sexp.of_comment
  let sexp_of_t_or_comment = To_sexp.of_t_or_comment

end

include Make (Src_pos.Relative)

module Parsed = Make (Src_pos.Absolute)

let relativize =
  let rel ~outer_p p = Src_pos.Absolute.diff p outer_p in
  let rec aux_t ~outer_p = function
    | Parsed.Atom (pos, s, sopt) -> Atom (rel pos ~outer_p, s, sopt)
    | Parsed.List (start_pos, tocs, end_pos) ->
      List
        ( rel start_pos ~outer_p
        , List.map tocs ~f:(fun toc -> aux_toc ~outer_p:start_pos toc)
        , rel end_pos ~outer_p )
  and aux_toc ~outer_p = function
    | Parsed.Sexp    t -> Sexp    (aux_t t ~outer_p)
    | Parsed.Comment c -> Comment (aux_c c ~outer_p)
  and aux_c ~outer_p = function
    | Parsed.Plain_comment (pos, txt) -> Plain_comment (rel pos ~outer_p, txt)
    | Parsed.Sexp_comment (pos, cs, t) ->
      Sexp_comment
        ( rel pos ~outer_p
        , List.map cs ~f:(fun c -> aux_c ~outer_p c)
        , aux_t t ~outer_p )
  in
  fun toc ->
    aux_toc toc ~outer_p:Src_pos.Absolute.origin

