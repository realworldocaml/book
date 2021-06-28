(* packaging of annotated sexp functions *)

module List = struct
  let iter t ~f = List.iter f t
  let map t ~f = List.rev (List.rev_map f t)
end

include Type_with_layout

type pos = Src_pos.Relative.t = { row : int; col : int }
let sexp_of_pos = Src_pos.Relative.sexp_of_t

module Lexer = struct
  let main = Lexer.main_with_layout
end

module Parser = Parser_with_layout

module Render = struct

  module Rel_pos = Src_pos.Relative
  module Abs_pos = Src_pos.Absolute

  type last_atom = {
    immed_after : Abs_pos.t;
    unescaped : bool;
  }

  type state = {
    mutable row_shift : Rel_pos.t;
    mutable current : Abs_pos.t;
    mutable last_atom : last_atom option;
    mutable last_comment_row : int;
  }

  (* the point of [immed_after_last_atom] is to prevent
     (A B C) from rendering as (A BBC) after we replace B with BB *)

  type 'a t = (char -> unit) -> state -> 'a

  let return a _putc _st = a

  let bind m ~f putc st = f (m putc st) putc st

  let run putc m =
    m putc {
      row_shift = Rel_pos.zero;
      current = Abs_pos.origin;
      last_atom = None;
      last_comment_row = 0; (* before the file starts *)
    }

  let emit_char putc st c =
    let {Abs_pos.col; row} = st.current in
    putc c;
    if c = '\n' then
      st.current <- {Abs_pos.row = 1 + row; col = 1}
    else
      st.current <- {Abs_pos.row; col = 1 + col}

  let emit_string putc st str =
    let n = String.length str in
    for i = 0 to n - 1 do
      emit_char putc st str.[i]
    done

  let emit_chars putc st c ~n =
    emit_string putc st (String.make n c)

  let advance putc ~anchor st ~by:delta ~unescaped_atom ~line_comment =
    let new_pos = Abs_pos.add (Abs_pos.add anchor delta) st.row_shift in
    let need_to_leave_room_between_two_unescaped_atoms_lest_they_become_one =
      unescaped_atom && begin
        match st.last_atom with
        | Some {immed_after; unescaped = prev_unescaped} ->
          new_pos = immed_after && prev_unescaped
        | None -> false
      end
    in
    (* avoid joining subsequent items into a preceding line comment *)
    let need_to_clear_line_comment = (new_pos.row = st.last_comment_row) in
    let need_to_reposition =
      not (Abs_pos.geq new_pos st.current)
      || need_to_clear_line_comment
      || need_to_leave_room_between_two_unescaped_atoms_lest_they_become_one
    in
    let (row_delta, new_pos) =
      if need_to_reposition then begin
        (* repositioning heuristic: just move to the next fresh row *)
        let new_row = 1 + st.current.Abs_pos.row in
        let row_delta = new_row - new_pos.Abs_pos.row in
        (row_delta, {Abs_pos.row = new_row; col = new_pos.Abs_pos.col})
      end else
        (0, new_pos)
    in
    begin (* advance to new_pos by emitting whitespace *)
      if new_pos.Abs_pos.row > st.current.Abs_pos.row then begin
        let n = (new_pos.Abs_pos.row - st.current.Abs_pos.row) in
        emit_chars putc st '\n' ~n
      end;
      if new_pos.Abs_pos.col > st.current.Abs_pos.col then begin
        let n = (new_pos.Abs_pos.col - st.current.Abs_pos.col) in
        emit_chars putc st ' ' ~n
      end;
    end;
    assert (new_pos = st.current);
    if line_comment then (
      st.last_comment_row <- st.current.row
    );
    st.row_shift <- {
      st.row_shift with Rel_pos.
                     row = st.row_shift.Rel_pos.row + row_delta;
    }

  let rec render_t putc ~anchor (st : state) t =
    match t with
    | Atom (delta, text, fmt_text) ->
      let fmt_text =
        match fmt_text with
        | None | Some "" -> Pre_sexp.mach_maybe_esc_str text
        | Some text -> text
      in
      let unescaped = fmt_text.[0] <> '"' in
      advance putc st ~by:delta ~anchor ~unescaped_atom:unescaped ~line_comment:false;
      emit_string putc st fmt_text;
      st.last_atom <- Some { immed_after = st.current; unescaped; };
    | List (start_delta, tocs, end_delta) ->
      advance putc st ~by:start_delta ~anchor ~unescaped_atom:false ~line_comment:false;
      let child_anchor = Abs_pos.sub st.current st.row_shift in
      emit_char putc st '(';
      List.iter tocs ~f:(fun toc -> render_toc putc ~anchor:child_anchor st toc);
      advance putc st ~by:end_delta ~anchor ~unescaped_atom:false ~line_comment:false;
      emit_char putc st ')';
      ()

  and render_toc putc ~anchor st = function
    | Sexp t -> render_t putc ~anchor st t
    | Comment c -> render_c putc ~anchor st c

  and render_c putc ~anchor st = function
    | Plain_comment (delta, text) ->
      let line_comment = String.length text > 0 && text.[0] = ';' in
      advance putc st ~by:delta ~anchor ~unescaped_atom:false ~line_comment;
      emit_string putc st text
    | Sexp_comment (delta, cs, t) ->
      advance putc st ~by:delta ~anchor ~unescaped_atom:false ~line_comment:false;
      emit_string putc st "#;";
      List.iter cs ~f:(render_c putc ~anchor st);
      render_t putc ~anchor st t

  let render asexp putc st = render_toc putc ~anchor:Abs_pos.origin st asexp

  let sexp = render

end

module Forget = struct

  (* In cps to prevent non-tail recursion.
     The polymorphism in the signature ensures that each function returns
     only through the continuation. *)
  module Cps : sig
    val forget_t    : t                 -> (Type.t        -> 'r) -> 'r
    val forget_toc  : t_or_comment      -> (Type.t option -> 'r) -> 'r
    val forget_tocs : t_or_comment list -> (Type.t list   -> 'r) -> 'r
  end = struct

    let rec forget_t t k =
      match t with
      | Atom (_, x, _) -> k (Type.Atom x)
      | List (_, tocs, _) -> forget_tocs tocs (fun xs -> k (Type.List xs))

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
  end

  let t             x = Cps.forget_t    x (fun y -> y)
  let t_or_comment  x = Cps.forget_toc  x (fun y -> y)
  let t_or_comments x = Cps.forget_tocs x (fun y -> y)
end
