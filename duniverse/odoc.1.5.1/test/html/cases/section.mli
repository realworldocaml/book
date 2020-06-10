(* Blank lines are needed because of
   https://caml.inria.fr/mantis/view.php?id=7701. *)

(** This is the module comment. Eventually, sections won't be allowed in it. *)

(** {1 Empty section} *)

(** {1 Text only}

    Foo bar. *)

(** {1 Aside only} *)

(** Foo bar. *)

(** {1 Value only} *)

val foo : unit

(** {1 Empty section}

    {1 within a comment}

    {2 and one with a nested section} *)

(** {1 {e This} [section] {b title} {_has} {^markup}}

    But links are impossible thanks to the parser, so we never have trouble
    rendering a section title in a table of contents – no link will be nested
    inside another link. *)
