(** Index. HTMLBook denotes index terms with a somewhat bulky
    notation:

    {v <a data-type="indexterm" data-primary="foo" data-secondary="bar"> V}

    We support a lighter-weight notation:

    {v <idx>foo/bar</idx> v}

    This module supports conversion between the two forms.
*)
open! Core

val indexterm_to_idx : Html.t -> Html.t
val idx_to_indexterm : Html.t -> Html.t
