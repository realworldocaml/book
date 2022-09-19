open! Core

(** Runs the patdiff algorithm on its inputs to compute the diff to go from the first
    string to the second string. Produces a multi-line ASCII diff. If specified,
    [~context] overrides the default number of lines of context for each hunk. [keep_ws]
    overrides the default behavior for whether to consider whitespace in diffs. *)
val patdiff
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> string
  -> string
  -> string

(** Like [patdiff], but for s-expressions.  Formats the s-expressions the same way as
    [print_s] from [Expect_test_helpers_base], then calls [patdiff]. *)
val patdiff_s
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> Sexp.t
  -> Sexp.t
  -> string

(** Like [patdiff], but prints the result to stdout. *)
val print_patdiff
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> string
  -> string
  -> unit

(** Like [patdiff_s], but prints the result to stdout. *)
val print_patdiff_s
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> Sexp.t
  -> Sexp.t
  -> unit

(** Produces a stateful function that prints the diff between the previous string and the
    current one.

    Prints the entirety of the first string it is given, whether that is via the
    [string option] argument before unstaging, or via the callback.
    If the initial string is [Some x]:  Prints the entirety of [x] immediately.
    If the initial string is [None]: Prints the entirety of the first string passed to the callback.
*)
val diff_printer
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> string option
  -> (string -> unit) Staged.t

(** Like [diff_printer], but for s-expressions.  Formats the s-expressions the same way as
    [print_s] from [Expect_test_helpers_base], then calls [diff_printer].

    Prints the entirety of the first sexp it is given, whether that is via the
    [sexp option] argument before unstaging, or via the callback.
    If the initial sexp is [Some x]:  Prints the entirety of [x] immediately.
    If the initial sexp is [None]: Prints the entirety of the first sexp passed to the callback.
*)
val diff_printer_s
  :  ?location_style:Patdiff_kernel.Format.Location_style.t
  -> ?context:int
  -> ?keep_ws:bool
  -> Sexp.t option
  -> (Sexp.t -> unit) Staged.t
