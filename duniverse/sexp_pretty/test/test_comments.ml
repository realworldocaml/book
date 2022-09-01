(** We have two kinds of tests:

    1. Handwritten tests showing the pretty strings for some known examples.

    2. Quickcheck tests which round-trip a sexp through pretty-printing and assert that
    the comments are preserved up to some post-processing of whitespace.

    The post-processing is applied to both the original comments and the pretty comments.
    It describes how closely we can look before we see how pretty-printing has changed
    comments.
*)

open! Import
open! Expect_test_helpers_core
open! Sexp_pretty

module Comment_print_style = struct
  type t = Config.comment_print_style =
    | Pretty_print
    | Conservative_print
  [@@deriving enumerate, sexp_of]

  let to_string t = Sexp.to_string_mach ([%sexp_of: t] t)
end

type t = Sexp.With_layout.t_or_comment list [@@deriving sexp_of]

let get_config ~comment_print_style =
  Config.update
    ~color:false
    { Config.default with
      comments =
        (match Config.default.comments with
         | Drop -> raise_s [%message "Expected default to be Print"]
         | Print (comment_indent, color_option, _) ->
           Print (comment_indent, color_option, comment_print_style))
    }
;;

let of_string s : t =
  match
    Lexing.from_string s
    |> Sexplib.Sexp.With_layout.Parser.sexps Sexplib.Sexp.With_layout.Lexer.main
  with
  | exception exn -> raise_s [%message "sexps_with_layout_of_string" (exn : exn) s]
  | t -> t
;;

let pretty_string comment_print_style list =
  let config = get_config ~comment_print_style in
  List.map list ~f:(Sexp_with_layout.pretty_string config) |> String.concat ~sep:" "
;;

let%expect_test "leading and trailing whitespace" =
  let module Example = struct
    type t =
      { leading : bool
      ; trailing : bool
      }
    [@@deriving enumerate, sexp_of]

    let to_string { leading; trailing } =
      let space_if bool = if bool then " " else "" in
      [%string {|#|%{space_if leading}a%{space_if trailing}|#|}]
    ;;

    let to_string_pretty t ~style =
      t
      |> to_string
      |> of_string
      |> pretty_string style
      (* Visualize the whitespace *)
      |> String.tr ~target:' ' ~replacement:'.'
    ;;

    let columns =
      Ascii_table_kernel.Column.create "comment" to_string
      :: List.map [%all: Comment_print_style.t] ~f:(fun style ->
        Ascii_table_kernel.Column.create
          (Comment_print_style.to_string style)
          (to_string_pretty ~style))
    ;;
  end
  in
  let ascii_table columns rows =
    let screen =
      Ascii_table_kernel.draw columns rows ~prefer_split_on_spaces:false
      |> Option.value_exn ~here:[%here]
    in
    Ascii_table_kernel.Screen.to_string
      screen
      ~bars:`Unicode
      ~string_with_attr:(fun _ s -> s)
  in
  ascii_table Example.columns [%all: Example.t] |> print_endline;
  [%expect
    {|
    ┌─────────┬──────────────┬────────────────────┐
    │ comment │ Pretty_print │ Conservative_print │
    ├─────────┼──────────────┼────────────────────┤
    │ #|a|#   │ #|.a.|#      │ #|a|#              │
    │ #| a|#  │ #|.a.|#      │ #|.a|#             │
    │ #|a |#  │ #|.a.|#      │ #|a.|#             │
    │ #| a |# │ #|.a.|#      │ #|.a.|#            │
    └─────────┴──────────────┴────────────────────┘ |}]
;;

let%expect_test "examples" =
  let test s =
    let t = of_string s in
    List.iter [%all: Comment_print_style.t] ~f:(fun style ->
      let s = pretty_string style t in
      print_endline [%string {|%{style#Comment_print_style}: %{s}|}])
  in
  (* multiple internal whitespace *)
  test {| #| a  b |# |};
  [%expect {|
    Pretty_print: #| a b |#

    Conservative_print: #| a  b |# |}];
  (* multiple lines *)
  test {| #| a
b
 c

   d |# |};
  [%expect
    {|
    Pretty_print: #| a b c d |#

    Conservative_print: #| a
    b
     c

       d |# |}];
  (* really long comment *)
  test
    {| #| really long comment to see if sexp pretty printing will ever add new line breaks |# |};
  [%expect
    {|
    Pretty_print: #|
       really long comment to see if sexp pretty printing will ever add new line
       breaks
    |#

    Conservative_print: #| really long comment to see if sexp pretty printing will ever add new line breaks |# |}];
  (* comments nested in sexp comment *)
  test
    {| #;
    ; plain comment
    #| block comment |#
    (sexp
with
#| nested comment |#
) |};
  [%expect
    {|
    Pretty_print: #;
    ; plain comment#| block comment |#
    (sexp with #| nested comment |#)

    Conservative_print: #;
    ; plain comment#| block comment |#
    (sexp with #| nested comment |#) |}];
  (* quoted newline *)
  test {| #| "quoted
newline" |# |};
  [%expect
    {|
    Pretty_print: #| "quoted newline" |#

    Conservative_print: #| "quoted
    newline" |# |}];
  ignore ()
;;

let%expect_test "sticky comments" =
  let test s =
    let t = of_string s in
    Config.[ Before; After; Same_line ]
    |> List.iter ~f:(fun sticky ->
      let label = [%sexp_of: Config.sticky_comments] sticky |> Sexp.to_string in
      print_endline [%string {|%{label}:|}];
      List.iter [%all: Comment_print_style.t] ~f:(fun comment_print_style ->
        let s =
          let config = get_config ~comment_print_style in
          let config = { config with Config.sticky_comments = sticky } in
          List.map t ~f:(Sexp_with_layout.pretty_string config)
          |> String.concat ~sep:" "
        in
        print_endline [%string "%{comment_print_style#Comment_print_style}:\n%{s}"]))
  in
  test
    {|
 (a
    ;; comment 1
    ;; comment 2

    (b c) ;; comment 3
    ( ;; comment 4
      d ;; comment 5

    #| multi line
    block
    comment
    |#
    ()
    ))
|};
  [%expect
    {|
    Before:
    Pretty_print:
    (a
      ;; comment 1
      ;; comment 2
      ;; comment 3
      (b c)
      (;; comment 4
       ;; comment 5
       d
       #| multi line block comment |#
       ()))

    Conservative_print:
    (a
      ;; comment 1
      ;; comment 2
      ;; comment 3
      (b c)
      (;; comment 4
       ;; comment 5
       d
       #| multi line
        block
        comment
        |#
    ()))

    After:
    Pretty_print:
    (a
      ;; comment 1
      ;; comment 2
      (b c)
      ;; comment 3
      (;; comment 4
       d
       ;; comment 5
       #| multi line block comment |#
       ()))

    Conservative_print:
    (a
      ;; comment 1
      ;; comment 2
      (b c)
      ;; comment 3
      (;; comment 4
       d
       ;; comment 5
       #| multi line
        block
        comment
        |#
    ()))

    Same_line:
    Pretty_print:
    (a
      ;; comment 1
      ;; comment 2
      (b c) ;; comment 3
      (;; comment 4
       d ;; comment 5
       #| multi line block comment |#
       ()))

    Conservative_print:
    (a
      ;; comment 1
      ;; comment 2
      (b c) ;; comment 3
      (;; comment 4
       d ;; comment 5
       #| multi line
        block
        comment
        |#
    ()))
      |}];
  ignore ()
;;

let round_trip_pretty style tc = of_string (pretty_string style tc)

let get_comment_strings t =
  let open Sexp.With_layout in
  let rec of_t = function
    | Atom _ -> []
    | List (_, t_or_comments, _) -> List.concat_map t_or_comments ~f:of_t_or_comment
  and of_t_or_comment = function
    | Sexp _ -> []
    | Comment comment -> of_comment comment
  and of_comment = function
    | Plain_comment (_, s) -> [ s ]
    | Sexp_comment (_, comments, t) ->
      List.concat [ List.concat_map comments ~f:of_comment; of_t t ]
  in
  List.concat_map t ~f:of_t_or_comment
;;

let whitespace_chars = String.to_list " \t\n\r"

let compress_whitespace s =
  String.split_on_chars s ~on:whitespace_chars
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

(* Doesn't strip sexp comments. Our comparison is on the [Plain_comment _] values,
   including those nested inside sexp comments. *)
let strip_comment_markers s =
  match String.chop_prefix s ~prefix:";" with
  | Some line_comment -> line_comment
  | None ->
    (match
       String.chop_prefix s ~prefix:"#|"
       |> Option.bind ~f:(String.chop_suffix ~suffix:"|#")
     with
     | Some s -> s
     | None -> raise_s [%message "Comment neither line comment nor block comment." s])
;;

let postprocess_comment (style : Comment_print_style.t) prev =
  match style with
  (* [Pretty_print] preserves non-whitespace characters *)
  | Pretty_print -> prev |> strip_comment_markers |> String.strip |> compress_whitespace
  (* [Conservative_print] preserves everything *)
  | Conservative_print -> prev
;;

module Test_helper = struct
  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  type 'a t = (module S with type t = 'a)

  let filter_map (type a) ((module M) : a t) ~f : a t =
    (module struct
      type t = M.t [@@deriving sexp_of]

      include
        Quickcheckable.Of_quickcheckable_filtered
          (M)
          (struct
            type t = M.t

            let of_quickcheckable a = f a
            let to_quickcheckable = Fn.id
          end)
    end)
  ;;
end

(* [Sexp_pretty] doesn't promise to round-trip. Let's limit ourselves to inputs
   that round-tripped up to the number of comments. *)
let round_trippable_sexps style =
  let equal_num_comments =
    Comparable.lift [%compare.equal: int] ~f:(fun list ->
      get_comment_strings list |> List.length)
  in
  Test_helper.filter_map
    (module Sexp_string_quickcheck.Sexp_string)
    ~f:(fun s ->
      Option.try_with (fun () ->
        let prev = of_string s in
        let next = round_trip_pretty style prev in
        assert (equal_num_comments prev next);
        s))
;;

let test_one style =
  let equal =
    Comparable.lift [%equal: string list] ~f:(fun list ->
      get_comment_strings list |> List.map ~f:(postprocess_comment style))
  in
  stage (fun s ->
    let prev = of_string s in
    let next = round_trip_pretty style prev in
    if not (equal prev next)
    then raise_s [%message "Comments changed too much." s (pretty_string style next)])
;;

let%expect_test "quickcheck" =
  let test here style =
    require_does_not_raise here (fun () ->
      let module M = (val round_trippable_sexps style) in
      Base_quickcheck.Test.run_exn (module M) ~f:(unstage (test_one style)))
  in
  test [%here] Pretty_print;
  [%expect {| |}];
  test [%here] Conservative_print;
  [%expect {| |}];
  ignore ()
;;
