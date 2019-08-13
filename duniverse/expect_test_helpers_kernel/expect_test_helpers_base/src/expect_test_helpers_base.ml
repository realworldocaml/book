open! Base
open! Stdio

include (
  Expect_test_helpers_base_intf :
    module type of struct
    include Expect_test_helpers_base_intf
  end
  with module CR := Expect_test_helpers_base_intf.CR
  with module Sexp_style := Expect_test_helpers_base_intf.Sexp_style)

let print_string string =
  print_string string;
  Out_channel.(flush stdout)
;;

module CR = struct
  include Expect_test_helpers_base_intf.CR

  let message t here =
    let cr cr =
      String.concat
        [ "(* "
        ; cr
        ; " require-failed: "
        ; here |> Source_code_position.to_string
        ; ".\n"
        ; "   Do not 'X' this CR; instead make the required property true,\n"
        ; "   which will make the CR disappear.  For more information, see\n"
        ; "   [Expect_test_helpers_base.require]. *)"
        ]
    in
    match t with
    | CR -> cr "CR"
    | CR_soon -> cr "CR-soon"
    | CR_someday -> cr "CR-someday"
    | Comment ->
      String.concat
        [ "(* require-failed: "; here |> Source_code_position.to_string; ". *)" ]
  ;;

  let hide_unstable_output = function
    | CR -> false
    | CR_soon | CR_someday | Comment -> true
  ;;
end

module Sexp_style = struct
  include Expect_test_helpers_base_intf.Sexp_style

  let default_pretty = Pretty (Sexp_pretty.Config.create ~color:false ())

  let simple_pretty =
    Pretty
      { indent = 1
      ; data_alignment = Data_not_aligned
      ; color_scheme = [||]
      ; atom_coloring = Color_none
      ; atom_printing = Escaped
      ; paren_coloring = false
      ; opening_parens = Same_line
      ; closing_parens = Same_line
      ; comments = Drop
      ; singleton_limit = Singleton_limit (Atom_threshold 0, Character_threshold 0)
      ; leading_threshold = Atom_threshold 0, Character_threshold 0
      ; separator = No_separator
      ; sticky_comments = false
      }
  ;;
end

let hide_positions_in_string =
  let module Re = Re.Pcre in
  let expanders =
    lazy
      ([ (* This first pattern has an alphabetic prefix because we want to match exceptions
            and their file positions without also matching timestamps.  However, [Re.Pcre]
            doesn't implement back-references, precluding a simple substitution.  Instead,
            we provide a length of matched data to copy into the output, effectively acting
            like a back-reference in this special case. *)
        "[a-zA-z]:[0-9]+:[0-9]+", 1, ":LINE:COL"
      ; "line [0-9]+:", 0, "line LINE:"
      ; "line [0-9]+, characters [0-9]+-[0-9]+", 0, "line LINE, characters C1-C2"
      ]
        |> List.map ~f:(fun (pattern, prefix_len, expansion) ->
          let rex = Re.regexp pattern in
          fun string ->
            Re.substitute ~rex string ~subst:(fun orig ->
              String.concat [ String.prefix orig prefix_len; expansion ])))
  in
  fun string ->
    List.fold (force expanders) ~init:string ~f:(fun ac expander -> expander ac)
;;

let maybe_hide_positions_in_string ?(hide_positions = false) string =
  if hide_positions then hide_positions_in_string string else string
;;

let sexp_style = ref Sexp_style.default_pretty

let sexp_to_string ?hide_positions sexp =
  let string =
    match !sexp_style with
    | To_string_mach -> Sexp.to_string_mach sexp ^ "\n"
    | To_string_hum -> Sexp.to_string_hum sexp ^ "\n"
    | Pretty config -> Sexp_pretty.pretty_string config sexp
  in
  maybe_hide_positions_in_string ?hide_positions string
;;

let print_s ?hide_positions sexp = print_string (sexp_to_string ?hide_positions sexp)
let on_print_cr = ref print_endline

let print_cr_with_optional_message
      ?(cr = CR.CR)
      ?(hide_positions = CR.hide_unstable_output cr)
      here
      optional_message
  =
  let cr = CR.message cr here |> maybe_hide_positions_in_string ~hide_positions in
  !on_print_cr
    (match optional_message with
     | None -> cr
     | Some sexp ->
       String.concat [ cr; "\n"; String.rstrip (sexp_to_string ~hide_positions sexp) ])
;;

let print_cr ?cr ?hide_positions here message =
  print_cr_with_optional_message ?cr ?hide_positions here (Some message)
;;

let require ?cr ?hide_positions ?if_false_then_print_s here bool =
  match bool with
  | true -> ()
  | false ->
    print_cr_with_optional_message
      ?cr
      ?hide_positions
      here
      (Option.map if_false_then_print_s ~f:force)
;;

let require_equal
      (type a)
      ?cr
      ?hide_positions
      ?if_false_then_print_s
      ?(message = "values are not equal")
      here
      (module M : With_equal with type t = a)
      x
      y
  =
  require
    ?cr
    ?hide_positions
    here
    (M.equal x y)
    ~if_false_then_print_s:
      (lazy
        [%message
          message
            ~_:(x : M.t)
            ~_:(y : M.t)
            ~_:(if_false_then_print_s : Sexp.t Lazy.t sexp_option)])
;;

let require_compare_equal
      (type a)
      ?cr
      ?hide_positions
      ?message
      here
      (module M : With_compare with type t = a)
      x
      y
  =
  require_equal
    ?cr
    ?hide_positions
    ?message
    here
    (module struct
      include M

      let equal = [%compare.equal: t]
    end)
    x
    y
;;

let require_sets_are_equal
      (type a)
      ?cr
      ?hide_positions
      ?(names = "first", "second")
      here
      (module M : Set with type t = a)
      first
      second
  =
  require
    ?cr
    ?hide_positions
    here
    (M.equal first second)
    ~if_false_then_print_s:
      (lazy
        (let show_diff (name1, set1) (name2, set2) =
           let diff = M.diff set1 set2 in
           if M.is_empty diff
           then [%message]
           else
             [%message
               (Printf.sprintf "in %s but not in %s" name1 name2) ~_:(diff : M.t)]
         in
         let first = fst names, first in
         let second = snd names, second in
         [%message.omit_nil
           "sets are not equal"
             ~_:(show_diff first second : Sexp.t)
             ~_:(show_diff second first : Sexp.t)]))
;;

type try_with_result =
  | Did_not_raise
  | Raised of Sexp.t

let try_with ?raise_message ?(show_backtrace = false) (type a) (f : unit -> a) =
  Backtrace.Exn.with_recording show_backtrace ~f:(fun () ->
    match ignore (f () : a) with
    | () -> Did_not_raise
    | exception exn ->
      let backtrace =
        if not show_backtrace then None else Some (Backtrace.Exn.most_recent ())
      in
      Ref.set_temporarily Backtrace.elide (not show_backtrace) ~f:(fun () ->
        Raised
          [%message
            ""
              ~_:(raise_message : string sexp_option)
              ~_:(exn : exn)
              (backtrace : Backtrace.t sexp_option)]))
;;

let require_does_not_raise ?cr ?hide_positions ?show_backtrace here f =
  match try_with f ?show_backtrace ~raise_message:"unexpectedly raised" with
  | Did_not_raise -> ()
  | Raised message -> print_cr ?cr ?hide_positions here message
;;

let require_does_raise ?cr ?hide_positions ?show_backtrace here f =
  match try_with f ?show_backtrace with
  | Raised message -> print_s ?hide_positions message
  | Did_not_raise -> print_cr ?cr ?hide_positions here [%message "did not raise"]
;;

let show_raise (type a) ?hide_positions ?show_backtrace (f : unit -> a) =
  print_s
    ?hide_positions
    (match try_with f ?show_backtrace ~raise_message:"raised" with
     | Did_not_raise -> [%message "did not raise"]
     | Raised message -> message)
;;

let quickcheck
      (type a)
      here
      ?cr
      ?hide_positions
      ?(seed = Base_quickcheck.Test.default_config.seed)
      ?(sizes = Base_quickcheck.Test.default_config.sizes)
      ?(trials = Base_quickcheck.Test.default_config.test_count)
      ?(shrinker = Base_quickcheck.Shrinker.atomic)
      ?(shrink_attempts = Base_quickcheck.Test.default_config.shrink_count)
      ?examples
      ~sexp_of
      ~f
      quickcheck_generator
  =
  match
    Base_quickcheck.Test.run
      ~config:{ seed; test_count = trials; shrink_count = shrink_attempts; sizes }
      ?examples
      (module struct
        type t = a

        let sexp_of_t = sexp_of
        let quickcheck_generator = quickcheck_generator
        let quickcheck_shrinker = shrinker
      end)
      ~f:(fun elt ->
        let cr_count = ref 0 in
        let original_on_print_cr = !on_print_cr in
        Ref.set_temporarily
          on_print_cr
          (fun string ->
             Int.incr cr_count;
             original_on_print_cr string)
          ~f:(fun () -> f elt);
        if !cr_count > 0
        then Or_error.errorf "printed %d CRs for Quickcheck-generated input" !cr_count
        else Ok ())
  with
  | Ok () -> ()
  | Error error -> print_cr here ?cr ?hide_positions [%sexp (error : Error.t)]
;;
