open Core_kernel
open Int.Replace_polymorphic_compare

include Parser_intf

module Body = struct
  module T = struct

    (* Requirements on [regex_string]:
       - it must have a valid Re2 syntax;
       - it must not change meaning if concatenated with another allowed regex_string:
         in particular, 'a|b' is not allowed (use e.g. '(?:a|b)' instead).
    *)
    type 'a t =
      { regex_string : Rope.t
      ; num_submatches : int
      ; to_result : int -> string option array -> 'a
      }

    let return x =
      { regex_string = Rope.of_string ""
      ; num_submatches = 0
      ; to_result = fun _ _ -> x
      }
    let map = `Custom (fun t ~f ->
      { t with to_result = fun shift matches -> f (t.to_result shift matches) })
    let apply tf tx =
      let to_result shift matches =
        let f = tf.to_result shift matches in
        let x = tx.to_result (shift + tf.num_submatches) matches in
        f x
      in
      { regex_string = Rope.(tf.regex_string ^ tx.regex_string)
      ; num_submatches = tf.num_submatches + tx.num_submatches
      ; to_result
      }
  end
  include T
  include Applicative.Make (T)

  let to_regex_string t = Rope.to_string t.regex_string

  let sexp_of_t _sexp_of_a t = Sexp.Atom (to_regex_string t)

  let to_re2 ?(case_sensitive=true) t =
    let options =
      { Options.default with
        case_sensitive
      ; encoding = Latin1
      ; dot_nl = true
      }
    in
    let s = to_regex_string t in
    (* We created [t.regex_string] ourselves, so the syntax ought to be good. *)
    match Regex.create ~options s with
    | Ok rex -> rex
    | Error e ->
      failwiths "Re2.Parser.to_re2 BUG: failed to compile"
        (s, e) [%sexp_of: string * Error.t]

  let compile ?case_sensitive t =
    let rex = to_re2 ?case_sensitive t in
    let to_result = t.to_result in
    Staged.stage (fun s ->
      match Regex.get_matches_exn ~max:1 rex s with
      | [] -> None
      | m :: _ ->
        let m = Regex.without_trailing_none m in
        Some (to_result 1 (Regex.Match.get_all m)))

  let run ?case_sensitive t = Staged.unstage (compile ?case_sensitive t)

  let ignore t =
    { regex_string = t.regex_string
    ; num_submatches = t.num_submatches
    ; to_result = fun _ _ -> ()
    }

  let matches ?case_sensitive t =
    let r = to_re2 ?case_sensitive (ignore t) in
    fun input -> Regex.matches r input

  module For_test = struct
    let should_match_with_case ~case_sensitive sexp_of_r rex inp result =
      [%test_pred: r t * string] ~message:"should_match"
        (fun (rex, inp) -> matches ~case_sensitive rex inp)
        (rex, inp);
      [%test_result: Sexp.t option]
        ~expect:(Some (sexp_of_r result))
        (Option.map ~f:sexp_of_r (run ~case_sensitive rex inp))

    let should_not_match_with_case ~case_sensitive rex inp =
      [%test_pred: _ t * string] ~message:"should_not_match"
        (fun (rex, inp) -> not (matches ~case_sensitive rex inp)) (rex, inp);
      [%test_pred: string t * string]
        (fun (rex, inp) -> Option.is_none (run ~case_sensitive rex inp)) (rex, inp)

    let match_only_if_case_insensitive sexp_of_r rex inp result =
      should_match_with_case ~case_sensitive:false sexp_of_r rex inp result;
      should_not_match_with_case ~case_sensitive:true rex inp

    let should_match sexp_of_r rex inp result =
      should_match_with_case ~case_sensitive:true sexp_of_r rex inp result;
      should_match_with_case ~case_sensitive:false sexp_of_r rex inp result

    let should_match_string = should_match String.sexp_of_t
    let should_match_unit rex inp = should_match Unit.sexp_of_t rex inp ()

    let should_not_match rex inp =
      should_not_match_with_case ~case_sensitive:true rex inp;
      should_not_match_with_case ~case_sensitive:false rex inp
  end
  open For_test

  let fail =
    { regex_string = Rope.of_string "$x"
    ; num_submatches = 0
    ; to_result = fun _ _ ->
        failwith "Re2.Parser.fail BUG: something matched regex '$x'"
    }

  let%test_unit _ = should_not_match fail ""
  let%test_unit _ = should_not_match fail "$x"
  let%test_unit _ = should_not_match fail "foo\nxyz"

  let of_captureless_string regex_string =
    { regex_string = Rope.of_string regex_string
    ; num_submatches = 0
    ; to_result = fun _ _ -> ()
    }

  let string s =
    of_captureless_string (Regex.escape s)

  let%test_unit _ = should_match_unit (string "blah") "bloblahba"
  let%test_unit _ = should_not_match (string ".") "x"
  let%test_unit _ =
    let nasty_string = "^(he+l*l.o)[abc]{{\\\\$" in
    should_match_unit (string nasty_string) ("before" ^ nasty_string ^ "after")
  let%test_unit _ = should_match_unit (string "blah\000blee") "blah\000blee"
  let%test_unit _ = should_not_match (string "blah\000nope") "blah\000blee"

  let and_capture t =
    { regex_string = Rope.(of_string "(" ^ t.regex_string ^ of_string ")")
    ; num_submatches = t.num_submatches + 1
    ; to_result = fun shift matches ->
        ( t.to_result (shift + 1) matches
        , Option.value_exn ~message:"Re2.Parser.capture bug: failed to capture"
            matches.(shift)
        )
    }

  let capture t = map (and_capture t) ~f:(fun ((), s) -> s)

  let%test_unit _ = match_only_if_case_insensitive [%sexp_of: string]
                      (capture (string "bLaH")) "baBLAHba" "BLAH"

  let%test_unit _ =
    should_match [%sexp_of: string * string]
      (both
         (capture (string "a") <* ignore (capture (string "b")))
         (capture (string "c")))
      "abc"
      ("a", "c")

  let or_ = function
    | [] -> fail
    | [x] -> x
    | ts ->
      (* We do a dummy capture for each branch so that we can tell which actually matched,
         for the purpose of calling the correct [to_result] callback (if any).
         This extra match is the cause of all the +1s below. *)
      { regex_string =
          Rope.(of_string "(?:"
                ^ List.reduce_exn ~f:(fun x y -> x ^ of_string "|" ^ y)
                    (List.map ts ~f:(fun t -> Rope.(of_string "()" ^  t.regex_string)))
                ^ of_string ")")
      ; num_submatches = List.sum (module Int) ts ~f:(fun t -> t.num_submatches + 1)
      ; to_result =
          let rec go i matches = function
            | [] -> failwith "Re2.Parser.or_.to_result bug: called on non-match"
            | t :: ts ->
              if Option.is_some matches.(i)
              then t.to_result (i + 1) matches
              else go (i + 1 + t.num_submatches) matches ts
          in
          fun shift matches ->
            go shift matches ts
      }
  let%test_unit _ =
    should_match_string (or_ [capture (string "a"); capture (string "b")]) "a" "a"
  let%test_unit _ =
    should_match_string (capture (or_ [string "a"; string "b"])) "a" "a"
  let%test_unit _ =
    should_not_match (or_ [string "a"; string "b"]) "c"
  let%test_unit _ =
    should_match_string (or_ [string "b"; string "c"] *> capture (string "a")) "ca" "a"
  let%test_unit _ =
    should_match_string (capture (string "a") <* or_ [string "b"; string "c"]) "ac" "a"
  let%test_unit _ =
    should_match_string
      (ignore (or_ [string "a"; string "b"]) *> (capture (string "c"))) "ac" "c"

  (* This is not subsumed by the [with_quantity] stuff because we can capture here, but we
     can't there. *)
  let optional ?(greedy=true) t =
    let q = Rope.of_string (if greedy then "?" else "??") in
    { regex_string = Rope.(of_string "(" ^ t.regex_string ^ of_string ")" ^ q)
    ; num_submatches = t.num_submatches + 1
    ; to_result = fun shift matches ->
        Option.map matches.(shift)
          ~f:(fun _ -> t.to_result (shift + 1) matches)
    }

  let%test_unit _ =
    should_match [%sexp_of: unit option]
      (optional (string "x"))
      "x" (Some ())
  let%test_unit _ =
    should_match [%sexp_of: unit option]
      (optional ~greedy:false (string "x"))
      "x" None

  let with_quantity t ~greedy ~quantity =
    let q = Rope.of_string (if greedy then "" else "?") in
    { regex_string =
        Rope.(of_string "(?:" ^ t.regex_string ^ of_string ")" ^ of_string quantity ^ q)
    ; num_submatches = t.num_submatches
    ; to_result = fun _ _ -> ()
    }

  let repeat ?(greedy=true) ?(min=0) ?(max=None) t =
    let validate_if_some validate x =
      Option.validate ~none:Validate.pass_unit ~some:validate x
    in
    Validate.of_list
      [ Validate.name "min" (Int.validate_non_negative min)
      ; Validate.name "max" (validate_if_some (Int.validate_lbound ~min:(Incl min)) max)
      ; Validate.name "re2 implementation restrictions"
          (Validate.of_list
             [ Int.validate_ubound ~max:(Incl 1000) min
             ; validate_if_some (Int.validate_ubound ~max:(Incl 1000)) max
             ])
      ]
    |> Validate.maybe_raise;
    match min, max with
    (* special cases *)
    | 0, None -> with_quantity t ~greedy ~quantity:"*"
    | 1, None -> with_quantity t ~greedy ~quantity:"+"
    | 0, Some 1 -> with_quantity t ~greedy ~quantity:"?"
    (* silly cases *)
    | 0, Some 0 -> of_captureless_string ""
    | 1, Some 1 -> ignore t
    (* actual cases *)
    | min, None -> with_quantity t ~greedy ~quantity:(sprintf "{%d,}" min)
    | min, Some max ->
      (* actually, [~greedy:false] is fine here as well -- r{n}? is permitted *)
      if min = max then with_quantity t ~greedy:true ~quantity:(sprintf "{%d}" min)
      else with_quantity t ~greedy ~quantity:(sprintf "{%d,%d}" min max)

  let%test_module "repeat" =
    (module struct
      let%test_unit _ =
        List.iter
          ~f:([%test_pred: int option * int option * string * string option]
                (fun (min, max, inp, result) ->
                   let a's = capture (repeat ?min ~max (string "a")) in
                   0 = [%compare: string option]
                         result (run (string "c" *> a's <* string "b") inp);
                ))
          [ None,   None,   "caaab", Some "aaa"
          ; None,   None,   "cb",    Some ""
          ; Some 0, None,   "cb",    Some ""
          ; Some 1, None,   "cb",    None
          ; Some 1, None,   "cab",   Some "a"
          ; Some 1, Some 2, "caaab", None
          ; Some 2, Some 2, "caaab", None
          ; Some 3, Some 3, "caaab", Some "aaa"
          ; Some 4, Some 4, "caaab", None
          ; Some 2, None,   "caaab", Some "aaa"
          ; Some 0, Some 0, "cb",    Some ""
          ; Some 1, Some 1, "cab",   Some "a"
          ; None,   Some 0, "cb",    Some ""
          ]

      let%test _ = Exn.does_raise (fun () -> repeat ~min:3 ~max:(Some 2) fail)
      let%test _ = Exn.does_raise (fun () -> repeat ~min:(-1) fail)
      let%test _ = Exn.does_raise (fun () -> repeat ~max:(Some (-1)) fail)
      let%test _ = Exn.does_raise (fun () -> repeat ~min:1001 fail)

      let%test_unit _ =
        should_match_string
          (capture (repeat (or_ [string "a"; string "b"]) *> string "a"))
          "baba" "baba"
      let%test_unit _ =
        should_match_string
          (capture (repeat ~greedy:false (or_ [string "a"; string "b"]) *> string "a"))
          "baba" "ba"
    end)

  let times t n = repeat ~min:n ~max:(Some n) t

  let%test_unit _ =
    should_match_string (capture (times (or_ [string "a"; string "b"]) 3)) "cabbage" "abb"
  let%test_unit _ =
    should_match_unit (times (string "hello") 0) ""

  let%test_unit _ =
    should_match_string
      (repeat (map ~f:(fun _ -> ()) (capture (string "x")))
       *> capture (string "y"))
      "xxy"
      "y"

  let start_of_input = of_captureless_string "^"
  let%test_unit _ = should_match_unit (start_of_input *> string "blah") "blahblee"
  let%test_unit _ = should_not_match (start_of_input *> string "blah") "bloblahba"

  let end_of_input = of_captureless_string "$"
  let%test_unit _ = should_match_unit (string "blee" *> end_of_input) "blahblee"
  let%test_unit _ = should_not_match (string "blah" *> end_of_input) "bloblahba"

  let%test_unit _ =
    should_match
      [%sexp_of: (unit option * unit option) * string]
      (and_capture (
         both
           (optional (string "a") <* start_of_input <* string "b")
           (end_of_input *> optional (string "c"))))
      "b"
      ((None, None), "b")

  let%test_unit _ =
    should_match_string
      (or_ [start_of_input *> capture (string "a"); capture (string "b")]) "ba" "b"

  let of_re2 r =
    let regex_string = Regex.pattern r in
    (* [Regex.num_submatches] includes 1 for the whole match, which we omit *)
    let num_submatches = Regex.num_submatches r - 1 in
    { regex_string = Rope.(of_string "(?:" ^ of_string regex_string ^ of_string ")")
    ; num_submatches
    ; to_result = fun shift matches -> Array.sub matches ~pos:shift ~len:num_submatches
    }
  let%test_module _ =
    (module struct
      let mk s = of_re2 (Regex.create_exn s)

      let%test_unit _ =
        let r = mk "a(b)(?:c([de])|(?P<foo>f)g)" in
        should_match [%sexp_of: string option array] r "abcd"
          [| Some "b"; Some "d"; None |];
        should_match [%sexp_of: string option array list] (all [r; r; r]) "abcdabfgabce"
          [ [| Some "b"; Some "d"; None     |]
          ; [| Some "b"; None    ; Some "f" |]
          ; [| Some "b"; Some "e"; None     |]
          ]

      let%test_unit "messing with options" =
        should_match_unit (ignore (mk "abc(?i)def")) "abcDEF";
        match_only_if_case_insensitive sexp_of_string
          (capture (string "abc" *> ignore (mk "(?i)") *> string "def"))
          "abcDEF"
          "abcDEF";
        should_not_match (mk "(?-i)abcdef") "abcDEF"
    end)

  let%test_unit _ =
    let r = of_re2 (Regex.create_exn "a|b") in
    let rs =
      [ start_of_input *> ignore (r <* string "x") <* end_of_input
      ; start_of_input *> ignore (capture (ignore (r <* string "x"))) <* end_of_input
      ]
    in
    List.iter rs ~f:(fun r ->
      should_match_unit r "ax";
      should_match_unit r "bx";
      should_not_match r "axq";
      should_not_match r "qbx")

  module Char = struct
    let capture_char t = map (capture t) ~f:(fun s -> s.[0])

    let c r = capture_char (of_captureless_string r)

    let upper = c "[[:upper:]]"
    let lower = c "[[:lower:]]"
    let alpha = c "[[:alpha:]]"
    let digit = c "[0-9]"
    let alnum = c "[[:alnum:]]"
    let space = c "[[:space:]]"
    let any   = c "."

    let%test_module _ =
      (module struct
        let all_chars = List.init (Char.to_int Char.max_value + 1) ~f:Char.of_int_exn
        let matches_pred regex pred =
          List.iter all_chars ~f:(fun c ->
            if pred c
            then
              should_match_with_case ~case_sensitive:true sexp_of_char
                regex (String.of_char c) c
            else
              should_not_match_with_case ~case_sensitive:true
                regex (String.of_char c))

        let%test_unit _ = matches_pred upper Char.is_uppercase
        let%test_unit _ = matches_pred lower Char.is_lowercase
        let%test_unit _ = matches_pred alpha Char.is_alpha
        let%test_unit _ = matches_pred digit Char.is_digit
        let%test_unit _ = matches_pred alnum Char.is_alphanum
        let%test_unit _ = matches_pred space Char.is_whitespace
        let%test_unit _ = matches_pred any   (Fn.const true)
      end)

    let char_list_to_regex_string chars = Regex.escape (String.of_char_list chars)

    let one_of = function
      | [] -> fail
      | [x] ->
        capture_char (string (String.of_char x))
      | chars ->
        capture_char (of_captureless_string ("[" ^ char_list_to_regex_string chars ^ "]"))

    let not_one_of = function
      (* this case is necessary because "[^]blah]" means "none of ]blah" *)
      | [] -> any
      | chars ->
        capture_char (of_captureless_string ("[^" ^ char_list_to_regex_string chars ^ "]"))

    let%test_module _ =
      (module struct
        let should_match_char = should_match sexp_of_char

        let%test_unit _ = should_match_char (one_of ['\xe2'; '\x82'; '\xac']) "\x82" '\x82'
        let%test_unit _ = should_match_char (one_of ['x'; 'x']) "x" 'x'
        let%test_unit _ = should_not_match (one_of []) "x"
        let%test_unit _ = should_match_char (or_ [one_of []; one_of ['x']]) "x" 'x'
        let%test_unit _ = should_match_char (one_of ['0'; '-'; '9']) "-" '-'
        let%test_unit _ = should_not_match (one_of ['0'; '-'; '9']) "5"
        let%test_unit _ =
          let difficult_char = one_of ['^'; '['; ']'] in
          let r = all [difficult_char; difficult_char; difficult_char] in
          should_match [%sexp_of: char list] r "^[]" ['^'; '['; ']'];
          should_not_match r "]]x"

        let%test_unit _ = should_not_match (one_of ['^'; ']']) "\\"
        let%test_unit _ = should_match_char (one_of ['\\'; 'n']) "n" 'n'
        let%test_unit _ = should_match_char (one_of ['x'; '\\']) "\\" '\\'
        let%test_unit _ = should_not_match (one_of ['.']) "a"
        let%test_unit _ =
          should_match_string
            (capture (ignore (all [any; one_of ['^']; any]))) "a^c" "a^c"

        let%test_unit _ = should_not_match (not_one_of ['x']) "x"
        let%test_unit _ = should_match_char (not_one_of [']']) "x" 'x'
        let%test_unit _ = should_match_char (one_of ['\000']) "\000" '\000'
        let%test_unit _ = should_match_char (one_of ['a'; '\000'; 'b']) "b" 'b'
        let%test_unit _ =
          let difficult_char = not_one_of ['^'; '['; ']'] in
          let r = both difficult_char difficult_char in
          should_match [%sexp_of: (char * char)] r "ab" ('a', 'b');
          should_not_match r "a^"
      end)
  end

  module Decimal = struct
    let digit = map Char.digit ~f:(fun c -> Int.of_string (String.of_char c))
    let sign =
      map (optional (Char.one_of ['+'; '-']))
        ~f:(function
          | None | Some '+' -> 1
          | Some '-' -> -1
          | Some c -> failwiths "matched unexpected character" c [%sexp_of: char])
    let unsigned = map (capture (repeat ~min:1 Char.digit)) ~f:Int.of_string
    let int = map2 sign unsigned ~f:( * )

    let%test_unit "Parsing an empty string shouldn't raise" =
      run int "" |> Core_kernel.ignore
    let%test_unit _ = should_not_match int ""
    let%test_unit _ = should_match Int.sexp_of_t int "-10" (-10)
    let%test_unit _ = should_match Int.sexp_of_t int "+005" 5
    let%test_unit _ = should_match Int.sexp_of_t int "42" 42
  end

  let any_string = capture (repeat (ignore Char.any))

  let%bench_module "big regex" =
    (module struct
      let big_regex_benchmark n =
        let regex =
          compile (
            Fn.apply_n_times ~n
              (fun x -> map (or_ [x; capture (string "boo")]) ~f:(fun x -> x)) any_string)
        in
        fun () ->
          [%test_result: string option]
            (Staged.unstage regex (String.make n 'x'))
            ~expect:(Some (String.make n 'x'))

      let%bench_fun "compilation" [@indexed (=) n [500; 1000; 2000; 10000]] =
        (fun () ->
           let (_ : unit -> unit) = big_regex_benchmark n in
           ())

      let%bench_fun "matching only" [@indexed (=) n [500; 1000; 2000]] =
        big_regex_benchmark n

    end)

end
include Body

module Open_on_rhs_intf = struct module type S = S with type 'a t = 'a t end
include Applicative.Make_let_syntax (Body) (Open_on_rhs_intf) (Body)
