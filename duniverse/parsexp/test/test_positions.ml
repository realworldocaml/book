open Import

module P = Parsexp.Positions

let cases =
  (* Cases of inputs, [.] represent a saved position, [:] represent a position saved
     twice. *)
  [ ""
  ; "."
  ; "1."
  ; "12."
  ; "123."
  ; "1234."
  ; "12345."
  ; "123456."
  ; ".\n"
  ; "\n."
  ; "\n\n\n"
  ; "\
. . . xxxxx
xxxx . xxx . xxx
"
  ; "....."
  ; ":"
  ; "..:.."
  ; Printf.sprintf "%*s." 40 ""
  ; Printf.sprintf "%*s." 300 ""
  ; String.concat (List.init (62*4) ~f:(fun _i -> ".."))
  ]

(* Extract the positions of the '.' in the input *)
let build_positions s =
  let builder = P.Builder.create () in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\n' -> P.Builder.add_newline builder ~offset:i
    | '.'  -> P.Builder.add         builder ~offset:i
    | ':'  -> P.Builder.add_twice   builder ~offset:i
    | _    -> ()
  done;
  P.Builder.contents builder

(* Same but with a trivial implementation (i.e. without using [Parsexp.Positions]). *)
let build_positions_simple s =
  let rec loop i (pos : P.pos) =
    if i = String.length s then
      []
    else
      let offset = pos.offset + 1 in
      match s.[i] with
      | '\n' -> loop (i + 1) { line = pos.line + 1
                             ; col  = 0
                             ; offset
                             }
      | '.' -> pos :: loop (i + 1) { pos with offset; col = pos.col + 1 }
      | ':' -> pos :: pos :: loop (i + 1) { pos with offset; col = pos.col + 1 }
      | _   ->        loop (i + 1) { pos with offset; col = pos.col + 1 }
  in
  loop 0 P.beginning_of_file

let%expect_test "build_positions_simple" =
  let f s = print_s [%sexp (build_positions_simple s : P.pos list)] in
  f "";
  [%expect{| () |}];
  f ".";
  [%expect{|
    ((
      (line   1)
      (col    0)
      (offset 0)))
  |}];
  f ". .";
  [%expect{|
    (((line 1) (col 0) (offset 0))
     ((line 1) (col 2) (offset 2)))
  |}];
  f ". xxx \n xxx .";
  [%expect{|
    (((line 1) (col 0) (offset 0))
     ((line 2) (col 5) (offset 12)))
  |}]
;;

let check_all_subsexps_map_to_their_position s sexps positions =
  let check_subsexp subsexp =
    match P.find_sub_sexp_in_list_phys positions sexps ~sub:subsexp with
    | None -> failwith "not found"
    | Some range ->
      assert (Sexp.(=) (Parsexp.Single.parse_string_exn (
        String.sub s
          ~pos:range.start_pos.offset
          ~len:(range.end_pos.offset - range.start_pos.offset))) subsexp)
  in
  let rec iter sexps =
    List.iter sexps ~f:(fun sexp ->
      check_subsexp sexp;
      match sexp with
      | Atom _ -> ()
      | List l -> iter l)
  in
  iter sexps
;;

let%expect_test "build_positions_ignore_commented_expr" =
  let f s =
    let sexps, positions = Parsexp.Many_and_positions.parse_string_exn s in
    print_s [%sexp (P.to_list positions : P.pos list)];
    check_all_subsexps_map_to_their_position s sexps positions;
  in
  f "a #;((b)) c";
  [%expect{|
    (((line 1) (col 0)  (offset 0))
     ((line 1) (col 0)  (offset 0))
     ((line 1) (col 10) (offset 10))
     ((line 1) (col 10) (offset 10)))
  |}]
;;

let%expect_test "all" =
  List.iter cases ~f:(fun input ->
    let expected = build_positions_simple input in
    let got      = build_positions        input |> P.to_list in
    require [%here] ([%compare.equal: P.pos list] got expected)
      ~if_false_then_print_s:
        (lazy
          [%sexp { input    : string
                 ; expected : P.pos list
                 ; got      : P.pos list
                 }]))

let%expect_test "find" =
  List.iter cases ~f:(fun input ->
    let positions = build_positions_simple input |> Array.of_list in
    let from_parsexp = build_positions input in
    let count = Array.length positions in
    for i = 0 to count - 1 do
      for j = i + 1 to count - 1 do
        let expected =
          P.make_range_incl
            ~start_pos:positions.(i)
            ~last_pos:positions.(j)
        in
        let got = Result.try_with (fun () -> P.find from_parsexp i j) in
        require [%here]
          (match got with
           | Ok got  -> [%compare.equal: P.range] got expected
           | Error _ -> false)
          ~if_false_then_print_s:
            (lazy
              [%sexp { input    : string
                     ; i        : int
                     ; j        : int
                     ; expected : (P.range, exn) Result.t = Ok expected
                     ; got      : (P.range, exn) Result.t
                     }])
      done
    done);
  [%expect]

let cases_for_find_sub_sexp =
  [ "( ( ( abc ) (+ 1 2) ) )"
  ]

module Annotated = struct
  type t =
    | Atom of P.range * Sexp.t
    | List of P.range * Sexp.t * t list

  let of_sexp_and_positions =
    let rec loop (sexp : Sexp.t) (positions : P.pos list) =
      match sexp, positions with
      | Atom _, start_pos :: last_pos :: rest ->
        (Atom (P.make_range_incl ~start_pos ~last_pos, sexp), rest)
      | List l, start_pos :: rest -> begin
          let annots_rev, rest =
            List.fold_left l ~init:([], rest) ~f:(fun (acc, positions) sexp ->
              let annot, rest = loop sexp positions in
              (annot :: acc, rest))
          in
          match rest with
          | [] -> assert false
          | last_pos :: rest ->
            (List (P.make_range_incl ~start_pos ~last_pos, sexp, List.rev annots_rev),
             rest)
        end
      | _ -> assert false
    in
    fun sexp positions ->
      let t, rest = loop sexp positions in
      assert (List.is_empty rest);
      t

  let rec iter t ~f =
    match t with
    | Atom (range, sexp) -> f range sexp
    | List (range, sexp, children) ->
      f range sexp;
      List.iter children ~f:(iter ~f)
end

let%expect_test "find_sub_sexp_phys" =
  List.iter cases_for_find_sub_sexp ~f:(fun input ->
    let sexp, positions = Parsexp.Single_and_positions.parse_string_exn input in
    let annot = Annotated.of_sexp_and_positions sexp (P.to_list positions) in
    Annotated.iter annot ~f:(fun expected sub ->
      let got = P.find_sub_sexp_phys positions sexp ~sub in
      require [%here]
        (Option.value_map got ~default:false ~f:([%compare.equal: P.range] expected))
        ~if_false_then_print_s:
          (lazy
            [%sexp
              { input    : string
              ; sexp     : Sexp.t
              ; sub      : Sexp.t
              ; expected : P.range option = Some expected
              ; got      : P.range option
              }])));
  [%expect {| |}]
