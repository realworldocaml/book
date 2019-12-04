open Base

let for_all_string s ~f =
  let b = ref true in
  for i = 0 to String.length s - 1 do
    b := !b && f s.[i]
  done;
  !b

let is_blank = function
  | ' ' | '\t' -> true
  | _ -> false

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

let is_blanks s = for_all_string s ~f:is_blank
let is_spaces s = for_all_string s ~f:is_space
let no_nl s = for_all_string s ~f:(fun c -> Char.(<>) c '\n')
let has_nl s = not (no_nl s)

module Line = struct
  type 'a not_blank =
    { trailing_blanks : string
    ; orig            : string
    ; data            : 'a
    }
  [@@deriving sexp_of, compare]

  type 'a t =
    | Blank     of string
    | Not_blank of 'a not_blank
  [@@deriving sexp_of, compare]

  let map t ~f =
    match t with
    | Blank     b -> Blank b
    | Not_blank n -> Not_blank { n with data = f n.orig n.data }

  let strip = function
    | Blank     _ -> Blank ""
    | Not_blank n -> Not_blank { n with trailing_blanks = "" }

  let invariant inv = function
    | Blank s -> assert (is_blanks s)
    | Not_blank n ->
      assert (is_blanks n.trailing_blanks);
      inv n.data;
      assert (no_nl n.orig);
      let len = String.length n.orig in
      assert (len > 0 && not (is_blank n.orig.[len - 1]));
  ;;

  let data t ~blank =
    match t with
    | Blank     _ -> blank
    | Not_blank n -> n.data
  ;;

  let orig = function
    | Blank     _ -> ""
    | Not_blank n -> n.orig
  ;;
end

type 'a single_line =
  { leading_blanks  : string
  ; trailing_spaces : string
  ; orig            : string
  ; data            : 'a
  }
[@@deriving sexp_of, compare]

type 'a multi_lines =
  { leading_spaces  : string
  ; trailing_spaces : string
  ; indentation     : string
  ; lines           : 'a Line.t list
  }
[@@deriving sexp_of, compare]

type 'a t =
  | Empty       of string
  | Single_line of 'a single_line
  | Multi_lines of 'a multi_lines
[@@deriving sexp_of, compare]

let invariant inv t =
  match t with
  | Empty s -> assert (is_spaces s)
  | Single_line s ->
    assert (is_blanks s.leading_blanks);
    assert (is_spaces s.trailing_spaces);
    inv s.data;
    assert (no_nl s.orig);
    let len = String.length s.orig in
    assert (len > 0
            && not (is_blank s.orig.[0])
            && not (is_blank s.orig.[len - 1]))
  | Multi_lines m ->
    assert (is_spaces m.leading_spaces);
    let ld_len = String.length m.leading_spaces in
    assert (ld_len = 0 || Char.equal m.leading_spaces.[ld_len - 1] '\n');
    let tr_has_nl = has_nl m.trailing_spaces in
    assert (is_spaces m.trailing_spaces &&
            (not tr_has_nl || Char.equal m.trailing_spaces.[0] '\n'));
    assert (is_blanks m.indentation);
    List.iter m.lines ~f:(Line.invariant inv);
    match m.lines with
    | [] -> assert false
    | Blank _ :: _ -> assert false
    | [Not_blank n] -> assert (ld_len > 0 && (tr_has_nl || String.is_empty n.trailing_blanks))
    | l ->
      let rec check_last = function
        | []                 -> assert false
        | [Line.Blank     _] -> assert false
        | [Line.Not_blank n] -> assert (tr_has_nl || String.is_empty n.trailing_blanks)
        | _ :: l             -> check_last l
      in
      check_last l
;;

let empty = Empty ""

let map t ~f =
  match t with
  | Empty e ->
    Empty e
  | Single_line s ->
    Single_line { s with data = f s.orig s.data }
  | Multi_lines m ->
    Multi_lines { m with lines = List.map m.lines ~f:(Line.map ~f) }

let data t ~blank =
  match t with
  | Empty       _ -> []
  | Single_line s -> [s.data]
  | Multi_lines m -> List.map m.lines ~f:(Line.data ~blank)

let stripped_original_lines t =
  match t with
  | Empty       _ -> []
  | Single_line s -> [s.orig]
  | Multi_lines m -> List.map m.lines ~f:Line.orig

let line_of_single s : _ Line.t =
  Not_blank
    { trailing_blanks = ""
    ; orig            = s.orig
    ; data            = s.data
    }

let to_lines t =
  match t with
  | Empty       _ -> []
  | Single_line s -> [line_of_single s]
  | Multi_lines m -> m.lines

let strip t =
  match t with
  | Empty _ -> Empty ""
  | Single_line s ->
    Single_line { s with leading_blanks  = ""; trailing_spaces = "" }
  | Multi_lines m ->
    match m.lines with
    | [] -> Empty ""
    | [Blank _] -> assert false
    | [Not_blank n] ->
      Single_line
        { leading_blanks  = ""
        ; trailing_spaces = ""
        ; orig            = n.orig
        ; data            = n.data
        }
    | lines ->
      Multi_lines
        { leading_spaces  = ""
        ; trailing_spaces = ""
        ; indentation     = ""
        ; lines           = List.map lines ~f:Line.strip
        }
;;

let to_string t =
  match t with
  | Empty s -> s
  | Single_line s ->
    s.leading_blanks ^ s.orig ^ s.trailing_spaces
  | Multi_lines m ->
    let indent (line :  _ Line.t) =
      match line with
      | Blank     b -> b
      | Not_blank n ->
        m.indentation ^ n.orig ^ n.trailing_blanks
    in
    let s = List.map m.lines ~f:indent |> String.concat ~sep:"\n" in
    m.leading_spaces ^ s ^ m.trailing_spaces
;;

let trim_lines lines =
  let rec loop0 : _ Line.t list -> _ = function
    | Blank _ :: l -> loop0 l
    | l -> loop1 l ~acc:[] ~acc_with_trailing_blanks:[]
  and loop1 ~acc ~acc_with_trailing_blanks = function
    | Blank     _ as x :: l ->
      loop1 l ~acc ~acc_with_trailing_blanks:(x :: acc_with_trailing_blanks)
    | Not_blank _ as x :: l ->
      let acc = x :: acc_with_trailing_blanks in
      loop1 l ~acc ~acc_with_trailing_blanks:acc
    | [] -> List.rev acc
  in
  loop0 lines

let not_blank_lines lines =
  List.fold_left lines ~init:[] ~f:(fun acc (l : _ Line.t) ->
    match l with
    | Blank     _ -> acc
    | Not_blank n -> n.orig :: acc)
  |> List.rev

let longest_common_prefix a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let len = min len_a len_b in
  let i = ref 0 in
  while !i < len && Char.equal a.[!i] b.[!i] do Int.incr i done;
  String.sub a ~pos:0 ~len:!i
;;

let indentation s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_blank s.[!i] do Int.incr i done;
  String.sub s ~pos:0 ~len:!i
;;

let extract_indentation lines =
  match not_blank_lines lines with
  | [] -> ("", lines)
  | first :: rest ->
    let indent =
      List.fold_left rest ~init:(indentation first) ~f:longest_common_prefix
    in
    let indent_len = String.length indent in
    let update_line : 'a Line.t -> 'a Line.t = function
      | Blank     b -> Blank b
      | Not_blank n ->
        let orig =
          String.sub n.orig ~pos:indent_len ~len:(String.length n.orig - indent_len)
        in
        Not_blank { n with orig }
    in
    (indent, List.map lines ~f:update_line)
;;

let break s at = (String.prefix s at, String.drop_prefix s at)

let reconcile (type a) t ~lines ~default_indentation ~pad_single_line =
  let module M = struct
    type t =
      | Empty
      | Single_line of a Line.not_blank
      | Multi_lines of a Line.t list
  end in
  let lines =
    match trim_lines lines |> extract_indentation |> snd with
    | []            -> M.Empty
    | [Blank     _] -> assert false
    | [Not_blank n] -> M.Single_line n
    | lines         -> M.Multi_lines lines
  in
  let padding = if pad_single_line then " " else "" in
  let res =
    match t, lines with
    | Empty       _ , Empty         -> t
    | Single_line s , Single_line n ->
      Single_line { s with orig = n.orig; data = n.data }
    | Multi_lines m , Multi_lines l ->
      Multi_lines { m with lines = l }
    | Empty       e , Multi_lines l ->
      let ld, tr =
        if has_nl e then
          let ld, tr = break e (String.index_exn e '\n') in
          (ld ^ "\n", tr)
        else
          ("\n", padding)
      in
      Multi_lines
        { leading_spaces  = ld
        ; trailing_spaces = tr
        ; indentation     = String.make (default_indentation + 2) ' '
        ; lines           = l
        }
    | Single_line m , Multi_lines l ->
      Multi_lines
        { leading_spaces  = "\n"
        ; trailing_spaces = m.trailing_spaces
        ; indentation     = String.make (default_indentation + 2) ' '
        ; lines           = l
        }
    | Single_line _ , Empty
    | Multi_lines _ , Empty         ->
      Empty padding
    | Empty       _ , Single_line n ->
      Single_line
        { orig            = n.orig
        ; data            = n.data
        ; leading_blanks  = padding
        ; trailing_spaces = padding
        }
    | Multi_lines m , Single_line n ->
      Multi_lines { m with lines = [Not_blank n] }
  in
  invariant ignore res;
  res
;;
