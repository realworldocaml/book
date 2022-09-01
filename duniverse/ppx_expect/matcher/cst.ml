open! Base
open Import

let for_all_string s ~f =
  let b = ref true in
  for i = 0 to String.length s - 1 do
    b := !b && f s.[i]
  done;
  !b
;;

let is_blank = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let is_blanks s = for_all_string s ~f:is_blank

let is_conflict_marker s =
  String.equal s "======="
  || List.exists [ "<<<<<<< "; "||||||| "; ">>>>>>> " ] ~f:(fun prefix ->
    String.is_prefix s ~prefix)
;;

let is_spaces s = for_all_string s ~f:is_space
let no_nl s = for_all_string s ~f:(fun c -> Char.( <> ) c '\n')
let has_nl s = not (no_nl s)

module Line = struct
  type 'a not_blank =
    { trailing_blanks : string
    ; orig : string
    ; data : 'a
    }
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a not_blank) -> ()

  let sexp_of_not_blank : 'a. ('a -> Sexplib0.Sexp.t) -> 'a not_blank -> Sexplib0.Sexp.t =
    fun _of_a__001_
      { trailing_blanks = trailing_blanks__003_; orig = orig__005_; data = data__007_ } ->
      let bnds__002_ = [] in
      let bnds__002_ =
        let arg__008_ = _of_a__001_ data__007_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "data"; arg__008_ ] :: bnds__002_
      in
      let bnds__002_ =
        let arg__006_ = sexp_of_string orig__005_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "orig"; arg__006_ ] :: bnds__002_
      in
      let bnds__002_ =
        let arg__004_ = sexp_of_string trailing_blanks__003_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_blanks"; arg__004_ ] :: bnds__002_
      in
      Sexplib0.Sexp.List bnds__002_
  ;;

  let _ = sexp_of_not_blank

  let compare_not_blank : 'a. ('a -> 'a -> int) -> 'a not_blank -> 'a not_blank -> int =
    fun _cmp__a a__009_ b__010_ ->
    if Ppx_compare_lib.phys_equal a__009_ b__010_
    then 0
    else (
      match compare_string a__009_.trailing_blanks b__010_.trailing_blanks with
      | 0 ->
        (match compare_string a__009_.orig b__010_.orig with
         | 0 -> _cmp__a a__009_.data b__010_.data
         | n -> n)
      | n -> n)
  ;;

  let _ = compare_not_blank

  let equal_not_blank : 'a. ('a -> 'a -> bool) -> 'a not_blank -> 'a not_blank -> bool =
    fun _cmp__a a__011_ b__012_ ->
    if Ppx_compare_lib.phys_equal a__011_ b__012_
    then true
    else
      Ppx_compare_lib.( && )
        (equal_string a__011_.trailing_blanks b__012_.trailing_blanks)
        (Ppx_compare_lib.( && )
           (equal_string a__011_.orig b__012_.orig)
           (_cmp__a a__011_.data b__012_.data))
  ;;

  let _ = equal_not_blank

  [@@@end]

  type 'a t =
    | Blank of string
    | Conflict_marker of string
    | Not_blank of 'a not_blank
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun (type a__020_) : ((a__020_ -> Sexplib0.Sexp.t) -> a__020_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__013_ -> function
      | Blank arg0__014_ ->
        let res0__015_ = sexp_of_string arg0__014_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Blank"; res0__015_ ]
      | Conflict_marker arg0__016_ ->
        let res0__017_ = sexp_of_string arg0__016_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Conflict_marker"; res0__017_ ]
      | Not_blank arg0__018_ ->
        let res0__019_ = sexp_of_not_blank _of_a__013_ arg0__018_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Not_blank"; res0__019_ ]
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__021_ b__022_ ->
    if Ppx_compare_lib.phys_equal a__021_ b__022_
    then 0
    else (
      match a__021_, b__022_ with
      | Blank _a__023_, Blank _b__024_ -> compare_string _a__023_ _b__024_
      | Blank _, _ -> -1
      | _, Blank _ -> 1
      | Conflict_marker _a__025_, Conflict_marker _b__026_ ->
        compare_string _a__025_ _b__026_
      | Conflict_marker _, _ -> -1
      | _, Conflict_marker _ -> 1
      | Not_blank _a__027_, Not_blank _b__028_ ->
        compare_not_blank _cmp__a _a__027_ _b__028_)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__031_ b__032_ ->
    if Ppx_compare_lib.phys_equal a__031_ b__032_
    then true
    else (
      match a__031_, b__032_ with
      | Blank _a__033_, Blank _b__034_ -> equal_string _a__033_ _b__034_
      | Blank _, _ -> false
      | _, Blank _ -> false
      | Conflict_marker _a__035_, Conflict_marker _b__036_ ->
        equal_string _a__035_ _b__036_
      | Conflict_marker _, _ -> false
      | _, Conflict_marker _ -> false
      | Not_blank _a__037_, Not_blank _b__038_ ->
        equal_not_blank _cmp__a _a__037_ _b__038_)
  ;;

  let _ = equal

  [@@@end]

  let map t ~f =
    match t with
    | Blank b -> Blank b
    | Conflict_marker c -> Conflict_marker c
    | Not_blank n -> Not_blank { n with data = f n.orig n.data }
  ;;

  let strip = function
    | Blank _ -> Blank ""
    | Conflict_marker c -> Conflict_marker (String.rstrip c)
    | Not_blank n -> Not_blank { n with trailing_blanks = "" }
  ;;

  let invariant inv = function
    | Blank s -> assert (is_blanks s)
    | Conflict_marker c -> assert (is_conflict_marker c)
    | Not_blank n ->
      assert (is_blanks n.trailing_blanks);
      inv n.data;
      assert (no_nl n.orig);
      let len = String.length n.orig in
      assert (len > 0 && not (is_blank n.orig.[len - 1]))
  ;;

  let data t ~blank ~conflict_marker =
    match t with
    | Blank _ -> blank
    | Conflict_marker marker -> conflict_marker marker
    | Not_blank n -> n.data
  ;;

  let orig = function
    | Blank _ -> ""
    | Conflict_marker c -> c
    | Not_blank n -> n.orig
  ;;
end

type 'a single_line =
  { leading_blanks : string
  ; trailing_spaces : string
  ; orig : string
  ; data : 'a
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a single_line) -> ()

let sexp_of_single_line : 'a. ('a -> Sexplib0.Sexp.t) -> 'a single_line -> Sexplib0.Sexp.t
  =
  fun _of_a__041_
    { leading_blanks = leading_blanks__043_
    ; trailing_spaces = trailing_spaces__045_
    ; orig = orig__047_
    ; data = data__049_
    } ->
    let bnds__042_ = [] in
    let bnds__042_ =
      let arg__050_ = _of_a__041_ data__049_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "data"; arg__050_ ] :: bnds__042_
    in
    let bnds__042_ =
      let arg__048_ = sexp_of_string orig__047_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "orig"; arg__048_ ] :: bnds__042_
    in
    let bnds__042_ =
      let arg__046_ = sexp_of_string trailing_spaces__045_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_spaces"; arg__046_ ] :: bnds__042_
    in
    let bnds__042_ =
      let arg__044_ = sexp_of_string leading_blanks__043_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "leading_blanks"; arg__044_ ] :: bnds__042_
    in
    Sexplib0.Sexp.List bnds__042_
;;

let _ = sexp_of_single_line

let compare_single_line : 'a. ('a -> 'a -> int) -> 'a single_line -> 'a single_line -> int
  =
  fun _cmp__a a__051_ b__052_ ->
  if Ppx_compare_lib.phys_equal a__051_ b__052_
  then 0
  else (
    match compare_string a__051_.leading_blanks b__052_.leading_blanks with
    | 0 ->
      (match compare_string a__051_.trailing_spaces b__052_.trailing_spaces with
       | 0 ->
         (match compare_string a__051_.orig b__052_.orig with
          | 0 -> _cmp__a a__051_.data b__052_.data
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_single_line

let equal_single_line : 'a. ('a -> 'a -> bool) -> 'a single_line -> 'a single_line -> bool
  =
  fun _cmp__a a__053_ b__054_ ->
  if Ppx_compare_lib.phys_equal a__053_ b__054_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__053_.leading_blanks b__054_.leading_blanks)
      (Ppx_compare_lib.( && )
         (equal_string a__053_.trailing_spaces b__054_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__053_.orig b__054_.orig)
            (_cmp__a a__053_.data b__054_.data)))
;;

let _ = equal_single_line

[@@@end]

type 'a multi_lines =
  { leading_spaces : string
  ; trailing_spaces : string
  ; indentation : string
  ; lines : 'a Line.t list
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a multi_lines) -> ()

let sexp_of_multi_lines : 'a. ('a -> Sexplib0.Sexp.t) -> 'a multi_lines -> Sexplib0.Sexp.t
  =
  fun _of_a__055_
    { leading_spaces = leading_spaces__057_
    ; trailing_spaces = trailing_spaces__059_
    ; indentation = indentation__061_
    ; lines = lines__063_
    } ->
    let bnds__056_ = [] in
    let bnds__056_ =
      let arg__064_ = sexp_of_list (Line.sexp_of_t _of_a__055_) lines__063_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "lines"; arg__064_ ] :: bnds__056_
    in
    let bnds__056_ =
      let arg__062_ = sexp_of_string indentation__061_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "indentation"; arg__062_ ] :: bnds__056_
    in
    let bnds__056_ =
      let arg__060_ = sexp_of_string trailing_spaces__059_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_spaces"; arg__060_ ] :: bnds__056_
    in
    let bnds__056_ =
      let arg__058_ = sexp_of_string leading_spaces__057_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "leading_spaces"; arg__058_ ] :: bnds__056_
    in
    Sexplib0.Sexp.List bnds__056_
;;

let _ = sexp_of_multi_lines

let compare_multi_lines : 'a. ('a -> 'a -> int) -> 'a multi_lines -> 'a multi_lines -> int
  =
  fun _cmp__a a__065_ b__066_ ->
  if Ppx_compare_lib.phys_equal a__065_ b__066_
  then 0
  else (
    match compare_string a__065_.leading_spaces b__066_.leading_spaces with
    | 0 ->
      (match compare_string a__065_.trailing_spaces b__066_.trailing_spaces with
       | 0 ->
         (match compare_string a__065_.indentation b__066_.indentation with
          | 0 ->
            compare_list
              (fun a__067_ b__068_ -> Line.compare _cmp__a a__067_ b__068_)
              a__065_.lines
              b__066_.lines
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_multi_lines

let equal_multi_lines : 'a. ('a -> 'a -> bool) -> 'a multi_lines -> 'a multi_lines -> bool
  =
  fun _cmp__a a__071_ b__072_ ->
  if Ppx_compare_lib.phys_equal a__071_ b__072_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__071_.leading_spaces b__072_.leading_spaces)
      (Ppx_compare_lib.( && )
         (equal_string a__071_.trailing_spaces b__072_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__071_.indentation b__072_.indentation)
            (equal_list
               (fun a__073_ b__074_ -> Line.equal _cmp__a a__073_ b__074_)
               a__071_.lines
               b__072_.lines)))
;;

let _ = equal_multi_lines

[@@@end]

type 'a t =
  | Empty of string
  | Single_line of 'a single_line
  | Multi_lines of 'a multi_lines
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a t) -> ()

let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun (type a__084_) : ((a__084_ -> Sexplib0.Sexp.t) -> a__084_ t -> Sexplib0.Sexp.t) ->
  fun _of_a__077_ -> function
    | Empty arg0__078_ ->
      let res0__079_ = sexp_of_string arg0__078_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Empty"; res0__079_ ]
    | Single_line arg0__080_ ->
      let res0__081_ = sexp_of_single_line _of_a__077_ arg0__080_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Single_line"; res0__081_ ]
    | Multi_lines arg0__082_ ->
      let res0__083_ = sexp_of_multi_lines _of_a__077_ arg0__082_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Multi_lines"; res0__083_ ]
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__085_ b__086_ ->
  if Ppx_compare_lib.phys_equal a__085_ b__086_
  then 0
  else (
    match a__085_, b__086_ with
    | Empty _a__087_, Empty _b__088_ -> compare_string _a__087_ _b__088_
    | Empty _, _ -> -1
    | _, Empty _ -> 1
    | Single_line _a__089_, Single_line _b__090_ ->
      compare_single_line _cmp__a _a__089_ _b__090_
    | Single_line _, _ -> -1
    | _, Single_line _ -> 1
    | Multi_lines _a__093_, Multi_lines _b__094_ ->
      compare_multi_lines _cmp__a _a__093_ _b__094_)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__097_ b__098_ ->
  if Ppx_compare_lib.phys_equal a__097_ b__098_
  then true
  else (
    match a__097_, b__098_ with
    | Empty _a__099_, Empty _b__100_ -> equal_string _a__099_ _b__100_
    | Empty _, _ -> false
    | _, Empty _ -> false
    | Single_line _a__101_, Single_line _b__102_ ->
      equal_single_line _cmp__a _a__101_ _b__102_
    | Single_line _, _ -> false
    | _, Single_line _ -> false
    | Multi_lines _a__105_, Multi_lines _b__106_ ->
      equal_multi_lines _cmp__a _a__105_ _b__106_)
;;

let _ = equal

[@@@end]

let invariant inv t =
  match t with
  | Empty s -> assert (is_spaces s)
  | Single_line s ->
    assert (is_blanks s.leading_blanks);
    assert (is_spaces s.trailing_spaces);
    inv s.data;
    assert (no_nl s.orig);
    let len = String.length s.orig in
    assert (len > 0 && (not (is_blank s.orig.[0])) && not (is_blank s.orig.[len - 1]))
  | Multi_lines m ->
    assert (is_spaces m.leading_spaces);
    let ld_len = String.length m.leading_spaces in
    assert (ld_len = 0 || Char.equal m.leading_spaces.[ld_len - 1] '\n');
    let tr_has_nl = has_nl m.trailing_spaces in
    assert (
      is_spaces m.trailing_spaces
      && ((not tr_has_nl) || Char.equal m.trailing_spaces.[0] '\n'));
    assert (is_blanks m.indentation);
    List.iter m.lines ~f:(Line.invariant inv);
    (match m.lines with
     | [] -> assert false
     | Blank _ :: _ -> assert false
     | [ Not_blank n ] ->
       assert (ld_len > 0 && (tr_has_nl || String.is_empty n.trailing_blanks))
     | l ->
       let rec check_last = function
         | ([] : _ Line.t list) -> assert false
         | [ Blank _ ] -> assert false
         | [ Not_blank n ] -> assert (tr_has_nl || String.is_empty n.trailing_blanks)
         | [ Conflict_marker m ] -> assert (not (String.is_empty m))
         | _ :: (_ :: _ as l) -> check_last l
       in
       check_last l)
;;

let empty = Empty ""

let map t ~f =
  match t with
  | Empty e -> Empty e
  | Single_line s -> Single_line { s with data = f s.orig s.data }
  | Multi_lines m -> Multi_lines { m with lines = List.map m.lines ~f:(Line.map ~f) }
;;

let data t ~blank ~conflict_marker =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.data ]
  | Multi_lines m -> List.map m.lines ~f:(Line.data ~blank ~conflict_marker)
;;

let stripped_original_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.orig ]
  | Multi_lines m -> List.map m.lines ~f:Line.orig
;;

let line_of_single s : _ Line.t =
  Not_blank { trailing_blanks = ""; orig = s.orig; data = s.data }
;;

let to_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ line_of_single s ]
  | Multi_lines m -> m.lines
;;

let strip t =
  match t with
  | Empty _ -> Empty ""
  | Single_line s -> Single_line { s with leading_blanks = ""; trailing_spaces = "" }
  | Multi_lines m ->
    (match m.lines with
     | [] -> Empty ""
     | [ Blank _ ] -> assert false
     | [ Not_blank n ] ->
       Single_line
         { leading_blanks = ""; trailing_spaces = ""; orig = n.orig; data = n.data }
     | lines ->
       Multi_lines
         { leading_spaces = ""
         ; trailing_spaces = ""
         ; indentation = ""
         ; lines = List.map lines ~f:Line.strip
         })
;;

let to_string t =
  match t with
  | Empty s -> s
  | Single_line s -> s.leading_blanks ^ s.orig ^ s.trailing_spaces
  | Multi_lines m ->
    let indent (line : _ Line.t) =
      match line with
      | Blank b -> b
      | Conflict_marker c -> c
      | Not_blank n -> m.indentation ^ n.orig ^ n.trailing_blanks
    in
    let s = List.map m.lines ~f:indent |> String.concat ~sep:"\n" in
    m.leading_spaces ^ s ^ m.trailing_spaces
;;

let trim_lines lines =
  let rec loop0 : _ Line.t list -> _ = function
    | Blank _ :: l -> loop0 l
    | l -> loop1 l ~acc:[] ~acc_with_trailing_blanks:[]
  and loop1 ~acc ~acc_with_trailing_blanks = function
    | (Blank _ as x) :: l ->
      loop1 l ~acc ~acc_with_trailing_blanks:(x :: acc_with_trailing_blanks)
    | ((Conflict_marker _ | Not_blank _) as x) :: l ->
      let acc = x :: acc_with_trailing_blanks in
      loop1 l ~acc ~acc_with_trailing_blanks:acc
    | [] -> List.rev acc
  in
  loop0 lines
;;

let not_blank_or_conflict_lines lines =
  List.fold_left lines ~init:[] ~f:(fun acc (l : _ Line.t) ->
    match l with
    | Blank _ | Conflict_marker _ -> acc
    | Not_blank n -> n.orig :: acc)
  |> List.rev
;;

let longest_common_prefix a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let len = min len_a len_b in
  let i = ref 0 in
  while !i < len && Char.equal a.[!i] b.[!i] do
    Int.incr i
  done;
  String.sub a ~pos:0 ~len:!i
;;

let indentation s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_blank s.[!i] do
    Int.incr i
  done;
  String.sub s ~pos:0 ~len:!i
;;

let extract_indentation lines =
  match not_blank_or_conflict_lines lines with
  | [] -> "", lines
  | first :: rest ->
    let indent = List.fold_left rest ~init:(indentation first) ~f:longest_common_prefix in
    let indent_len = String.length indent in
    let update_line : 'a Line.t -> 'a Line.t = function
      | Blank b -> Blank b
      | Conflict_marker c -> Conflict_marker c
      | Not_blank n ->
        let orig =
          String.sub n.orig ~pos:indent_len ~len:(String.length n.orig - indent_len)
        in
        Not_blank { n with orig }
    in
    indent, List.map lines ~f:update_line
;;

let break s at = String.prefix s at, String.drop_prefix s at

let reconcile (type a) t ~lines ~default_indentation ~pad_single_line =
  let module M = struct
    type t =
      | Empty
      | Single_line of a Line.not_blank
      | Multi_lines of a Line.t list
  end
  in
  let lines =
    match trim_lines lines |> extract_indentation |> snd with
    | [] -> M.Empty
    | [ Blank _ ] -> assert false
    | [ Not_blank n ] -> M.Single_line n
    | lines -> M.Multi_lines lines
  in
  let padding = if pad_single_line then " " else "" in
  let res =
    match t, lines with
    | Empty _, Empty -> t
    | Single_line s, Single_line n -> Single_line { s with orig = n.orig; data = n.data }
    | Multi_lines m, Multi_lines l -> Multi_lines { m with lines = l }
    | Empty e, Multi_lines l ->
      let ld, tr =
        if has_nl e
        then (
          let ld, tr = break e (String.index_exn e '\n') in
          ld ^ "\n", tr)
        else "\n", padding
      in
      Multi_lines
        { leading_spaces = ld
        ; trailing_spaces = tr
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line m, Multi_lines l ->
      Multi_lines
        { leading_spaces = "\n"
        ; trailing_spaces = m.trailing_spaces
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line _, Empty | Multi_lines _, Empty -> Empty padding
    | Empty _, Single_line n ->
      Single_line
        { orig = n.orig
        ; data = n.data
        ; leading_blanks = padding
        ; trailing_spaces = padding
        }
    | Multi_lines m, Single_line n -> Multi_lines { m with lines = [ Not_blank n ] }
  in
  invariant ignore res;
  res
;;
