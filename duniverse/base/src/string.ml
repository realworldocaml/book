open! Import
module Array = Array0
module Bytes = Bytes0
include String0

let invalid_argf = Printf.invalid_argf
let raise_s = Error.raise_s
let stage = Staged.stage

module T = struct
  type t = string [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_string

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_string in
    fun x -> func x
  ;;

  let t_of_sexp = (string_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_string : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "string" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ string_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "string.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare = compare
end

include T
include Comparator.Make (T)

type elt = char

let invariant (_ : t) = ()

(* This is copied/adapted from 'blit.ml'.
   [sub], [subo] could be implemented using [Blit.Make(Bytes)] plus unsafe casts to/from
   string but were inlined here to avoid using [Bytes.unsafe_of_string] as much as possible.
*)
let sub src ~pos ~len =
  if pos = 0 && len = String.length src
  then src
  else (
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(length src);
    let dst = Bytes.create len in
    if len > 0 then Bytes.unsafe_blit_string ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
;;

let subo ?(pos = 0) ?len src =
  sub
    src
    ~pos
    ~len:
      (match len with
       | Some i -> i
       | None -> length src - pos)
;;

let rec contains_unsafe t ~pos ~end_ char =
  pos < end_
  && (Char.equal (unsafe_get t pos) char || contains_unsafe t ~pos:(pos + 1) ~end_ char)
;;

let contains ?(pos = 0) ?len t char =
  let total_length = String.length t in
  let len = Option.value len ~default:(total_length - pos) in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
  contains_unsafe t ~pos ~end_:(pos + len) char
;;

let is_empty t = length t = 0

let rec index_from_exn_internal string ~pos ~len ~not_found char =
  if pos >= len
  then raise not_found
  else if Char.equal (unsafe_get string pos) char
  then pos
  else index_from_exn_internal string ~pos:(pos + 1) ~len ~not_found char
;;

let index_exn_internal t ~not_found char =
  index_from_exn_internal t ~pos:0 ~len:(length t) ~not_found char
;;

let index_exn =
  let not_found = Not_found_s (Atom "String.index_exn: not found") in
  let index_exn t char = index_exn_internal t ~not_found char in
  (* named to preserve symbol in compiled binary *)
  index_exn
;;

let index_from_exn =
  let not_found = Not_found_s (Atom "String.index_from_exn: not found") in
  let index_from_exn t pos char =
    let len = length t in
    if pos < 0 || pos > len
    then invalid_arg "String.index_from_exn"
    else index_from_exn_internal t ~pos ~len ~not_found char
  in
  (* named to preserve symbol in compiled binary *)
  index_from_exn
;;

let rec rindex_from_exn_internal string ~pos ~len ~not_found char =
  if pos < 0
  then raise not_found
  else if Char.equal (unsafe_get string pos) char
  then pos
  else rindex_from_exn_internal string ~pos:(pos - 1) ~len ~not_found char
;;

let rindex_exn_internal t ~not_found char =
  let len = length t in
  rindex_from_exn_internal t ~pos:(len - 1) ~len ~not_found char
;;

let rindex_exn =
  let not_found = Not_found_s (Atom "String.rindex_exn: not found") in
  let rindex_exn t char = rindex_exn_internal t ~not_found char in
  (* named to preserve symbol in compiled binary *)
  rindex_exn
;;

let rindex_from_exn =
  let not_found = Not_found_s (Atom "String.rindex_from_exn: not found") in
  let rindex_from_exn t pos char =
    let len = length t in
    if pos < -1 || pos >= len
    then invalid_arg "String.rindex_from_exn"
    else rindex_from_exn_internal t ~pos ~len ~not_found char
  in
  (* named to preserve symbol in compiled binary *)
  rindex_from_exn
;;

let index t char =
  try Some (index_exn t char) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let rindex t char =
  try Some (rindex_exn t char) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let index_from t pos char =
  try Some (index_from_exn t pos char) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let rindex_from t pos char =
  try Some (rindex_from_exn t pos char) with
  | Not_found_s _ | Caml.Not_found -> None
;;

module Search_pattern0 = struct
  type t =
    { pattern : string
    ; case_sensitive : bool
    ; kmp_array : int array
    }

  let sexp_of_t { pattern; case_sensitive; kmp_array = _ } : Sexp.t =
    List
      [ List [ Atom "pattern"; sexp_of_string pattern ]
      ; List [ Atom "case_sensitive"; sexp_of_bool case_sensitive ]
      ]
  ;;

  let pattern t = t.pattern
  let case_sensitive t = t.case_sensitive

  (* Find max number of matched characters at [next_text_char], given the current
     [matched_chars]. Try to extend the current match, if chars don't match, try to match
     fewer chars. If chars match then extend the match. *)
  let kmp_internal_loop ~matched_chars ~next_text_char ~pattern ~kmp_array ~char_equal =
    let matched_chars = ref matched_chars in
    while
      !matched_chars > 0
      && not (char_equal next_text_char (unsafe_get pattern !matched_chars))
    do
      matched_chars := Array.unsafe_get kmp_array (!matched_chars - 1)
    done;
    if char_equal next_text_char (unsafe_get pattern !matched_chars)
    then matched_chars := !matched_chars + 1;
    !matched_chars
  ;;

  let get_char_equal ~case_sensitive =
    match case_sensitive with
    | true -> Char.equal
    | false -> Char.Caseless.equal
  ;;

  (* Classic KMP pre-processing of the pattern: build the int array, which, for each i,
     contains the length of the longest non-trivial prefix of s which is equal to a suffix
     ending at s.[i] *)
  let create pattern ~case_sensitive =
    let n = length pattern in
    let kmp_array = Array.create ~len:n (-1) in
    if n > 0
    then (
      let char_equal = get_char_equal ~case_sensitive in
      Array.unsafe_set kmp_array 0 0;
      let matched_chars = ref 0 in
      for i = 1 to n - 1 do
        matched_chars
        := kmp_internal_loop
             ~matched_chars:!matched_chars
             ~next_text_char:(unsafe_get pattern i)
             ~pattern
             ~kmp_array
             ~char_equal;
        Array.unsafe_set kmp_array i !matched_chars
      done);
    { pattern; case_sensitive; kmp_array }
  ;;

  (* Classic KMP: use the pre-processed pattern to optimize look-behinds on non-matches.
     We return int to avoid allocation in [index_exn]. -1 means no match. *)
  let index_internal ?(pos = 0) { pattern; case_sensitive; kmp_array } ~in_:text =
    if pos < 0 || pos > length text - length pattern
    then -1
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let j = ref pos in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      while !j < n && !matched_chars < k do
        let next_text_char = unsafe_get text !j in
        matched_chars
        := kmp_internal_loop
             ~matched_chars:!matched_chars
             ~next_text_char
             ~pattern
             ~kmp_array
             ~char_equal;
        j := !j + 1
      done;
      if !matched_chars = k then !j - k else -1)
  ;;

  let matches t str = index_internal t ~in_:str >= 0

  let index ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p < 0 then None else Some p
  ;;

  let index_exn ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p >= 0
    then p
    else
      raise_s
        (Sexp.message "Substring not found" [ "substring", sexp_of_string t.pattern ])
  ;;

  let index_all { pattern; case_sensitive; kmp_array } ~may_overlap ~in_:text =
    if length pattern = 0
    then List.init (1 + length text) ~f:Fn.id
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      let found = ref [] in
      for j = 0 to n do
        if !matched_chars = k
        then (
          found := (j - k) :: !found;
          (* we just found a match in the previous iteration *)
          match may_overlap with
          | true -> matched_chars := Array.unsafe_get kmp_array (k - 1)
          | false -> matched_chars := 0);
        if j < n
        then (
          let next_text_char = unsafe_get text j in
          matched_chars
          := kmp_internal_loop
               ~matched_chars:!matched_chars
               ~next_text_char
               ~pattern
               ~kmp_array
               ~char_equal)
      done;
      List.rev !found)
  ;;

  let replace_first ?pos t ~in_:s ~with_ =
    match index ?pos t ~in_:s with
    | None -> s
    | Some i ->
      let len_s = length s in
      let len_t = length t.pattern in
      let len_with = length with_ in
      let dst = Bytes.create (len_s + len_with - len_t) in
      Bytes.blit_string ~src:s ~src_pos:0 ~dst ~dst_pos:0 ~len:i;
      Bytes.blit_string ~src:with_ ~src_pos:0 ~dst ~dst_pos:i ~len:len_with;
      Bytes.blit_string
        ~src:s
        ~src_pos:(i + len_t)
        ~dst
        ~dst_pos:(i + len_with)
        ~len:(len_s - i - len_t);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
  ;;


  let replace_all t ~in_:s ~with_ =
    let matches = index_all t ~may_overlap:false ~in_:s in
    match matches with
    | [] -> s
    | _ :: _ ->
      let len_s = length s in
      let len_t = length t.pattern in
      let len_with = length with_ in
      let num_matches = List.length matches in
      let dst = Bytes.create (len_s + ((len_with - len_t) * num_matches)) in
      let next_dst_pos = ref 0 in
      let next_src_pos = ref 0 in
      List.iter matches ~f:(fun i ->
        let len = i - !next_src_pos in
        Bytes.blit_string
          ~src:s
          ~src_pos:!next_src_pos
          ~dst
          ~dst_pos:!next_dst_pos
          ~len;
        Bytes.blit_string
          ~src:with_
          ~src_pos:0
          ~dst
          ~dst_pos:(!next_dst_pos + len)
          ~len:len_with;
        next_dst_pos := !next_dst_pos + len + len_with;
        next_src_pos := !next_src_pos + len + len_t);
      Bytes.blit_string
        ~src:s
        ~src_pos:!next_src_pos
        ~dst
        ~dst_pos:!next_dst_pos
        ~len:(len_s - !next_src_pos);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
  ;;

  module Private = struct
    type public = t

    type nonrec t = t =
      { pattern : string
      ; case_sensitive : bool
      ; kmp_array : int array
      }
    [@@deriving_inline equal, sexp_of]

    let equal =
      (fun a__001_ b__002_ ->
         if Ppx_compare_lib.phys_equal a__001_ b__002_
         then true
         else
           Ppx_compare_lib.( && )
             (equal_string a__001_.pattern b__002_.pattern)
             (Ppx_compare_lib.( && )
                (equal_bool a__001_.case_sensitive b__002_.case_sensitive)
                (equal_array equal_int a__001_.kmp_array b__002_.kmp_array))
           : t -> t -> bool)
    ;;

    let sexp_of_t =
      (function
        | { pattern = v_pattern
          ; case_sensitive = v_case_sensitive
          ; kmp_array = v_kmp_array
          } ->
          let bnds = [] in
          let bnds =
            let arg = sexp_of_array sexp_of_int v_kmp_array in
            Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "kmp_array"; arg ]
            :: bnds
          in
          let bnds =
            let arg = sexp_of_bool v_case_sensitive in
            Ppx_sexp_conv_lib.Sexp.List
              [ Ppx_sexp_conv_lib.Sexp.Atom "case_sensitive"; arg ]
            :: bnds
          in
          let bnds =
            let arg = sexp_of_string v_pattern in
            Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "pattern"; arg ]
            :: bnds
          in
          Ppx_sexp_conv_lib.Sexp.List bnds
          : t -> Ppx_sexp_conv_lib.Sexp.t)
    ;;

    [@@@end]

    let representation = Fn.id
  end
end

module Search_pattern_helper = struct
  module Search_pattern = Search_pattern0
end

open Search_pattern_helper

let substr_index_gen ~case_sensitive ?pos t ~pattern =
  Search_pattern.index ?pos (Search_pattern.create ~case_sensitive pattern) ~in_:t
;;

let substr_index_exn_gen ~case_sensitive ?pos t ~pattern =
  Search_pattern.index_exn ?pos (Search_pattern.create ~case_sensitive pattern) ~in_:t
;;

let substr_index_all_gen ~case_sensitive t ~may_overlap ~pattern =
  Search_pattern.index_all
    (Search_pattern.create ~case_sensitive pattern)
    ~may_overlap
    ~in_:t
;;

let substr_replace_first_gen ~case_sensitive ?pos t ~pattern =
  Search_pattern.replace_first
    ?pos
    (Search_pattern.create ~case_sensitive pattern)
    ~in_:t
;;

let substr_replace_all_gen ~case_sensitive t ~pattern =
  Search_pattern.replace_all (Search_pattern.create ~case_sensitive pattern) ~in_:t
;;

let is_substring_gen ~case_sensitive t ~substring =
  Option.is_some (substr_index_gen t ~pattern:substring ~case_sensitive)
;;

let substr_index = substr_index_gen ~case_sensitive:true
let substr_index_exn = substr_index_exn_gen ~case_sensitive:true
let substr_index_all = substr_index_all_gen ~case_sensitive:true
let substr_replace_first = substr_replace_first_gen ~case_sensitive:true
let substr_replace_all = substr_replace_all_gen ~case_sensitive:true
let is_substring = is_substring_gen ~case_sensitive:true

let is_substring_at_gen =
  let rec loop ~str ~str_pos ~sub ~sub_pos ~sub_len ~char_equal =
    if sub_pos = sub_len
    then true
    else if char_equal (unsafe_get str str_pos) (unsafe_get sub sub_pos)
    then
      loop ~str ~str_pos:(str_pos + 1) ~sub ~sub_pos:(sub_pos + 1) ~sub_len ~char_equal
    else false
  in
  fun str ~pos:str_pos ~substring:sub ~char_equal ->
    let str_len = length str in
    let sub_len = length sub in
    if str_pos < 0 || str_pos > str_len
    then
      invalid_argf
        "String.is_substring_at: invalid index %d for string of length %d"
        str_pos
        str_len
        ();
    str_pos + sub_len <= str_len
    && loop ~str ~str_pos ~sub ~sub_pos:0 ~sub_len ~char_equal
;;

let is_suffix_gen string ~suffix ~char_equal =
  let string_len = length string in
  let suffix_len = length suffix in
  string_len >= suffix_len
  && is_substring_at_gen
       string
       ~pos:(string_len - suffix_len)
       ~substring:suffix
       ~char_equal
;;

let is_prefix_gen string ~prefix ~char_equal =
  let string_len = length string in
  let prefix_len = length prefix in
  string_len >= prefix_len
  && is_substring_at_gen string ~pos:0 ~substring:prefix ~char_equal
;;

module Caseless = struct
  module T = struct
    type t = string [@@deriving_inline sexp]

    let t_of_sexp = (string_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
    let sexp_of_t = (sexp_of_string : t -> Ppx_sexp_conv_lib.Sexp.t)

    [@@@end]

    let char_compare_caseless c1 c2 =
      Char.compare (Char.lowercase c1) (Char.lowercase c2)
    ;;

    let rec compare_loop ~pos ~string1 ~len1 ~string2 ~len2 =
      if pos = len1
      then if pos = len2 then 0 else -1
      else if pos = len2
      then 1
      else (
        let c =
          char_compare_caseless (unsafe_get string1 pos) (unsafe_get string2 pos)
        in
        match c with
        | 0 -> compare_loop ~pos:(pos + 1) ~string1 ~len1 ~string2 ~len2
        | _ -> c)
    ;;

    let compare string1 string2 =
      if phys_equal string1 string2
      then 0
      else
        compare_loop
          ~pos:0
          ~string1
          ~len1:(String.length string1)
          ~string2
          ~len2:(String.length string2)
    ;;

    let hash_fold_t state t =
      let len = length t in
      let state = ref (hash_fold_int state len) in
      for pos = 0 to len - 1 do
        state := hash_fold_char !state (Char.lowercase (unsafe_get t pos))
      done;
      !state
    ;;

    let hash t = Hash.run hash_fold_t t
    let is_suffix s ~suffix = is_suffix_gen s ~suffix ~char_equal:Char.Caseless.equal
    let is_prefix s ~prefix = is_prefix_gen s ~prefix ~char_equal:Char.Caseless.equal
    let substr_index = substr_index_gen ~case_sensitive:false
    let substr_index_exn = substr_index_exn_gen ~case_sensitive:false
    let substr_index_all = substr_index_all_gen ~case_sensitive:false
    let substr_replace_first = substr_replace_first_gen ~case_sensitive:false
    let substr_replace_all = substr_replace_all_gen ~case_sensitive:false
    let is_substring = is_substring_gen ~case_sensitive:false
    let is_substring_at = is_substring_at_gen ~char_equal:Char.Caseless.equal
  end

  include T
  include Comparable.Make (T)
end

let of_string = Fn.id
let to_string = Fn.id

let init n ~f =
  if n < 0 then invalid_argf "String.init %d" n ();
  let t = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set t i (f i)
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t
;;

let to_list s =
  let rec loop acc i = if i < 0 then acc else loop (s.[i] :: acc) (i - 1) in
  loop [] (length s - 1)
;;

let to_list_rev s =
  let len = length s in
  let rec loop acc i = if i = len then acc else loop (s.[i] :: acc) (i + 1) in
  loop [] 0
;;

let rev t =
  let len = length t in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    unsafe_set res i (unsafe_get t (len - 1 - i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
;;

(** Efficient string splitting *)

let lsplit2_exn =
  let not_found = Not_found_s (Atom "String.lsplit2_exn: not found") in
  let lsplit2_exn line ~on:delim =
    let pos = index_exn_internal line ~not_found delim in
    sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(length line - pos - 1)
  in
  (* named to preserve symbol in compiled binary *)
  lsplit2_exn
;;

let rsplit2_exn =
  let not_found = Not_found_s (Atom "String.rsplit2_exn: not found") in
  let rsplit2_exn line ~on:delim =
    let pos = rindex_exn_internal line ~not_found delim in
    sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(length line - pos - 1)
  in
  (* named to preserve symbol in compiled binary *)
  rsplit2_exn
;;

let lsplit2 line ~on =
  try Some (lsplit2_exn line ~on) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let rsplit2 line ~on =
  try Some (rsplit2_exn line ~on) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let rec char_list_mem l (c : char) =
  match l with
  | [] -> false
  | hd :: tl -> Char.equal hd c || char_list_mem tl c
;;

let split_gen str ~on =
  let is_delim =
    match on with
    | `char c' -> fun c -> Char.equal c c'
    | `char_list l -> fun c -> char_list_mem l c
  in
  let len = length str in
  let rec loop acc last_pos pos =
    if pos = -1
    then sub str ~pos:0 ~len:last_pos :: acc
    else if is_delim str.[pos]
    then (
      let pos1 = pos + 1 in
      let sub_str = sub str ~pos:pos1 ~len:(last_pos - pos1) in
      loop (sub_str :: acc) pos (pos - 1))
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
;;

let split str ~on = split_gen str ~on:(`char on)
let split_on_chars str ~on:chars = split_gen str ~on:(`char_list chars)

let split_lines =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - if !pos > 0 && Char.equal t.[!pos - 1] '\r' then 2 else 1;
    eol := !pos + 1
  in
  fun t ->
    let n = length t in
    if n = 0
    then []
    else (
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if Char.equal t.[!pos] '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if Char.( <> ) t.[!pos] '\n'
        then decr pos
        else (
          (* Because [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol)
      done;
      sub t ~pos:0 ~len:!eol :: !ac)
;;

let is_suffix s ~suffix = is_suffix_gen s ~suffix ~char_equal:Char.equal
let is_prefix s ~prefix = is_prefix_gen s ~prefix ~char_equal:Char.equal

let is_substring_at s ~pos ~substring =
  is_substring_at_gen s ~pos ~substring ~char_equal:Char.equal
;;

let wrap_sub_n t n ~name ~pos ~len ~on_error =
  if n < 0
  then invalid_arg (name ^ " expecting nonnegative argument")
  else (
    try sub t ~pos ~len with
    | _ -> on_error)
;;

let drop_prefix t n =
  wrap_sub_n ~name:"drop_prefix" t n ~pos:n ~len:(length t - n) ~on_error:""
;;

let drop_suffix t n =
  wrap_sub_n ~name:"drop_suffix" t n ~pos:0 ~len:(length t - n) ~on_error:""
;;

let prefix t n = wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~on_error:t
let suffix t n = wrap_sub_n ~name:"suffix" t n ~pos:(length t - n) ~len:n ~on_error:t

let lfindi ?(pos = 0) t ~f =
  let n = length t in
  let rec loop i = if i = n then None else if f i t.[i] then Some i else loop (i + 1) in
  loop pos
;;

let find t ~f =
  match lfindi t ~f:(fun _ c -> f c) with
  | None -> None
  | Some i -> Some t.[i]
;;

let find_map t ~f =
  let n = length t in
  let rec loop i =
    if i = n
    then None
    else (
      match f t.[i] with
      | None -> loop (i + 1)
      | Some _ as res -> res)
  in
  loop 0
;;

let rfindi ?pos t ~f =
  let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
  let pos =
    match pos with
    | Some pos -> pos
    | None -> length t - 1
  in
  loop pos
;;

let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))

let rstrip ?(drop = Char.is_whitespace) t =
  match last_non_drop t ~drop with
  | None -> ""
  | Some i -> if i = length t - 1 then t else prefix t (i + 1)
;;

let first_non_drop ~drop t = lfindi t ~f:(fun _ c -> not (drop c))

let lstrip ?(drop = Char.is_whitespace) t =
  match first_non_drop t ~drop with
  | None -> ""
  | Some 0 -> t
  | Some n -> drop_prefix t n
;;

(* [strip t] could be implemented as [lstrip (rstrip t)].  The implementation
   below saves (at least) a factor of two allocation, by only allocating the
   final result.  This also saves some amount of time. *)
let strip ?(drop = Char.is_whitespace) t =
  let length = length t in
  if length = 0 || not (drop t.[0] || drop t.[length - 1])
  then t
  else (
    match first_non_drop t ~drop with
    | None -> ""
    | Some first ->
      (match last_non_drop t ~drop with
       | None -> assert false
       | Some last -> sub t ~pos:first ~len:(last - first + 1)))
;;

let mapi t ~f =
  let l = length t in
  let t' = Bytes.create l in
  for i = 0 to l - 1 do
    Bytes.unsafe_set t' i (f i t.[i])
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t'
;;

(* repeated code to avoid requiring an extra allocation for a closure on each call. *)
let map t ~f =
  let l = length t in
  let t' = Bytes.create l in
  for i = 0 to l - 1 do
    Bytes.unsafe_set t' i (f t.[i])
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t'
;;

let to_array s = Array.init (length s) ~f:(fun i -> s.[i])

let exists =
  let rec loop s i ~len ~f = i < len && (f s.[i] || loop s (i + 1) ~len ~f) in
  fun s ~f -> loop s 0 ~len:(length s) ~f
;;

let for_all =
  let rec loop s i ~len ~f = i = len || (f s.[i] && loop s (i + 1) ~len ~f) in
  fun s ~f -> loop s 0 ~len:(length s) ~f
;;

let fold t ~init ~f =
  let n = length t in
  let rec loop i ac = if i = n then ac else loop (i + 1) (f ac t.[i]) in
  loop 0 init
;;

let foldi t ~init ~f =
  let n = length t in
  let rec loop i ac = if i = n then ac else loop (i + 1) (f i ac t.[i]) in
  loop 0 init
;;

let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t = Container.min_elt ~fold t
let max_elt t = Container.max_elt ~fold t
let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

let mem =
  let rec loop t c ~pos:i ~len =
    i < len && (Char.equal c (unsafe_get t i) || loop t c ~pos:(i + 1) ~len)
  in
  fun t c -> loop t c ~pos:0 ~len:(length t)
;;

let tr ~target ~replacement s =
  if Char.equal target replacement
  then s
  else if mem s target
  then map s ~f:(fun c -> if Char.equal c target then replacement else c)
  else s
;;

let tr_multi ~target ~replacement =
  if is_empty target
  then stage Fn.id
  else if is_empty replacement
  then invalid_arg "tr_multi replacement is empty string"
  else (
    match Bytes_tr.tr_create_map ~target ~replacement with
    | None -> stage Fn.id
    | Some tr_map ->
      stage (fun s ->
        if exists s ~f:(fun c -> Char.( <> ) c (unsafe_get tr_map (Char.to_int c)))
        then map s ~f:(fun c -> unsafe_get tr_map (Char.to_int c))
        else s))
;;

(* fast version, if we ever need it:
   {[
     let concat_array ~sep ar =
       let ar_len = Array.length ar in
       if ar_len = 0 then ""
       else
         let sep_len = length sep in
         let res_len_ref = ref (sep_len * (ar_len - 1)) in
         for i = 0 to ar_len - 1 do
           res_len_ref := !res_len_ref + length ar.(i)
         done;
         let res = create !res_len_ref in
         let str_0 = ar.(0) in
         let len_0 = length str_0 in
         blit ~src:str_0 ~src_pos:0 ~dst:res ~dst_pos:0 ~len:len_0;
         let pos_ref = ref len_0 in
         for i = 1 to ar_len - 1 do
           let pos = !pos_ref in
           blit ~src:sep ~src_pos:0 ~dst:res ~dst_pos:pos ~len:sep_len;
           let new_pos = pos + sep_len in
           let str_i = ar.(i) in
           let len_i = length str_i in
           blit ~src:str_i ~src_pos:0 ~dst:res ~dst_pos:new_pos ~len:len_i;
           pos_ref := new_pos + len_i
         done;
         res
   ]} *)

let concat_array ?sep ar = concat ?sep (Array.to_list ar)
let concat_map ?sep s ~f = concat_array ?sep (Array.map (to_array s) ~f)

(* [filter t f] is implemented by the following algorithm.

   Let [n = length t].

   1. Find the lowest [i] such that [not (f t.[i])].

   2. If there is no such [i], then return [t].

   3. If there is such an [i], allocate a string, [out], to hold the result.  [out] has
   length [n - 1], which is the maximum possible output size given that there is at least
   one character not satisfying [f].

   4. Copy characters at indices 0 ... [i - 1] from [t] to [out].

   5. Walk through characters at indices [i+1] ... [n-1] of [t], copying those that
   satisfy [f] from [t] to [out].

   6. If we completely filled [out], then return it.  If not, return the prefix of [out]
   that we did fill in.

   This algorithm has the property that it doesn't allocate a new string if there's
   nothing to filter, which is a common case. *)
let filter t ~f =
  let n = length t in
  let i = ref 0 in
  while !i < n && f t.[!i] do
    incr i
  done;
  if !i = n
  then t
  else (
    let out = Bytes.create (n - 1) in
    Bytes.blit_string ~src:t ~src_pos:0 ~dst:out ~dst_pos:0 ~len:!i;
    let out_pos = ref !i in
    incr i;
    while !i < n do
      let c = t.[!i] in
      if f c
      then (
        Bytes.set out !out_pos c;
        incr out_pos);
      incr i
    done;
    let out = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:out in
    if !out_pos = n - 1 then out else sub out ~pos:0 ~len:!out_pos)
;;

let chop_prefix s ~prefix =
  if is_prefix s ~prefix then Some (drop_prefix s (length prefix)) else None
;;

let chop_prefix_if_exists s ~prefix =
  if is_prefix s ~prefix then drop_prefix s (length prefix) else s
;;

let chop_prefix_exn s ~prefix =
  match chop_prefix s ~prefix with
  | Some str -> str
  | None -> invalid_argf "String.chop_prefix_exn %S %S" s prefix ()
;;

let chop_suffix s ~suffix =
  if is_suffix s ~suffix then Some (drop_suffix s (length suffix)) else None
;;

let chop_suffix_if_exists s ~suffix =
  if is_suffix s ~suffix then drop_suffix s (length suffix) else s
;;

let chop_suffix_exn s ~suffix =
  match chop_suffix s ~suffix with
  | Some str -> str
  | None -> invalid_argf "String.chop_suffix_exn %S %S" s suffix ()
;;

(* There used to be a custom implementation that was faster for very short strings
   (peaking at 40% faster for 4-6 char long strings).
   This new function is around 20% faster than the default hash function, but slower
   than the previous custom implementation. However, the new OCaml function is well
   behaved, and this implementation is less likely to diverge from the default OCaml
   implementation does, which is a desirable property. (The only way to avoid the
   divergence is to expose the macro redefined in hash_stubs.c in the hash.h header of
   the OCaml compiler.) *)
module Hash = struct
  external hash : string -> int = "Base_hash_string" [@@noalloc]
end

(* [include Hash] to make the [external] version override the [hash] from
   [Hashable.Make_binable], so that we get a little bit of a speedup by exposing it as
   external in the mli. *)
let _ = hash

include Hash
include Comparable.Validate (T)

(* for interactive top-levels -- modules deriving from String should have String's pretty
   printer. *)
let pp = Caml.Format.pp_print_string
let of_char c = make 1 c

let of_char_list l =
  let t = Bytes.create (List.length l) in
  List.iteri l ~f:(fun i c -> Bytes.set t i c);
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t
;;

module Escaping = struct
  (* If this is changed, make sure to update [escape], which attempts to ensure all the
     invariants checked here.  *)
  let build_and_validate_escapeworthy_map escapeworthy_map escape_char func =
    let escapeworthy_map =
      if List.Assoc.mem escapeworthy_map ~equal:Char.equal escape_char
      then escapeworthy_map
      else (escape_char, escape_char) :: escapeworthy_map
    in
    let arr = Array.create ~len:256 (-1) in
    let vals = Array.create ~len:256 false in
    let rec loop = function
      | [] -> Ok arr
      | (c_from, c_to) :: l ->
        let k, v =
          match func with
          | `Escape -> Char.to_int c_from, c_to
          | `Unescape -> Char.to_int c_to, c_from
        in
        if arr.(k) <> -1 || vals.(Char.to_int v)
        then
          Or_error.error_s
            (Sexp.message
               "escapeworthy_map not one-to-one"
               [ "c_from", sexp_of_char c_from
               ; "c_to", sexp_of_char c_to
               ; ( "escapeworthy_map"
                 , sexp_of_list (sexp_of_pair sexp_of_char sexp_of_char) escapeworthy_map
                 )
               ])
        else (
          arr.(k) <- Char.to_int v;
          vals.(Char.to_int v) <- true;
          loop l)
    in
    loop escapeworthy_map
  ;;

  let escape_gen ~escapeworthy_map ~escape_char =
    match build_and_validate_escapeworthy_map escapeworthy_map escape_char `Escape with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok
        (fun src ->
           (* calculate a list of (index of char to escape * escaped char) first, the order
              is from tail to head *)
           let to_escape_len = ref 0 in
           let to_escape =
             foldi src ~init:[] ~f:(fun i acc c ->
               match escapeworthy.(Char.to_int c) with
               | -1 -> acc
               | n ->
                 (* (index of char to escape * escaped char) *)
                 incr to_escape_len;
                 (i, Char.unsafe_of_int n) :: acc)
           in
           match to_escape with
           | [] -> src
           | _ ->
             (* [to_escape] divide [src] to [List.length to_escape + 1] pieces separated by
                the chars to escape.

                Lets take
                {[
                  escape_gen_exn
                    ~escapeworthy_map:[('a', 'A'); ('b', 'B'); ('c', 'C')]
                    ~escape_char:'_'
                ]}
                for example, and assume the string to escape is

                "000a111b222c333"

                then [to_escape] is [(11, 'C'); (7, 'B'); (3, 'A')].

                Then we create a [dst] of length [length src + 3] to store the
                result, copy piece "333" to [dst] directly, then copy '_' and 'C' to [dst];
                then move on to next; after 3 iterations, copy piece "000" and we are done.

                Finally the result will be

                "000_A111_B222_C333" *)
             let src_len = length src in
             let dst_len = src_len + !to_escape_len in
             let dst = Bytes.create dst_len in
             let rec loop last_idx last_dst_pos = function
               | [] ->
                 (* copy "000" at last *)
                 Bytes.blit_string ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
               | (idx, escaped_char) :: to_escape ->
                 (*[idx] = the char to escape*)
                 (* take first iteration for example *)
                 (* calculate length of "333", minus 1 because we don't copy 'c' *)
                 let len = last_idx - idx - 1 in
                 (* set the dst_pos to copy to *)
                 let dst_pos = last_dst_pos - len in
                 (* copy "333", set [src_pos] to [idx + 1] to skip 'c' *)
                 Bytes.blit_string ~src ~src_pos:(idx + 1) ~dst ~dst_pos ~len;
                 (* backoff [dst_pos] by 2 to copy '_' and 'C' *)
                 let dst_pos = dst_pos - 2 in
                 Bytes.set dst dst_pos escape_char;
                 Bytes.set dst (dst_pos + 1) escaped_char;
                 loop idx dst_pos to_escape
             in
             (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] first *)
             loop src_len dst_len to_escape;
             Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
  ;;

  let escape_gen_exn ~escapeworthy_map ~escape_char =
    Or_error.ok_exn (escape_gen ~escapeworthy_map ~escape_char) |> stage
  ;;

  let escape ~escapeworthy ~escape_char =
    (* For [escape_gen_exn], we don't know how to fix invalid escapeworthy_map so we have
       to raise exception; but in this case, we know how to fix duplicated elements in
       escapeworthy list, so we just fix it instead of raising exception to make this
       function easier to use.  *)
    let escapeworthy_map =
      escapeworthy
      |> List.dedup_and_sort ~compare:Char.compare
      |> List.map ~f:(fun c -> c, c)
    in
    escape_gen_exn ~escapeworthy_map ~escape_char
  ;;

  (* In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

     a : `Literal
     _ : `Escaping
     a : `Escaped
     _ : `Escaping
     _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1] *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal | `Escaped ->
      if Char.equal str.[i] escape_char then `Escaping else `Literal
  ;;

  let unescape_gen ~escapeworthy_map ~escape_char =
    match build_and_validate_escapeworthy_map escapeworthy_map escape_char `Unescape with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok
        (fun src ->
           (* Continue the example in [escape_gen_exn], now we unescape

              "000_A111_B222_C333"

              back to

              "000a111b222c333"

              Then [to_unescape] is [14; 9; 4], which is indexes of '_'s.

              Then we create a string [dst] to store the result, copy "333" to it, then copy
              'c', then move on to next iteration. After 3 iterations copy "000" and we are
              done.  *)
           (* indexes of escape chars *)
           let to_unescape =
             let rec loop i status acc =
               if i >= length src
               then acc
               else (
                 let status = update_escape_status src ~escape_char i status in
                 loop
                   (i + 1)
                   status
                   (match status with
                    | `Escaping -> i :: acc
                    | `Escaped | `Literal -> acc))
             in
             loop 0 `Literal []
           in
           match to_unescape with
           | [] -> src
           | idx :: to_unescape' ->
             let dst = Bytes.create (length src - List.length to_unescape) in
             let rec loop last_idx last_dst_pos = function
               | [] ->
                 (* copy "000" at last *)
                 Bytes.blit_string ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
               | idx :: to_unescape ->
                 (* [idx] = index of escaping char *)
                 (* take 1st iteration as example, calculate the length of "333", minus 2 to
                    skip '_C' *)
                 let len = last_idx - idx - 2 in
                 (* point [dst_pos] to the position to copy "333" to *)
                 let dst_pos = last_dst_pos - len in
                 (* copy "333" *)
                 Bytes.blit_string ~src ~src_pos:(idx + 2) ~dst ~dst_pos ~len;
                 (* backoff [dst_pos] by 1 to copy 'c' *)
                 let dst_pos = dst_pos - 1 in
                 Bytes.set
                   dst
                   dst_pos
                   (match escapeworthy.(Char.to_int src.[idx + 1]) with
                    | -1 -> src.[idx + 1]
                    | n -> Char.unsafe_of_int n);
                 (* update [last_dst_pos] and [last_idx] *)
                 loop idx dst_pos to_unescape
             in
             if idx < length src - 1
             then
               (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] *)
               loop (length src) (Bytes.length dst) to_unescape
             else
               (* for escaped string ending with an escaping char like "000_", just ignore
                  the last escaping char *)
               loop (length src - 1) (Bytes.length dst) to_unescape';
             Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
  ;;

  let unescape_gen_exn ~escapeworthy_map ~escape_char =
    Or_error.ok_exn (unescape_gen ~escapeworthy_map ~escape_char) |> stage
  ;;

  let unescape ~escape_char = unescape_gen_exn ~escapeworthy_map:[] ~escape_char

  let preceding_escape_chars str ~escape_char pos =
    let rec loop p cnt =
      if p < 0 || Char.( <> ) str.[p] escape_char then cnt else loop (p - 1) (cnt + 1)
    in
    loop (pos - 1) 0
  ;;

  (* In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

     a : `Literal
     _ : `Escaping
     a : `Escaped
     _ : `Escaping
     _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1] *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal | `Escaped ->
      if Char.equal str.[i] escape_char then `Escaping else `Literal
  ;;

  let escape_status str ~escape_char pos =
    let odd = preceding_escape_chars str ~escape_char pos mod 2 = 1 in
    match odd, Char.equal str.[pos] escape_char with
    | true, (true | false) -> `Escaped
    | false, true -> `Escaping
    | false, false -> `Literal
  ;;

  let check_bound str pos function_name =
    if pos >= length str || pos < 0
    then invalid_argf "%s: out of bounds" function_name ()
  ;;

  let is_char_escaping str ~escape_char pos =
    check_bound str pos "is_char_escaping";
    match escape_status str ~escape_char pos with
    | `Escaping -> true
    | `Escaped | `Literal -> false
  ;;

  let is_char_escaped str ~escape_char pos =
    check_bound str pos "is_char_escaped";
    match escape_status str ~escape_char pos with
    | `Escaped -> true
    | `Escaping | `Literal -> false
  ;;

  let is_char_literal str ~escape_char pos =
    check_bound str pos "is_char_literal";
    match escape_status str ~escape_char pos with
    | `Literal -> true
    | `Escaped | `Escaping -> false
  ;;

  let index_from str ~escape_char pos char =
    check_bound str pos "index_from";
    let rec loop i status =
      if i >= pos
      && (match status with
          | `Literal -> true
          | `Escaped | `Escaping -> false)
      && Char.equal str.[i] char
      then Some i
      else (
        let i = i + 1 in
        if i >= length str
        then None
        else loop i (update_escape_status str ~escape_char i status))
    in
    loop pos (escape_status str ~escape_char pos)
  ;;

  let index_from_exn str ~escape_char pos char =
    match index_from str ~escape_char pos char with
    | None ->
      raise_s
        (Sexp.message
           "index_from_exn: not found"
           [ "str", sexp_of_t str
           ; "escape_char", sexp_of_char escape_char
           ; "pos", sexp_of_int pos
           ; "char", sexp_of_char char
           ])
    | Some pos -> pos
  ;;

  let index str ~escape_char char = index_from str ~escape_char 0 char
  let index_exn str ~escape_char char = index_from_exn str ~escape_char 0 char

  let rindex_from str ~escape_char pos char =
    check_bound str pos "rindex_from";
    (* if the target char is the same as [escape_char], we have no way to determine which
       escape_char is literal, so just return None *)
    if Char.equal char escape_char
    then None
    else (
      let rec loop pos =
        if pos < 0
        then None
        else (
          let escape_chars = preceding_escape_chars str ~escape_char pos in
          if escape_chars mod 2 = 0 && Char.equal str.[pos] char
          then Some pos
          else loop (pos - escape_chars - 1))
      in
      loop pos)
  ;;

  let rindex_from_exn str ~escape_char pos char =
    match rindex_from str ~escape_char pos char with
    | None ->
      raise_s
        (Sexp.message
           "rindex_from_exn: not found"
           [ "str", sexp_of_t str
           ; "escape_char", sexp_of_char escape_char
           ; "pos", sexp_of_int pos
           ; "char", sexp_of_char char
           ])
    | Some pos -> pos
  ;;

  let rindex str ~escape_char char =
    if is_empty str then None else rindex_from str ~escape_char (length str - 1) char
  ;;

  let rindex_exn str ~escape_char char =
    rindex_from_exn str ~escape_char (length str - 1) char
  ;;

  (* [split_gen str ~escape_char ~on] works similarly to [String.split_gen], with an
     additional requirement: only split on literal chars, not escaping or escaped *)
  let split_gen str ~escape_char ~on =
    let is_delim =
      match on with
      | `char c' -> fun c -> Char.equal c c'
      | `char_list l -> fun c -> char_list_mem l c
    in
    let len = length str in
    let rec loop acc status last_pos pos =
      if pos = len
      then List.rev (sub str ~pos:last_pos ~len:(len - last_pos) :: acc)
      else (
        let status = update_escape_status str ~escape_char pos status in
        if (match status with
          | `Literal -> true
          | `Escaped | `Escaping -> false)
        && is_delim str.[pos]
        then (
          let sub_str = sub str ~pos:last_pos ~len:(pos - last_pos) in
          loop (sub_str :: acc) status (pos + 1) (pos + 1))
        else loop acc status last_pos (pos + 1))
    in
    loop [] `Literal 0 0
  ;;

  let split str ~on = split_gen str ~on:(`char on)
  let split_on_chars str ~on:chars = split_gen str ~on:(`char_list chars)

  let split_at str pos =
    sub str ~pos:0 ~len:pos, sub str ~pos:(pos + 1) ~len:(length str - pos - 1)
  ;;

  let lsplit2 str ~on ~escape_char =
    Option.map (index str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let rsplit2 str ~on ~escape_char =
    Option.map (rindex str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let lsplit2_exn str ~on ~escape_char = split_at str (index_exn str ~escape_char on)
  let rsplit2_exn str ~on ~escape_char = split_at str (rindex_exn str ~escape_char on)

  (* [last_non_drop_literal] and [first_non_drop_literal] are either both [None] or both
     [Some]. If [Some], then the former is >= the latter. *)
  let last_non_drop_literal ~drop ~escape_char t =
    rfindi t ~f:(fun i c ->
      (not (drop c))
      || is_char_escaping t ~escape_char i
      || is_char_escaped t ~escape_char i)
  ;;

  let first_non_drop_literal ~drop ~escape_char t =
    lfindi t ~f:(fun i c ->
      (not (drop c))
      || is_char_escaping t ~escape_char i
      || is_char_escaped t ~escape_char i)
  ;;

  let rstrip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    match last_non_drop_literal t ~drop ~escape_char with
    | None -> ""
    | Some i -> if i = length t - 1 then t else prefix t (i + 1)
  ;;

  let lstrip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    match first_non_drop_literal t ~drop ~escape_char with
    | None -> ""
    | Some 0 -> t
    | Some n -> drop_prefix t n
  ;;

  (* [strip t] could be implemented as [lstrip (rstrip t)].  The implementation
     below saves (at least) a factor of two allocation, by only allocating the
     final result.  This also saves some amount of time. *)
  let strip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    let length = length t in
    (* performance hack: avoid copying [t] in common cases *)
    if length = 0 || not (drop t.[0] || drop t.[length - 1])
    then t
    else (
      match first_non_drop_literal t ~drop ~escape_char with
      | None -> ""
      | Some first ->
        (match last_non_drop_literal t ~drop ~escape_char with
         | None -> assert false
         | Some last -> sub t ~pos:first ~len:(last - first + 1)))
  ;;
end

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! String_replace_polymorphic_compare

let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  if min > max
  then
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
  else Ok (clamp_unchecked t ~min ~max)
;;

(* Override [Search_pattern] with default case-sensitivity argument at the end of the
   file, so that call sites above are forced to supply case-sensitivity explicitly. *)
module Search_pattern = struct
  include Search_pattern0

  let create ?(case_sensitive = true) pattern = create pattern ~case_sensitive
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include String_replace_polymorphic_compare
