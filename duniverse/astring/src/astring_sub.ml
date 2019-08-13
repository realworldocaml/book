(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring_unsafe

let sunsafe_get = string_unsafe_get

(* Errors *)

let strf = Format.asprintf
let err_base = "not on the same base string"
let err_empty_sub pos = strf "empty substring [%d;%d]" pos pos
let err_pos_range start stop len =
  strf "invalid start:%d stop:%d for position range [0;%d]" start stop len

(* From strings *)

let v ?(start = 0) ?stop s =
  let s_len = string_length s in
  let stop = match stop with None -> s_len | Some stop -> stop in
  if start < 0 || stop > s_len || stop < start
  then invalid_arg (err_pos_range start stop s_len)
  else (s, start, stop)

let of_string_with_range ?(first = 0) ?(len = max_int) s =
  if len < 0 then invalid_arg (Astring_base.err_neg_len len) else
  let s_len = string_length s in
  let max_idx = s_len - 1 in
  let empty = function
  | first when first < 0 -> (s, 0, 0)
  | first when first > max_idx -> (s, s_len, s_len)
  | first -> (s, first, first)
  in
  if len = 0 then empty first else
  let last (* index *) = match len with
  | len when len = max_int -> max_idx
  | len ->
      let last = first + len - 1 in
      if last > max_idx then max_idx else last
  in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty first else
  (s, first, last + 1 (* position *))

let of_string_with_index_range ?(first = 0) ? last s =
  let s_len = string_length s in
  let max_idx = s_len - 1 in
  let empty = function
  | first when first < 0 -> (s, 0, 0)
  | first when first > max_idx -> (s, s_len, s_len)
  | first -> (s, first, first)
  in
  let last (* index *) = match last with
  | None -> max_idx
  | Some last -> if last > max_idx then max_idx else last
  in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty first else
  (s, first, last + 1 (* position *))

(* Substrings *)

type t = string * int * int

let empty = (Astring_base.empty, 0, 0)
let start_pos (_, start, _) = start
let stop_pos (_, _, stop) = stop
let base_string (s, _, _) = s
let length (_, start, stop) = stop - start
let get (s, start, _) i = string_safe_get s (start + i)
let get_byte s i = char_to_byte (get s i)
let unsafe_get (s, start, _) i = string_unsafe_get s (start + i)
let unsafe_get_byte s i = char_to_byte (unsafe_get s i)

let head ?(rev = false) (s, start, stop) =
  if start = stop then None else
  Some (string_unsafe_get s (if rev then stop - 1 else start))

let get_head ?(rev = false) (s, start, stop) =
  if start = stop then invalid_arg (err_empty_sub start) else
  string_unsafe_get s (if rev then stop - 1 else start)

let of_string s = v s
let to_string (s, start, stop) =
  if start = stop then Astring_base.empty else
  if start = 0 && stop = string_length s then s else
  unsafe_string_sub s start (stop - start)

let rebase (_, start, stop as sub) = (to_string sub, 0, stop - start)
let hash s = Hashtbl.hash s

(* Stretching substrings *)

let start (s, start, _) = (s, start, start)
let stop (s, _, stop) = (s, stop, stop)
let base (s, _, _) = (s, 0, string_length s)

let tail ?(rev = false) (s, start, stop as sub) =
  if start = stop then sub else
  if rev then (s, start, stop - 1) else (s, start + 1, stop)

let fextend ?max ~sat (s, start, stop) =
  let max_idx = string_length s - 1 in
  let max_idx = match max with
  | None -> max_idx
  | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
  | Some max -> let i = stop + max - 1 in if i > max_idx then max_idx else i
  in
  let rec loop i =
    if i > max_idx then (s, start, i) else
    if sat (string_unsafe_get s i) then loop (i + 1) else
    (s, start, i)
  in
  loop stop

let rextend ?max ~sat (s, start, stop) =
  let min_idx = match max with
  | None -> 0
  | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
  | Some max -> let i = start - max in if i < 0 then 0 else i
  in
  let rec loop i =
    if i < min_idx then (s, min_idx, stop) else
    if sat (string_unsafe_get s i) then loop (i - 1) else
    (s, i + 1, stop)
  in
  loop (start - 1)

let extend ?(rev = false) ?max ?(sat = (fun _ -> true)) sub = match rev with
| true  -> rextend ?max ~sat sub
| false -> fextend ?max ~sat sub

let freduce ?max ~sat (s, start, stop as sub) =
  if start = stop then sub else
  let min_idx = match max with
  | None -> start
  | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
  | Some max -> let i = stop - max in if i < start then start else i
  in
  let rec loop i =
    if i < min_idx then (s, start, min_idx) else
    if sat (string_unsafe_get s i) then loop (i - 1) else
    (s, start, i + 1)
  in
  loop (stop - 1)

let rreduce ?max ~sat (s, start, stop as sub) =
  if start = stop then sub else
  let max_idx = stop - 1 in
  let max_idx = match max with
  | None -> max_idx
  | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
  | Some max -> let i = start + max - 1 in if i > max_idx then max_idx else i
  in
  let rec loop i =
    if i > max_idx then (s, i, stop) else
    if sat (string_unsafe_get s i) then loop (i + 1) else
    (s, i, stop)
  in
  loop start

let reduce ?(rev = false) ?max ?(sat = (fun _ -> true)) sub = match rev with
| true  -> rreduce ?max ~sat sub
| false -> freduce ?max ~sat sub

let extent (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  let start = if start0 < start1 then start0 else start1 in
  let stop = if stop0 < stop1 then stop1 else stop0 in
  (s0, start, stop)

let overlap (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  if not (start0 <= stop1 && start1 <= stop0) then None else
  let start = if start0 < start1 then start1 else start0 in
  let stop = if stop0 < stop1 then stop0 else stop1 in
  Some (s0, start, stop)

(* Appending substrings *)

let append (s0, start0, _ as sub0) (s1, start1, _ as sub1) =
  let l0 = length sub0 in
  if l0 = 0 then rebase sub1 else
  let l1 = length sub1 in
  if l1 = 0 then rebase sub0 else
  let len = l0 + l1 in
  let b = Bytes.create len in
  bytes_unsafe_blit_string s0 start0 b 0 l0;
  bytes_unsafe_blit_string s1 start1 b l0 l1;
  (bytes_unsafe_to_string b, 0, len)

let concat ?sep:(sep, sep_start, _ as sep_sub = empty) = function
| [] -> empty
| [s] -> rebase s
| (s, start, _ as sub) :: ss ->
    let sub_len = length sub in
    let sep_len = length sep_sub in
    let rec cat_len sep_count l ss =
      if l < 0 then l else
      match ss with
      | s :: ss -> cat_len (sep_count + 1) (l + length s) ss
      | [] ->
          if sep_len = 0 then l else
          let max_sep_count = Sys.max_string_length / sep_len in
          if sep_count < 0 || sep_count > max_sep_count then -1 else
          sep_count * sep_len + l
    in
    let cat_len = cat_len 0 sub_len ss in
    if cat_len < 0 then invalid_arg Astring_base.err_max_string_len else
    let b = Bytes.create cat_len in
    bytes_unsafe_blit_string s start b 0 sub_len;
    let rec loop i = function
    | [] -> bytes_unsafe_to_string b
    | (str, str_start, _ as str_sub) :: ss ->
        let sep_pos = i in
        let str_pos = i + sep_len in
        let str_len = length str_sub in
        bytes_unsafe_blit_string sep sep_start b sep_pos sep_len;
        bytes_unsafe_blit_string str str_start b str_pos str_len;
        loop (str_pos + str_len) ss
    in
    (loop sub_len ss, 0, cat_len)

(* Predicates *)

let is_empty (_, start, stop) = stop - start = 0

let is_prefix ~affix:(affix, astart, _ as affix_sub) (s, sstart, _ as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get affix (astart + i) <> sunsafe_get s (sstart + i)
    then false
    else loop (i + 1)
  in
  loop 0

let is_infix ~affix:(affix, astart, _ as affix_sub) (s, sstart, _ as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx_a (* zero based idx *) = len_a - 1 in
  let max_zidx_s (* zero based idx *) = len_s - len_a  in
  let rec loop i k =
    if i > max_zidx_s then false else
    if k > max_zidx_a then true else
    if k > 0 then
      if sunsafe_get affix (astart + k) = sunsafe_get s (sstart + i + k)
      then loop i (k + 1)
      else loop (i + 1) 0
    else if sunsafe_get affix astart = sunsafe_get s (sstart + i)
    then loop i 1
    else loop (i + 1) 0
  in
  loop 0 0

let is_suffix ~affix:(affix, _, astop as affix_sub) (s, _, sstop as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let max_idx_a = astop - 1 in
  let max_idx_s = sstop - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get affix (max_idx_a - i) <> sunsafe_get s (max_idx_s - i)
    then false
    else loop (i + 1)
  in
  loop 0

let for_all sat (s, start, stop) =
  Astring_base.for_all sat s ~first:start ~last:(stop - 1)

let exists sat (s, start, stop) =
  Astring_base.exists sat s ~first:start ~last:(stop - 1)

let same_base (s0, _, _) (s1, _, _) = s0 == s1

let equal_bytes (s0, start0, stop0) (s1, start1, stop1) =
  if s0 == s1 && start0 = start1 && stop0 = stop1 then true else
  let len0 = stop0 - start0 in
  let len1 = stop1 - start1 in
  if len0 <> len1 then false else
  let max_zidx = len0 - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get s0 (start0 + i) <> sunsafe_get s1 (start1 + i)
    then false
    else loop (i + 1)
  in
  loop 0

let compare_bytes (s0, start0, stop0) (s1, start1, stop1) =
  if s0 == s1 && start0 = start1 && stop0 = stop1 then 0 else
  let len0 = stop0 - start0 in
  let len1 = stop1 - start1 in
  let min_len = if len0 < len1 then len0 else len1 in
  let max_i = min_len - 1 in
  let rec loop i =
    if i > max_i then Pervasives.compare len0 len1 else
    let c0 = sunsafe_get s0 (start0 + i) in
    let c1 = sunsafe_get s1 (start1 + i) in
    let cmp = Pervasives.compare c0 c1 in
    if cmp <> 0 then cmp else
    loop (i + 1)
  in
  loop 0

let eq_pos : int -> int -> bool = fun p0 p1 -> p0 = p1
let equal (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  eq_pos start0 start1 && eq_pos stop0 stop1

let compare_pos : int -> int -> int = Pervasives.compare
let compare (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  let c = compare_pos start0 start1 in
  if c <> 0 then c else
  compare_pos stop0 stop1

(* Extracting substrings *)

let with_range ?(first = 0) ?(len = max_int) (s, start, stop) =
  if len < 0 then invalid_arg (Astring_base.err_neg_len len) else
  let s_len = stop - start in
  let max_idx = s_len - 1 in
  let empty = function
  | first when first < 0 -> (s, start, start)
  | first when first > max_idx -> (s, stop, stop)
  | first -> (s, start + first, start + first)
  in
  if len = 0 then empty first else
  let last (* index *) = match len with
  | len when len = max_int -> max_idx
  | len ->
      let last = first + len - 1 in
      if last > max_idx then max_idx else last
  in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty first else
  (s, start + first, start + last + 1 (* position *))

let with_index_range ?(first = 0) ? last (s, start, stop) =
  let s_len = stop - start in
  let max_idx = s_len - 1 in
  let empty = function
  | first when first < 0 -> (s, start, start)
  | first when first > max_idx -> (s, stop, stop)
  | first -> (s, start + first, start + first)
  in
  let last (* index *) = match last with
  | None -> max_idx
  | Some last -> if last > max_idx then max_idx else last
  in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty first else
  (s, start + first, start + last + 1 (* position *))

let trim ?(drop = Astring_char.Ascii.is_white) (s, start, stop as sub) =
  let len = stop - start in
  if len = 0 then sub else
  let max_pos = stop in
  let max_idx = stop - 1 in
  let rec left_pos i =
    if i > max_idx then max_pos else
    if drop (sunsafe_get s i) then left_pos (i + 1) else i
  in
  let rec right_pos i =
    if i < start then start else
    if drop (sunsafe_get s i) then right_pos (i - 1) else (i + 1)
  in
  let left = left_pos start in
  if left = max_pos then (s, (start + stop) / 2, (start + stop) / 2)  else
  let right = right_pos max_idx in
  if left = start && right = max_pos then sub else
  (s, left, right)

let fspan ~min ~max ~sat (s, start, stop as sub) =
  if min < 0 then invalid_arg (Astring_base.err_neg_min min) else
  if max < 0 then invalid_arg (Astring_base.err_neg_max max) else
  if min > max || max = 0 then ((s, start, start), sub) else
  let max_idx = stop - 1 in
  let max_idx =
    let k = start + max - 1 in (if k > max_idx || k < 0 then max_idx else k)
  in
  let need_idx = start + min in
  let rec loop i =
    if i <= max_idx && sat (sunsafe_get s i) then loop (i + 1) else
    if i < need_idx || i = 0 then ((s, start, start), sub) else
    if i = stop then (sub, (s, stop, stop)) else
    (s, start, i), (s, i, stop)
  in
  loop start

let rspan ~min ~max ~sat (s, start, stop as sub) =
  if min < 0 then invalid_arg (Astring_base.err_neg_min min) else
  if max < 0 then invalid_arg (Astring_base.err_neg_max max) else
  if min > max || max = 0 then (sub, (s, stop, stop)) else
  let max_idx = stop - 1 in
  let min_idx = let k = stop - max in if k < start then start else k in
  let need_idx = stop - min - 1 in
  let rec loop i =
    if i >= min_idx && sat (sunsafe_get s i) then loop (i - 1) else
    if i > need_idx || i = max_idx then (sub, (s, stop, stop)) else
    if i = start - 1 then ((s, start, start), sub) else
    (s, start, i + 1), (s, i + 1, stop)
  in
  loop max_idx

let span ?(rev = false) ?(min = 0) ?(max = max_int) ?(sat = fun _ -> true) sub =
  match rev with
  | true  -> rspan ~min ~max ~sat sub
  | false -> fspan ~min ~max ~sat sub

let take ?(rev = false) ?min ?max ?sat s =
  (if rev then snd else fst) @@ span ~rev ?min ?max ?sat s

let drop ?(rev = false) ?min ?max ?sat s =
  (if rev then fst else snd) @@ span ~rev ?min ?max ?sat s

let fcut ~sep:(sep, sep_start, sep_stop) (s, start, stop) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ((s, start, i), (s, i + sep_len, stop))
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep i (k + 1)
    else scan (i + 1)
  and scan i =
    if i > max_s_idx then None else
    if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep i 1
    else scan (i + 1)
  in
  scan start

let rcut ~sep:(sep, sep_start, sep_stop) (s, start, stop) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ((s, start, i), (s, i + sep_len, stop))
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep i (k + 1)
    else rscan (i - 1)
  and rscan i =
    if i < start then None else
    if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep i 1
    else rscan (i - 1)
  in
  rscan (max_s_idx - max_sep_zidx)

let cut ?(rev = false) ~sep s = match rev with
| true  -> rcut ~sep s
| false -> fcut ~sep s

let add_sub ~no_empty s ~start ~stop acc =
  if start = stop then (if no_empty then acc else (s, start, start) :: acc) else
  (s, start, stop) :: acc

let fcuts ~no_empty ~sep:(sep, sep_start, sep_stop) (s, start, stop as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep sstart i k acc =
    if k > max_sep_zidx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub ~no_empty s ~start:sstart ~stop:i acc)
    else
      if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
      then check_sep sstart i (k + 1) acc
      else scan sstart (i + 1) acc
  and scan sstart i acc =
    if i > max_s_idx then
      if sstart = start then (if no_empty && s_len = 0 then [] else [sub]) else
      List.rev (add_sub ~no_empty s ~start:sstart ~stop acc)
    else
      if sunsafe_get s i = sunsafe_get sep sep_start
      then check_sep sstart i 1 acc
      else scan sstart (i + 1) acc
  in
  scan start start []

let rcuts ~no_empty ~sep:(sep, sep_start, sep_stop) (s, start, stop as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep sstop i k acc =
    if k > max_sep_zidx then
      let start = i + sep_len in
      rscan i (i - sep_len) (add_sub ~no_empty s ~start ~stop:sstop acc)
    else
      if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
      then check_sep sstop i (k + 1) acc
      else rscan sstop (i - 1) acc
  and rscan sstop i acc =
    if i < start then
      if sstop = stop then (if no_empty && s_len = 0 then [] else [sub]) else
      add_sub ~no_empty s ~start ~stop:sstop acc
    else
      if sunsafe_get s i = sunsafe_get sep sep_start
      then check_sep sstop i 1 acc
      else rscan sstop (i - 1) acc
  in
  rscan stop (max_s_idx - max_sep_zidx) []

let cuts ?(rev = false) ?(empty = true) ~sep s = match rev with
| true  -> rcuts ~no_empty:(not empty) ~sep s
| false -> fcuts ~no_empty:(not empty) ~sep s

let fields
    ?(empty = false) ?(is_sep = Astring_char.Ascii.is_white)
    (s, start, stop as sub)
  =
  let no_empty = not empty in
  let max_pos = stop in
  let rec loop i end_pos acc =
    if i < start then begin
      if end_pos = max_pos
      then (if no_empty && max_pos = start then [] else [sub])
      else add_sub ~no_empty s ~start ~stop:end_pos acc
    end else begin
      if not (is_sep (sunsafe_get s i)) then loop (i - 1) end_pos acc else
      loop (i - 1) i (add_sub ~no_empty s ~start:(i + 1) ~stop:end_pos acc)
    end
  in
  loop (max_pos - 1) max_pos []

(* Traversing *)

let ffind sat (s, start, stop) =
  let max_idx = stop - 1 in
  let rec loop i =
    if i > max_idx then None else
    if sat (sunsafe_get s i) then Some (s, i, i + 1) else loop (i + 1)
  in
  loop start

let rfind sat (s, start, stop) =
  let rec loop i =
    if i < start then None else
    if sat (sunsafe_get s i) then Some (s, i, i + 1) else loop (i - 1)
  in
  loop (stop - 1)

let find ?(rev = false) sat sub = match rev with
| true  -> rfind sat sub
| false -> ffind sat sub

let ffind_sub ~sub:(sub, sub_start, sub_stop) (s, start, stop) =
  let len_sub = sub_stop - sub_start in
  let len_s = stop - start in
  if len_sub > len_s then None else
  let max_zidx_sub = len_sub - 1 in
  let max_idx_s = start + len_s - len_sub in
  let rec loop i k =
    if i > max_idx_s then None else
    if k > max_zidx_sub then Some (s, i, i + len_sub) else
    if k > 0 then
      if sunsafe_get sub (sub_start + k) = sunsafe_get s (i + k)
      then loop i (k + 1)
      else loop (i + 1) 0
    else if sunsafe_get sub sub_start = sunsafe_get s i then loop i 1 else
    loop (i + 1) 0
  in
  loop start 0

let rfind_sub ~sub:(sub, sub_start, sub_stop) (s, start, stop) =
  let len_sub = sub_stop - sub_start in
  let len_s = stop - start in
  if len_sub > len_s then None else
  let max_zidx_sub = len_sub - 1 in
  let rec loop i k =
    if i < start then None else
    if k > max_zidx_sub then Some (s, i, i + len_sub) else
    if k > 0 then
      if sunsafe_get sub (sub_start + k) = sunsafe_get s (i + k)
      then loop i (k + 1)
      else loop (i - 1) 0
    else if sunsafe_get sub sub_start = sunsafe_get s i then loop i 1 else
    loop (i - 1) 0
  in
  loop (stop - len_sub) 0

let find_sub ?(rev = false) ~sub start = match rev with
| true  -> rfind_sub ~sub start
| false -> ffind_sub ~sub start

let filter sat (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  let max_idx = stop - 1 in
  let rec loop b k i = (* k is the write index in b *)
    if i > max_idx then
      ((if k = len then bytes_unsafe_to_string b else Bytes.sub_string b 0 k),
       0, k)
    else
    let c = sunsafe_get s i in
    if sat c then (bytes_unsafe_set b k c; loop b (k + 1) (i + 1)) else
    loop b k (i + 1)
  in
  loop b 0 start

let filter_map f (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  let max_idx = stop - 1 in
  let rec loop b k i = (* k is the write index in b *)
    if i > max_idx then
      ((if k = len then bytes_unsafe_to_string b else Bytes.sub_string b 0 k),
       0, k)
    else
    match f (sunsafe_get s i) with
    | None -> loop b k (i + 1)
    | Some c -> bytes_unsafe_set b k c; loop b (k + 1) (i + 1)
  in
  loop b 0 start

let map f (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    bytes_unsafe_set b i (f (sunsafe_get s (start + i)))
  done;
  (bytes_unsafe_to_string b, 0, len)

let mapi f (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    bytes_unsafe_set b i (f i (sunsafe_get s (start + i)))
  done;
  (bytes_unsafe_to_string b, 0, len)

let fold_left f acc (s, start, stop) =
  Astring_base.fold_left f acc s ~first:start ~last:(stop - 1)

let fold_right f (s, start, stop) acc =
  Astring_base.fold_right f s acc ~first:start ~last:(stop - 1)

let iter f (s, start, stop) =
  for i = start to stop - 1 do f (sunsafe_get s i) done

let iteri f (s, start, stop)  =
  for i = start to stop - 1 do f (i - start) (sunsafe_get s i) done

(* Pretty printing *)

let pp ppf s =
  Format.pp_print_string ppf (to_string s)

let dump ppf s =
  Format.pp_print_char ppf '"';
  Format.pp_print_string ppf (Astring_escape.escape_string (to_string s));
  Format.pp_print_char ppf '"';
  ()

let dump_raw ppf (s, start, stop) =
  Format.fprintf ppf "@[<1>(@[<1>(base@ \"%s\")@]@ @[<1>(start@ %d)@]@ \
                         @[(stop@ %d)@])@]"
    (Astring_escape.escape_string s) start stop

(* OCaml base type conversions *)

let of_char c = v (Astring_base.of_char c)
let to_char s = Astring_base.to_char (to_string s)
let of_bool b = v (Astring_base.of_bool b)
let to_bool s = Astring_base.to_bool (to_string s)
let of_int i = v (Astring_base.of_int i)
let to_int s = Astring_base.to_int (to_string s)
let of_nativeint i = v (Astring_base.of_nativeint i)
let to_nativeint s = Astring_base.to_nativeint (to_string s)
let of_int32 i = v (Astring_base.of_int32 i)
let to_int32 s = Astring_base.to_int32 (to_string s)
let of_int64 i = v (Astring_base.of_int64 i)
let to_int64 s = Astring_base.to_int64 (to_string s)
let of_float f = v (Astring_base.of_float f)
let to_float s = Astring_base.to_float (to_string s)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
