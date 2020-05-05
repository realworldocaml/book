(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring

(* Unsafe string and byte manipulations. If you don't believe the
   author's invariants, replacing with safe versions makes everything
   safe in the library. He won't be upset. *)

let bytes_unsafe_set = Bytes.unsafe_set
let string_unsafe_get = String.unsafe_get

(* Errors *)

let err_invalid_seg s = strf "%a: invalid segment" String.dump s
let err_invalid_ext s = strf "%a: invalid extension" String.dump s

(* A few useful constants *)

let windows = Sys.os_type = "Win32"
let dir_sep_char = if windows then '\\' else '/'
let dir_sep = String.of_char dir_sep_char
let dir_sep_sub = String.sub dir_sep
let not_dir_sep c = c <> dir_sep_char

let dot = "."
let dot_dir = dot ^ dir_sep
let dot_dir_sub = String.sub dot_dir
let dotdot = ".."
let dotdot_dir = dotdot ^ dir_sep
let dotdot_dir_sub = String.sub dotdot_dir

(* Platform specific preliminaties *)

module Windows = struct

  let is_unc_path p = String.is_prefix "\\\\" p
  let has_drive p = String.exists (Char.equal ':') p
  let non_unc_path_start p = match String.find (Char.equal ':') p with
  | None -> 0
  | Some i -> i + 1 (* exists by construction *)

  let parse_unc s =
    (* parses an UNC path, the \\ prefix was already parsed, adds a root path
       if there's only a volume, UNC paths are always absolute. *)
    let p = String.sub ~start:2 s in
    let not_bslash c = c <> '\\' in
    let parse_seg p = String.Sub.span ~min:1 ~sat:not_bslash p in
    let ensure_root r = Some (if String.Sub.is_empty r then (s ^ "\\") else s)
    in
    match parse_seg p with
    | (seg1, _) when String.Sub.is_empty seg1 -> None (* \\ or \\\ *)
    | (seg1, rest) ->
        let seg1_len = String.Sub.length seg1 in
        match String.Sub.get_head ~rev:true seg1 with
        | '.' when seg1_len = 1 -> (* \\.\device\ *)
            begin match parse_seg (String.Sub.tail rest) with
            | (seg, _) when String.Sub.is_empty seg -> None
            | (_, rest) -> ensure_root rest
            end
        | '?' when seg1_len = 1 ->
            begin match parse_seg (String.Sub.tail rest) with
            | (seg2, _) when String.Sub.is_empty seg2 -> None
            | (seg2, rest) ->
                if (String.Sub.get_head ~rev:true seg2 = ':') (* \\?\drive:\ *)
                then (ensure_root rest) else
                if not (String.Sub.equal_bytes seg2 (String.sub "UNC"))
                then begin (* \\?\server\share\ *)
                  match parse_seg (String.Sub.tail rest) with
                  | (seg, _) when String.Sub.is_empty seg -> None
                  | (_, rest) -> ensure_root rest
                end else begin (* \\?\UNC\server\share\ *)
                  match parse_seg (String.Sub.tail rest) with
                  | (seg, _) when String.Sub.is_empty seg -> None
                  | (_, rest) ->
                      match parse_seg (String.Sub.tail rest) with
                      | (seg, _) when String.Sub.is_empty seg -> None
                      | (_, rest) -> ensure_root rest
                end
            end
        | _ -> (* \\server\share\ *)
            begin match parse_seg (String.Sub.tail rest) with
            | (seg, _) when String.Sub.is_empty seg -> None
            | (_, rest) -> ensure_root rest
            end

  let sub_split_volume p =
    (* splits a windows path into its volume (or drive) and actual file
       path. When called the path in [p] is guaranteed to be non empty
       and if [p] is an UNC path it is guaranteed to the be parseable by
       parse_unc_windows. *)
    let split_before i = String.sub p ~stop:i, String.sub p ~start:i in
    if not (is_unc_path p) then
      begin match String.find (Char.equal ':') p with
      | None -> String.Sub.empty, String.sub p
      | Some i -> split_before (i + 1)
      end
    else
    let bslash ~start = match String.find ~start (Char.equal '\\') p with
    | None -> assert false | Some i -> i
    in
    let i = bslash ~start:2 in
    let j = bslash ~start:(i + 1) in
    match p.[i-1] with
    | '.' when i = 3 -> split_before j
    | '?' when i = 3 ->
        if p.[j-1] = ':' then split_before j else
        if (String.Sub.equal_bytes
              (String.sub p ~start:(i + 1) ~stop:j)
              (String.sub "UNC"))
        then split_before (bslash ~start:((bslash ~start:(j + 1)) + 1))
      else split_before (bslash ~start:(j + 1))
    | _ -> split_before j

  let is_root p =
    let _, path = sub_split_volume p in
    String.Sub.length path = 1 && String.Sub.get path 0 = dir_sep_char
end

module Posix = struct
  let has_volume p = String.is_prefix "//" p
  let is_root p = String.equal p dir_sep || String.equal p "//"
end

(* Segments *)

let is_seg_windows s =
  let valid c = c <> '\x00' && c <> dir_sep_char && c <> '/' in
  String.for_all valid s

let is_seg_posix s =
  let valid c = c <> '\x00' && c <> dir_sep_char in
  String.for_all valid s

let is_seg = if windows then is_seg_windows else is_seg_posix

let _split_last_seg p = String.Sub.span ~rev:true ~sat:not_dir_sep p
let _sub_last_seg p = String.Sub.take ~rev:true ~sat:not_dir_sep p
let _sub_last_non_empty_seg p = (* returns empty on roots though *)
  let dir, last = _split_last_seg p in
  match String.Sub.is_empty last with
  | false -> last
  | true -> _sub_last_seg (String.Sub.tail ~rev:true dir)

let _split_last_non_empty_seg p =
  let (dir, last_seg as r) = _split_last_seg p in
  match String.Sub.is_empty last_seg with
  | false -> r, true
  | true -> _split_last_seg (String.Sub.tail ~rev:true dir), false

let sub_last_seg_windows p = _sub_last_seg (snd (Windows.sub_split_volume p))
let sub_last_seg_posix p = _sub_last_seg (String.sub p)
let sub_last_seg = if windows then sub_last_seg_windows else sub_last_seg_posix

let sub_last_non_empty_seg_windows p =
  _sub_last_non_empty_seg (snd (Windows.sub_split_volume p))

let sub_last_non_empty_seg_posix p =
  _sub_last_non_empty_seg (String.sub p)

let sub_last_non_empty_seg =
  if windows then sub_last_non_empty_seg_windows else
  sub_last_non_empty_seg_posix

let is_rel_seg = function "." | ".." -> true | _ -> false

let sub_is_rel_seg seg = match String.Sub.length seg with
| 1 when String.Sub.get seg 0 = '.' -> true
| 2 when String.Sub.get seg 0 = '.' && String.Sub.get seg 1 = '.' -> true
| _ -> false

let sub_is_dir_seg seg = match String.Sub.length seg with
| 0 -> true
| 1 when String.Sub.get seg 0 = '.' -> true
| 2 when String.Sub.get seg 0 = '.' && String.Sub.get seg 1 = '.' -> true
| _ -> false

let segs_of_path p = String.cuts ~sep:dir_sep p
let segs_to_path segs = String.concat ~sep:dir_sep segs

(* File paths *)

type t = string (* N.B. a path is never "" or something is wrooong. *)

let err s = Result.Error (`Msg (strf "%a: invalid path" String.dump s))

let validate_and_collapse_seps p =
  (* collapse non-initial sequences of [dir_sep] to a single one and checks
     no null byte *)
  let max_idx = String.length p - 1 in
  let rec with_buf b last_sep k i = (* k is the write index in b *)
    if i > max_idx then Result.Ok (Bytes.sub_string b 0 k) else
    let c = string_unsafe_get p i in
    if c = '\x00' then err p else
    if c <> dir_sep_char
    then (bytes_unsafe_set b k c; with_buf b false (k + 1) (i + 1)) else
    if not last_sep
    then (bytes_unsafe_set b k c; with_buf b true (k + 1) (i + 1)) else
    with_buf b true k (i + 1)
  in
  let rec try_no_alloc last_sep i =
    if i > max_idx then Result.Ok p else
    let c = string_unsafe_get p i in
    if c = '\x00' then err p else
    if c <> dir_sep_char then try_no_alloc false (i + 1) else
    if not last_sep then try_no_alloc true (i + 1) else
    let b = Bytes.of_string p in (* copy and overwrite starting from i *)
    with_buf b true i (i + 1)
  in
  let start = (* Allow initial double sep for POSIX and UNC paths *)
    if max_idx > 0 then (if p.[0] = dir_sep_char then 1 else 0) else 0
  in
  try_no_alloc false start

let of_string_windows s =
  if s = "" then err s else
  let p = String.map (fun c -> if c = '/' then '\\' else c) s in
  match validate_and_collapse_seps p with
  | Result.Error _ as e -> e
  | Result.Ok p as some ->
      if Windows.is_unc_path p then
        (match Windows.parse_unc p with None -> err s | Some p -> Result.Ok p)
      else
      match String.find (Char.equal ':') p with
      | None -> some
      | Some i when i = String.length p - 1 -> err p (* path is empty *)
      | Some _ -> Result.Ok p

let of_string_posix p = if p = "" then err p else validate_and_collapse_seps p
let of_string = if windows then of_string_windows else of_string_posix

let v s = match of_string s with
| Result.Ok p -> p
| Result.Error (`Msg m) -> invalid_arg m


let add_seg p seg =
  if not (is_seg seg) then invalid_arg (err_invalid_seg seg);
  let sep = if p.[String.length p - 1] = dir_sep_char then "" else dir_sep in
  String.concat ~sep [p; seg]

let append_posix p0 p1 =
  if p1.[0] = dir_sep_char (* absolute *) then p1 else
  let sep = if p0.[String.length p0 - 1] = dir_sep_char then "" else dir_sep in
  String.concat ~sep [p0; p1]

let append_windows p0 p1 =
  if Windows.is_unc_path p1 || Windows.has_drive p1 then p1 else
  if p1.[0] = dir_sep_char then (* absolute *) p1 else
  let sep = if p0.[String.length p0 - 1] = dir_sep_char then "" else dir_sep in
  String.concat ~sep [p0; p1]

let append = if windows then append_windows else append_posix

let ( / ) = add_seg
let ( // ) = append

let split_volume_windows p =
  let vol, path = Windows.sub_split_volume p in
  String.Sub.to_string vol, String.Sub.to_string path

let split_volume_posix p =
  if Posix.has_volume p then dir_sep, String.with_range ~first:1 p else "", p

let split_volume = if windows then split_volume_windows else split_volume_posix

let segs_windows p =
  let _, path = Windows.sub_split_volume p in
  segs_of_path (String.Sub.to_string path)

let segs_posix p =
  let segs = segs_of_path p in
  if Posix.has_volume p then List.tl segs else segs

let segs = if windows then segs_windows else segs_posix

(* File and directory paths *)

let is_dir_path p = sub_is_dir_seg (sub_last_seg p)
let is_file_path p = not (is_dir_path p)
let to_dir_path p = add_seg p ""

let filename p = match String.Sub.to_string (sub_last_seg p) with
| "" | "." | ".." -> ""
| filename -> filename

(* Base and parent paths *)

let sub_is_root p = String.Sub.length p = 1 && String.Sub.get p 0 = dir_sep_char

let _split_base p =
  let dir, last_seg = _split_last_seg p in
  match String.Sub.is_empty dir with
  | true -> (* single seg *) dot_dir_sub, String.Sub.to_string p
  | false ->
      match String.Sub.is_empty last_seg with
      | false -> dir, String.Sub.to_string last_seg
      | true ->
          let dir_file = String.Sub.tail ~rev:true dir in
          let dir, dir_last_seg = _split_last_seg dir_file in
          match String.Sub.is_empty dir with
          | true -> dot_dir_sub, String.Sub.to_string p
          | false -> dir, String.Sub.(to_string (extend dir_last_seg))

let split_base_windows p =
  let vol, path = Windows.sub_split_volume p in
  if sub_is_root path then p, dot_dir else
  let dir, b = _split_base path in
  String.Sub.(base_string (append vol dir)), b

let split_base_posix p =
  if Posix.is_root p then p, dot_dir else
  let dir, b = _split_base (String.sub p) in
  String.Sub.to_string dir, b

let split_base = if windows then split_base_windows else split_base_posix

let base p = snd (split_base p)

let _basename p = match String.Sub.to_string (_sub_last_non_empty_seg p) with
| "." | ".." -> ""
| basename -> basename

let basename_windows p =
  let vol, path = Windows.sub_split_volume p in
  if sub_is_root path then "" else _basename path

let basename_posix p = if Posix.is_root p then "" else _basename (String.sub p)
let basename p = if windows then basename_windows p else basename_posix p

let _parent p =
  (* The parent algorithm is not very smart. It tries to preserve the
     original path and avoids dealing with normalization. We simply
     only keep everything before the last non-empty, non-relative,
     path segment and if the resulting path is empty we return
     "./". Otherwise if the last non-empty segment is "." or ".." we
     simply postfix with "../" *)
  let (dir, seg), is_last = _split_last_non_empty_seg p in
  let dsep = if is_last then dir_sep_sub else String.Sub.empty in
  if sub_is_rel_seg seg then [p; dsep; dotdot_dir_sub] else
  if String.Sub.is_empty dir then [dot_dir_sub] else [dir]

let parent_windows p =
  let vol, path = Windows.sub_split_volume p in
  if sub_is_root path then p else
  String.Sub.(base_string @@ concat (vol :: _parent path))

let parent_posix p =
  if Posix.is_root p then p else
  String.Sub.(base_string @@ concat (_parent (String.sub p)))

let parent = if windows then parent_windows else parent_posix

(* Normalization *)

let rem_empty_seg_windows p =
  let vol, path = Windows.sub_split_volume p in
  if sub_is_root path then p else
  let max = String.Sub.stop_pos path - 1 in
  if String.get p max <> dir_sep_char then p else
  String.with_index_range p ~last:(max - 1)

let rem_empty_seg_posix p = match String.length p with
| 1 -> p
| 2 ->
    if p.[0] <> dir_sep_char && p.[1] = dir_sep_char
    then String.of_char p.[0]
    else p
| len ->
    let max = len - 1 in
    if p.[max] <> dir_sep_char then p else
    String.with_index_range p ~last:(max - 1)

let rem_empty_seg =
  if windows then rem_empty_seg_windows else rem_empty_seg_posix

let normalize_rel_segs segs = (* result is non empty but may be [""] *)
  let rec loop acc = function
  | "." :: [] -> ("" :: acc) (* final "." remove but preserve directoryness. *)
  | "." :: rest -> loop acc rest
  | ".." :: rest ->
      begin match acc with
      | ".." :: _ | [] -> loop (".." :: acc) rest
      | seg :: acc -> (* N.B. seg can't be "." *)
          match rest with
          | [] -> ("" :: acc) (* preserve directoryness *)
          | rest -> loop acc rest
      end
  | seg :: rest -> loop (seg :: acc) rest
  | [] ->
      match acc with
      | ".." :: _ -> ("" :: acc) (* normalize final .. to ../ *)
      |  [] -> [""]
      | acc -> acc
  in
  List.rev (loop [] segs)

let normalize_segs = function
| "" :: segs -> (* absolute path *)
    let rec rem_dotdots = function ".." :: ss -> rem_dotdots ss | ss -> ss in
    "" :: (rem_dotdots @@ normalize_rel_segs segs)
| segs ->
    match normalize_rel_segs segs with
    | [""] -> ["."; ""]
    | segs -> segs

let normalize_windows p =
  let vol, path = Windows.sub_split_volume p in
  let path = String.Sub.to_string path in
  let path = segs_to_path @@ normalize_segs (segs_of_path path) in
  String.Sub.(to_string (concat [vol; String.sub path]))

let normalize_posix p =
  let has_volume = Posix.has_volume p in
  let segs = segs_of_path p in
  let segs = normalize_segs @@ if has_volume then List.tl segs else segs in
  let segs = if has_volume then "" :: segs else segs in
  segs_to_path segs

let normalize = if windows then normalize_windows else normalize_posix

(* Prefixes *)

let is_prefix prefix p =
  if not (String.is_prefix prefix p) then false else
  (* Further check the prefix is segment-based. If [prefix] ends with a
     dir_sep_char nothing more needs to be checked. If it doesn't we need
     to check that [p]'s remaining suffix is either empty or
     starts with a directory separator. *)
  let suff_start = String.length prefix in
  if prefix.[suff_start - 1] = dir_sep_char then true else
  if suff_start = String.length p then (* suffix empty *) true else
  p.[suff_start] = dir_sep_char

let _prefix_last_index p0 p1 = (* last char index of segment-based prefix *)
  let l0 = String.length p0 in
  let l1 = String.length p1 in
  let p0, p1, max = if l0 < l1 then p0, p1, l0 - 1 else p1, p0, l1 - 1 in
  let rec loop last_dir_sep i p0 p1 = match i > max || p0.[i] <> p1.[i] with
  | false ->
      let last_dir_sep = if p0.[i] = dir_sep_char then i else last_dir_sep in
      loop last_dir_sep (i + 1) p0 p1
  | true ->
      if i = 0 then None else
      let last = i - 1 in
      if last_dir_sep = last then Some last else
      match last = max with
      | true ->
          if l1 = l0 then Some last else
          if p1.[i] = dir_sep_char then Some last else
          if last_dir_sep <> -1 then Some last_dir_sep else None
      | false ->
          if last_dir_sep <> -1 then Some last_dir_sep else None
  in
  loop (-1) 0 p0 p1

let find_prefix_windows p0 p1 = match _prefix_last_index p0 p1 with
| None -> None
| Some i ->
    let v0_len = String.Sub.length (fst (Windows.sub_split_volume p0)) in
    let v1_len = String.Sub.length (fst (Windows.sub_split_volume p1)) in
    let max_vlen = if v0_len > v1_len then v0_len else v1_len in
    if i < max_vlen then None else Some (String.with_index_range p0 ~last:i)

let find_prefix_posix p0 p1 = match _prefix_last_index p0 p1 with
| None -> None
| Some 0 when Posix.has_volume p0 || Posix.has_volume p1 -> None
| Some i -> Some (String.with_index_range p0 ~last:i)

let find_prefix = if windows then find_prefix_windows else find_prefix_posix

let rem_prefix prefix p = match is_prefix prefix p with
| false -> None
| true ->
    match String.length prefix with
    | len when len = String.length p -> None
    | len ->
        let first = if p.[len] = dir_sep_char then len + 1 else len in
        match String.with_index_range p ~first with
        | "" -> Some dot_dir
        | q -> Some q

(* Roots and relativization *)

let _relativize ~root p =
  let root = (* root is always interpreted as a directory *)
    let root = normalize root in
    if root.[String.length root - 1] = dir_sep_char then root else
    root ^ dir_sep
  in
  let p = normalize p in
  let rec walk root p = match root, p with
  | (".." :: _, s :: _) when s <> ".." ->
      (* [root] has too many up segments. Cannot walk down to express [p],
           e.g. "../a" can't be expressed relative to "../../". *)
      None
  | (sr :: root, sp :: (_ :: _ as p)) when sr = sp ->
      (* the next directory in [root] and [p] match and it's not the last
         segment of [p], walk to next segment *)
      walk root p
  | [""], [""] ->
      (* walk ends at the end of both path simultaneously, [p] is a
         directory that matches exactly [root] interpreted as a directory. *)
      Some (segs_to_path ["."; ""])
  | root, p ->
      (* walk ends here, either the next directory is different in
         [root] and [p] or it is equal but it is the last one for [p]
         and different from [""] (i.e. [p] is a file path and prefix
         of [root]). To get to the current position from the remaining
         root we need to go up the number of non-empty segments that
         remain in [root] (length root - 1). To get to the path [p]
         from the current position we just use [p] so prepending
         length root - 1 ".." segments to [p] tells us how to go from
         the remaining root to [p]. *)
      let segs = List.fold_left (fun acc _ -> dotdot :: acc) p (List.tl root) in
      Some (segs_to_path segs)
  in
  match segs root, segs p with
  | ("" :: _, s :: _) when s <> "" -> None (* absolute/relative mismatch *)
  | (s :: _, "" :: _) when s <> "" -> None (* absolute/relative mismatch *)
  | ["."; ""], p ->
      (* p is relative and must be expressed w.r.t. "./", so it is itself. *)
      Some (segs_to_path p)
  | root, p ->
      (* walk in the segments of root and p until a segment mismatches.
         at that point express the remaining p relative to the remaining
         root. Note that because of normalization both [root] and [p] may
         only have initial .. segments and [root] by construction has a
         final "" segment. *)
      walk root p

let relativize_windows ~root p =
  let rvol, root = Windows.sub_split_volume root in
  let pvol, p = Windows.sub_split_volume p in
  if not (String.Sub.equal_bytes rvol pvol) then None else
  let root = String.Sub.to_string root in
  let p = String.Sub.to_string p in
  _relativize ~root p

let relativize_posix ~root p = _relativize ~root p

let relativize = if windows then relativize_windows else relativize_posix

let is_rooted ~root p = match relativize ~root p with
| None -> false
| Some r -> not (String.equal dotdot r || String.is_prefix dotdot_dir r)

(* Predicates and comparison *)

let is_rel_posix p = p.[0] <> dir_sep_char
let is_rel_windows p =
  if Windows.is_unc_path p then false else
  p.[Windows.non_unc_path_start p] <> dir_sep_char

let is_rel = if windows then is_rel_windows else is_rel_posix
let is_abs p = not (is_rel p)
let is_root = if windows then Windows.is_root else Posix.is_root

let is_current_dir_posix ?(prefix = false) p = match prefix with
| false ->  String.equal dot p || String.equal dot_dir p
| true -> String.equal dot p || String.is_prefix dot_dir p

let is_current_dir_windows ?(prefix = false) p =
  if Windows.is_unc_path p then false else
  let start = Windows.non_unc_path_start p in
  match String.length p - start with
  | 1 -> p.[start] = '.'
  | n when n = 2 || prefix -> p.[start] = '.' && p.[start + 1] = dir_sep_char
  | _ -> false

let is_current_dir =
  if windows then is_current_dir_windows else is_current_dir_posix

let is_parent_dir_posix ?(prefix = false) p = match prefix with
| false -> String.equal dotdot p || String.equal dotdot_dir p
| true -> String.equal dotdot p || String.is_prefix dotdot_dir p

let is_parent_dir_windows ?(prefix = false) p =
  if Windows.is_unc_path p then false else
  let start = Windows.non_unc_path_start p in
  match String.length p - start with
  | 1 -> false
  | 2 -> p.[start] = '.' && p.[start + 1] = '.'
  | n when n = 3 || prefix ->
      p.[start] = '.' && p.[start + 1] = '.' && p.[start + 2] = dir_sep_char
  | _ -> false

let is_parent_dir =
  if windows then is_parent_dir_windows else is_parent_dir_posix

let is_dotfile p = match basename p with | "" -> false | s -> s.[0] = '.'

let equal = String.equal
let compare = String.compare

(* Conversions and pretty printing *)

let to_string p = p
let pp ppf p = Format.pp_print_string ppf (to_string p)
let dump ppf p = String.dump ppf (to_string p)

(* File extensions *)

type ext = string

let ext_sep_char = '.'
let ext_sep = String.of_char ext_sep_char
let ext_sep_sub = String.Sub.of_char ext_sep_char
let eq_ext_sep c = c = ext_sep_char
let neq_ext_sep c = c <> ext_sep_char

let rec sub_multi_ext seg =
  let first_not_sep = String.Sub.drop ~sat:eq_ext_sep seg in
  String.Sub.drop ~sat:neq_ext_sep first_not_sep

let sub_single_ext seg =
  let name_dot, ext = String.Sub.span ~rev:true ~sat:neq_ext_sep seg in
  if String.Sub.exists neq_ext_sep name_dot
  then String.Sub.extend ~max:1 ~rev:true ext
  else String.Sub.empty

let sub_ext ?(multi = false) seg =
  if multi then sub_multi_ext seg else sub_single_ext seg

let sub_get_ext ?multi p = sub_ext ?multi (sub_last_non_empty_seg p)
let get_ext ?multi p = String.Sub.to_string (sub_get_ext ?multi p)

let has_ext e p =
  let ext = sub_get_ext ~multi:true p in
  if String.Sub.is_empty ext then false else
  if not (String.(Sub.is_suffix (sub e) ext)) then false else
  if not (String.is_empty e) && e.[0] = ext_sep_char then true else
  (* Check there's a dot before the suffix [e] in [ext] *)
  let dot_index = String.Sub.length ext - String.length e - 1 in
  String.Sub.get ext dot_index = ext_sep_char

let mem_ext exts p = List.exists (fun ext -> has_ext ext p) exts

let exists_ext ?(multi = false) p =
  let ext = sub_get_ext ~multi p in
  if multi then String.Sub.exists eq_ext_sep (String.Sub.tail ext) else
  not (String.Sub.is_empty ext)

let add_ext e p =
  if String.is_empty e then p else
  if not (is_seg e) then invalid_arg (err_invalid_ext e) else
  let seg = sub_last_non_empty_seg p in
  if sub_is_dir_seg seg then p else
  let e_has_dot = e.[0] = ext_sep_char in
  let maybe_dot = if e_has_dot then String.Sub.empty else ext_sep_sub in
  let has_empty = p.[String.length p - 1] = dir_sep_char in
  let maybe_empty = if has_empty then dir_sep_sub else String.Sub.empty in
  let seg_end = String.Sub.stop_pos seg - 1 in
  let prefix = String.sub_with_index_range ~last:seg_end p in
  let path = [prefix; maybe_dot; String.sub e; maybe_empty] in
  String.Sub.(base_string (concat path))

let _split_ext ?multi p =
  let ext = sub_get_ext ?multi p in
  if String.Sub.is_empty ext then p, ext else
  let before_ext = String.Sub.start_pos ext - 1 in
  if String.Sub.stop_pos ext = String.length p
  then String.with_index_range p ~last:before_ext, ext else
  let prefix = String.sub_with_index_range p ~last:before_ext in
  String.Sub.(base_string (concat [prefix; dir_sep_sub])), ext

let rem_ext ?multi p = fst (_split_ext ?multi p)
let set_ext ?multi e p = add_ext e (rem_ext ?multi p)
let split_ext ?multi p =
  let p, ext = _split_ext ?multi p in
  p, String.Sub.to_string ext

let ( + ) p e = add_ext e p
let ( -+ ) p e = set_ext e p

(* Path sets and maps *)

type path = t

module Set = struct
  include Set.Make (String)

  let pp ?sep:(pp_sep = Format.pp_print_cut) pp_elt ppf ps =
    let pp_elt elt is_first =
      if is_first then () else pp_sep ppf ();
      Format.fprintf ppf "%a" pp_elt elt; false
    in
    ignore (fold pp_elt ps true)

  let dump_path = dump
  let dump ppf ss =
    let pp_elt elt is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "%a" dump_path elt;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_elt ss true);
    Format.fprintf ppf "}@]";
    ()

  let err_empty () = invalid_arg "empty set"
  let err_absent p ps =
    invalid_arg (strf "%a not in set %a" dump_path p dump ps)

  let get_min_elt ps = try min_elt ps with Not_found -> err_empty ()
  let min_elt ps = try Some (min_elt ps) with Not_found -> None

  let get_max_elt ps = try max_elt ps with Not_found -> err_empty ()
  let max_elt ps = try Some (max_elt ps) with Not_found -> None

  let get_any_elt ps = try choose ps with Not_found -> err_empty ()
  let choose ps = try Some (choose ps) with Not_found -> None

  let get p ps = try find p ps with Not_found -> err_absent p ps
  let find p ps = try Some (find p ps) with Not_found -> None

  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = struct
  include Map.Make (String)

  let err_empty () = invalid_arg "empty map"
  let err_absent s = invalid_arg (strf "%s is not bound in map" s)

  let get_min_binding m = try min_binding m with Not_found -> err_empty ()
  let min_binding m = try Some (min_binding m) with Not_found -> None

  let get_max_binding m = try max_binding m with Not_found -> err_empty ()
  let max_binding m = try Some (max_binding m) with Not_found -> None

  let get_any_binding m = try choose m with Not_found -> err_empty ()
  let choose m = try Some (choose m) with Not_found -> None

  let get k s = try find k s with Not_found -> err_absent k
  let find k m = try Some (find k m) with Not_found -> None

  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty

  let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

  let pp ?sep:(pp_sep = Format.pp_print_cut) pp_binding ppf (m : 'a t) =
    let pp_binding k v is_first =
      if is_first then () else pp_sep ppf ();
      pp_binding ppf (k, v); false
    in
    ignore (fold pp_binding m true)

  let dump pp_v ppf m =
    let pp_binding k v is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "@[<1>(@[%a@],@ @[%a@])@]" dump k pp_v v;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_binding m true);
    Format.fprintf ppf "}@]";
    ()
end

type set = Set.t
type 'a map = 'a Map.t

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
