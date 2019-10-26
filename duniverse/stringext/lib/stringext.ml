open String

let string_after s n = String.sub s n (String.length s - n)

let quote s =
  let len = String.length s in
  let buf = Buffer.create (2 * len) in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf


(* Not tail recursive for "performance", please choose low values for
   [max]. The idea is that max is always small because it's hard
   code *)
let split_char_bounded str ~on ~max =
  if str = "" then []
  else if max = 1 then [str]
  else
    let rec loop offset tokens =
      if tokens = max - 1
      then [sub str offset (length str - offset)]
      else
        try
          let index = index_from str offset on in
          if index = offset then
            ""::(loop (offset + 1) (tokens + 1))
          else
            let token = String.sub str offset (index - offset) in
            token::(loop (index + 1) (tokens + 1))
        with Not_found -> [sub str offset (length str - offset)]
    in loop 0 0

let split_char_unbounded str ~on =
  if str = "" then []
  else
    let rec loop acc offset =
      try begin
        let index = rindex_from str offset on in
        if index = offset then
          loop (""::acc) (index - 1)
        else
          let token = sub str (index + 1) (offset - index) in
          loop (token::acc) (index - 1)
      end
      with Not_found -> (sub str 0 (offset + 1))::acc
    in loop [] (length str - 1)

let of_char = String.make 1

let full_split str ~on =
  if str = "" then []
  else
    let sep = of_char on in
    let rec loop acc offset =
      try begin
        let index = rindex_from str offset on in
        if index = offset then
          loop (sep::acc) (index - 1)
        else
          let token = sub str (index + 1) (offset - index) in
          loop (sep::token::acc) (index - 1)
      end
      with Not_found ->
        if offset >= 0
        then (sub str 0 (offset + 1))::acc
        else acc
    in loop [] (length str - 1)

(* copying core's convention for String.split but with an optional max
   argument *)
let split ?max s ~on =
  match max with
  | None -> split_char_unbounded s ~on
  | Some max ->                 (* assert (max < 100); *)
    split_char_bounded s ~on ~max

let rindex_from_on s ~offset ~on =
  let rec loop i =
    if i < 0 then raise Not_found
    else if String.contains on s.[i] then i
    else loop (i - 1)
  in loop offset

let trim_left_sub s ~pos ~len ~chars =
  let start_pos =
    let final = pos + len in
    let rec loop last_char i =
      if i = final then last_char
      else if String.contains chars s.[i] then loop (i + 1) (i + 1)
      else last_char
    in loop pos pos
  in
  let new_len = len - (start_pos - pos) in
  String.sub s start_pos new_len

let split_trim_left str ~on ~trim =
  if str = "" then []
  else
    let rec loop acc offset =
      try begin
        let index = rindex_from_on str ~offset ~on in
        if index = offset then
          loop (""::acc) (index - 1)
        else
          let token = trim_left_sub str ~pos:(index + 1)
                        ~len:(offset - index) ~chars:trim in
          loop (token::acc) (index - 1)
      end
      with Not_found ->
        (trim_left_sub str ~pos:0 ~len:(offset + 1) ~chars:trim)::acc
    in loop [] (length str - 1)

exception Found_int of int

let first_char_ne s c =
  String.length s > 0 && s.[0] <> c

let trim_left s =
  if first_char_ne s ' ' then s
  else
    let len = String.length s in
    try
      for i=0 to len - 1 do
        if s.[i] <> ' ' then raise (Found_int i)
      done;
      ""
    with Found_int non_space ->
      sub s non_space (len - non_space)

let substr_eq ?(start=0) s ~pattern =
  try
    for i = 0 to String.length pattern - 1 do
      if s.[i + start] <> pattern.[i] then raise Exit
    done;
    true
  with _ -> false

let find_from ?(start=0) str ~pattern =
  try
    for i = start to (String.length str) - (String.length pattern) do
      if substr_eq ~start:i str ~pattern then
        raise (Found_int i)
    done;
    None
  with
  | Found_int i -> Some i
  |  _ -> None

let find_min l ~f =
  let rec loop x fx = function
    | [] -> Some (x, fx)
    | x'::xs ->
      let fx' = f x' in
      if fx' < fx then loop x' fx' xs
      else loop x fx xs
  in
  match l with
  | [] -> None
  | x::xs -> loop x (f x) xs

let replace_all str ~pattern ~with_ =
  let (slen, plen) = String.(length str, length pattern) in
  let buf = Buffer.create slen in
  let rec loop i =
    match find_from ~start:i str ~pattern with
    | None ->
      Buffer.add_substring buf str i (slen - i);
      Buffer.contents buf
    | Some j ->
      Buffer.add_substring buf str i (j - i);
      Buffer.add_string buf with_;
      loop (j + plen)
  in loop 0

exception Found_replace of int * string * string

let replace_all_assoc str tbl =
  let slen = String.length str in
  let buf = Buffer.create slen in
  let rec loop i =
    if i >= slen then Buffer.contents buf
    else
      let r =
        try
          let found = ref false in
          let e =
            find_min tbl ~f:(fun (pattern, with_) ->
              match find_from ~start:i str ~pattern with
              | None   -> max_int
              | Some j when j = i -> raise (Found_replace (j, pattern, with_))
              | Some j -> found := true; j)
          in
          match e with
          | None -> None
          | Some ((pattern, with_), j) when !found -> Some (j, pattern, with_)
          | Some _ -> None
        with Found_replace (j, pattern, with_) -> Some (j, pattern, with_)
      in
      match r with
      | None ->
        Buffer.add_substring buf str i (slen - i);
        Buffer.contents buf
      | Some (j, pattern, with_) ->
        Buffer.add_substring buf str i (j - i);
        Buffer.add_string buf with_;
        loop (j + String.length pattern)
  in loop 0

let iteri f l =
  let rec loop i = function
    | [] -> ()
    | x::xs -> (f i x); loop (succ i) xs
  in loop 0 l

let of_list xs =
  let l = List.length xs in
  let s = Bytes.create l in
  iteri (fun i c -> Bytes.set s i c) xs;
  Bytes.unsafe_to_string s

let to_list s =
  let rec loop acc i =
    if i = -1 then acc
    else
      loop (s.[i] :: acc) (pred i)
  in loop [] (String.length s - 1)

let of_array a =
  let len = Array.length a in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set bytes i a.(i)
  done;
  Bytes.unsafe_to_string bytes

let to_array s = Array.init (String.length s) (String.get s)

(* ripped off from one of dbuenzli's libs *)
let cut s ~on =
  let sep_max = length on - 1 in
  if sep_max < 0 then invalid_arg "Stringext.cut: empty separator" else
    let s_max = length s - 1 in
    if s_max < 0 then None else
      let k = ref 0 in
      let i = ref 0 in
      (* We run from the start of [s] to end with [i] trying to match the
         first character of [on] in [s]. If this matches, we verify that
         the whole [on] is matched using [k]. If it doesn't match we
         continue to look for [on] with [i]. If it matches we exit the
         loop and extract a substring from the start of [s] to the
         position before the [on] we found and another from the position
         after the [on] we found to end of string. If [i] is such that no
         separator can be found we exit the loop and return the no match
         case. *)
      try
        while (!i + sep_max <= s_max) do
          (* Check remaining [on] chars match, access to unsafe s (!i + !k) is
             guaranteed by loop invariant. *)
          if unsafe_get s !i <> unsafe_get on 0 then incr i else begin
            k := 1;
            while (!k <= sep_max && unsafe_get s (!i + !k) = unsafe_get on !k)
            do incr k done;
            if !k <= sep_max then (* no match *) incr i else raise Exit
          end
        done;
        None (* no match in the whole string. *)
      with
      | Exit -> (* i is at the beginning of the separator *)
        let left_end = !i - 1 in
        let right_start = !i + sep_max + 1 in
        Some (sub s 0 (left_end + 1),
              sub s right_start (s_max - right_start + 1))

let rcut s ~on =
  let sep_max = length on - 1 in
  if sep_max < 0 then invalid_arg "Stringext.rcut: empty separator" else
    let s_max = length s - 1 in
    if s_max < 0 then None else
      let k = ref 0 in
      let i = ref s_max in
      (* We run from the end of [s] to the beginning with [i] trying to
         match the last character of [on] in [s]. If this matches, we
         verify that the whole [on] is matched using [k] (we do that
         backwards).  If it doesn't match we continue to look for [on]
         with [i].  If it matches we exit the loop and extract a
         substring from the start of [s] to the position before the
         [on] we found and another from the position after the [on] we
         found to end of string.  If [i] is such that no separator can
         be found we exit the loop and return the no match case. *)
      try
        while (!i >= sep_max) do
          if unsafe_get s !i <> unsafe_get on sep_max then decr i else begin
            (* Check remaining [on] chars match, access to unsafe_get
               s (sep_start + !k) is guaranteed by loop invariant. *)
            let sep_start = !i - sep_max in
            k := sep_max - 1;
            while (!k >= 0 && unsafe_get s (sep_start + !k) = unsafe_get on !k)
            do decr k done;
            if !k >= 0 then (* no match *) decr i else raise Exit
          end
        done;
        None (* no match in the whole string. *)
      with
      | Exit -> (* i is at the end of the separator *)
        let left_end = !i - sep_max - 1 in
        let right_start = !i + 1 in
        Some (sub s 0 (left_end + 1),
              sub s right_start (s_max - right_start + 1))

let chop_prefix s ~prefix =
  let prefix_l = String.length prefix in
  let string_l = String.length s in
  if prefix_l > string_l then None
  else
    try
      for i = 0 to prefix_l - 1 do
        if s.[i] <> prefix.[i] then raise Exit;
      done;
      Some (String.sub s prefix_l (string_l - prefix_l))
    with _ -> None

let drop s n =
  let l = String.length s in
  if n >= l
  then ""
  else String.sub s n (l - n)

let take s n =
  if n >= String.length s
  then s
  else String.sub s 0 n
