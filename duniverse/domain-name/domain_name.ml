(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring

type 'a s = string array

let root = Array.make 0 ""

let [@inline always] check_host_label s =
  String.get s 0 <> '-' && (* leading may not be '-' *)
  String.for_all (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
      | _ -> false)
    s (* only LDH (letters, digits, hyphen)! *)

let host_exn t =
  (* TLD should not be all-numeric! *)
  if
    (if Array.length t > 0 then
       String.exists Char.Ascii.is_letter (Array.get t 0)
     else true) &&
    Array.for_all check_host_label t
  then
    t
  else
    invalid_arg "invalid host name"

let host t =
  try Ok (host_exn t) with
  | Invalid_argument e -> Error (`Msg e)

let check_service_label s =
  match String.cut ~sep:"_" s with
  | None -> false
  | Some (empty, srv) ->
    if String.length empty > 0 then
      false
    else
      let slen = String.length srv in
      if slen > 0 && slen <= 15 then
        (* service must be LDH,
           hyphen _not_ at begin nor end, no hyphen following a hyphen
           1-15 characters *)
        let v, _ = String.fold_left (fun (valid, h) c ->
            let h' = c = '-' in
            let v = Char.Ascii.(is_letter c || is_digit c) || h' in
            let hh = not (h && h') in
            (v && valid && hh, h'))
            (true, false) srv
        in
        v && String.get srv 0 <> '-' && String.get srv (pred slen) <> '-'
      else
        false

let [@inline always] is_proto s =
  s = "_tcp" || s = "_udp" || s = "_sctp"

let [@inline always] check_label_length s =
  let l = String.length s in
  l < 64 && l > 0

let [@inline always] check_total_length t =
  Array.fold_left (fun acc s -> acc + 1 + String.length s) 1 t <= 255

let service_exn t =
  let l = Array.length t in
  if
    if l > 2 then
      let name = Array.sub t 0 (l - 2) in
      check_service_label (Array.get t (l - 1)) &&
      is_proto (Array.get t (l - 2)) &&
      Array.for_all check_label_length name &&
      check_total_length t &&
      match host name with Ok _ -> true | Error _ -> false
    else
      false
  then
    t
  else
    invalid_arg "invalid service name"

let service t =
  try Ok (service_exn t) with
  | Invalid_argument e -> Error (`Msg e)

let raw t = t

let [@inline always] check t =
  Array.for_all check_label_length t &&
  check_total_length t

let get_label_exn ?(rev = false) xs idx =
  let idx' = if rev then idx else pred (Array.length xs) - idx in
  try Array.get xs idx' with
  | Invalid_argument _ -> invalid_arg "bad index for domain name"

let get_label ?rev xs idx =
  try Ok (get_label_exn ?rev xs idx) with
  | Invalid_argument e -> Error (`Msg e)

let find_label_exn ?(rev = false) xs p =
  let l = pred (Array.length xs) in
  let check x = x >= 0 && x <= l in
  let rec go next idx =
    if check idx then
      if p (Array.get xs idx) then
        idx
      else
        go next (next idx)
    else
      invalid_arg "label not found"
  in
  let next, start = if rev then (succ, 0) else (pred, l) in
  let r = go next start in
  l - r

let find_label ?rev xs p =
  try Some (find_label_exn ?rev xs p) with
  | Invalid_argument _ -> None

let count_labels xs = Array.length xs

let prepend_label_exn xs lbl =
  let n = Array.make 1 lbl in
  let n = Array.append xs n in
  if check_label_length lbl && check_total_length n then n
  else invalid_arg "invalid domain name"

let prepend_label xs lbl =
  try Ok (prepend_label_exn xs lbl) with
  | Invalid_argument e -> Error (`Msg e)

let drop_label_exn ?(rev = false) ?(amount = 1) t =
  let len = Array.length t - amount
  and start = if rev then amount else 0
  in
  Array.sub t start len

let drop_label ?rev ?amount t =
  try Ok (drop_label_exn ?rev ?amount t) with
  | Invalid_argument _ -> Error (`Msg "couldn't drop labels")

let append_exn pre post =
  let r = Array.append post pre in
  if check_total_length r then r else invalid_arg "invalid domain name"

let append pre post =
  try Ok (append_exn pre post) with
  | Invalid_argument _ -> Error (`Msg "couldn't concatenate domain names")

let of_strings_exn xs =
  let labels =
    (* we support both example.com. and example.com *)
    match List.rev xs with
    | ""::rst -> rst
    | rst -> rst
  in
  let t = Array.of_list labels in
  if check t then t
  else invalid_arg "invalid host name"

let of_strings xs =
  try Ok (of_strings_exn xs) with
  | Invalid_argument e -> Error (`Msg e)

let of_string s = of_strings (String.cuts ~sep:"." s)

let of_string_exn s = of_strings_exn (String.cuts ~sep:"." s)

let of_array a = a

let to_array a = a

let to_strings ?(trailing = false) dn =
  let labels = Array.to_list dn in
  List.rev (if trailing then "" :: labels else labels)

let to_string ?trailing dn = String.concat ~sep:"." (to_strings ?trailing dn)

let canonical t =
  let str = to_string t in
  of_string_exn (String.Ascii.lowercase str)

(*BISECT-IGNORE-BEGIN*)
let pp ppf xs = Fmt.string ppf (to_string xs)
(*BISECT-IGNORE-END*)

let compare_label a b =
  String.compare (String.Ascii.lowercase a) (String.Ascii.lowercase b)

let compare_domain cmp_sub a b =
  let la = Array.length a in
  match compare la (Array.length b) with
  | 0 ->
    let rec cmp idx =
      if idx = la then 0
      else
        match cmp_sub (Array.get a idx) (Array.get b idx) with
        | 0 -> cmp (succ idx)
        | x -> x
    in
    cmp 0
  | x -> x

let compare = compare_domain compare_label

let equal_label ?(case_sensitive = false) a b =
  let cmp = if case_sensitive then String.compare else compare_label in
  cmp a b = 0

let equal ?(case_sensitive = false) a b =
  let cmp = if case_sensitive then String.compare else compare_label in
  compare_domain cmp a b = 0

let is_subdomain ~subdomain ~domain =
  let supl = Array.length domain in
  let rec cmp idx =
    if idx = supl then
      true
    else
      compare_label (Array.get domain idx) (Array.get subdomain idx) = 0 &&
      cmp (succ idx)
  in
  if Array.length subdomain < supl then
    false
  else
    cmp 0

module Ordered = struct
  type t = [ `raw ] s
  let compare = compare_domain compare_label
end

module Host_ordered = struct
  type t = [ `host ] s
  let compare = compare_domain compare_label
end

module Service_ordered = struct
  type t = [ `service ] s
  let compare = compare_domain compare_label
end

type 'a t = 'a s

module Host_map = struct
  include Map.Make(Host_ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Host_set = Set.Make(Host_ordered)

module Service_map = struct
  include Map.Make(Service_ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Service_set = Set.Make(Service_ordered)

module Map = struct
  include Map.Make(Ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Set = Set.Make(Ordered)
