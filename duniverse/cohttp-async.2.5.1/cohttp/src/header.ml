(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011-2012 Martin Jambon <martin@mjambon.com>
 * Copyright (c) 2010 Mika Illouz
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

module LString : sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  val compare: t -> t -> int
end = struct
  type t = string
  let of_string x = String.lowercase_ascii x
  let to_string x = x
  let compare a b = String.compare a b
end

module StringMap = Map.Make(LString)
type t = string list StringMap.t

let user_agent = Printf.sprintf "ocaml-cohttp/%s" Conf.version

let compare = StringMap.compare Stdlib.compare

let headers_with_list_values = Array.map LString.of_string [|
  "accept";"accept-charset";"accept-encoding";"accept-language";
  "accept-ranges";"allow";"cache-control";"connection";"content-encoding";
  "content-language";"expect";"if-match";"if-none-match";"link";"pragma";
  "proxy-authenticate";"te";"trailer";"transfer-encoding";"upgrade";
  "vary";"via";"warning";"www-authenticate"; |]

let is_header_with_list_value =
  let tbl = Hashtbl.create (Array.length headers_with_list_values) in
  headers_with_list_values |> Array.iter (fun h -> Hashtbl.add tbl h ());
  fun h -> Hashtbl.mem tbl h

let init () =
  StringMap.empty

let is_empty x = StringMap.is_empty x

let init_with k v =
  StringMap.singleton (LString.of_string k) [v]

let add h k v =
  let k = LString.of_string k in
  try StringMap.add k (v::(StringMap.find k h)) h
  with Not_found -> StringMap.add k [v] h

let add_list h l =
  List.fold_left (fun h (k, v) -> add h k v) h l

let add_multi h k l =
  List.fold_left (fun h v -> add h k v) h l

let add_opt h k v =
  match h with
  |None -> init_with k v
  |Some h -> add h k v

let remove h k =
  let k = LString.of_string k in
  StringMap.remove k h

let replace h k v =
  let k = LString.of_string k in
  StringMap.add k [v] h

let get h k =
  let k = LString.of_string k in
  try
    let v = StringMap.find k h in
    if is_header_with_list_value k
    then Some (String.concat "," v)
    else Some (List.hd v)
  with Not_found | Failure _ -> None

let mem h k = StringMap.mem (LString.of_string k) h

let add_unless_exists h k v =
  if mem h k
  then h
  else add h k v

let add_opt_unless_exists h k v =
  match h with
  | None -> init_with k v
  | Some h -> add_unless_exists h k v

let get_multi h k =
  let k = LString.of_string k in
  try StringMap.find k h with Not_found -> []

let map fn h = StringMap.mapi (fun k v -> fn (LString.to_string k) v)  h
let iter fn h = ignore(map fn h)
let fold fn h acc = StringMap.fold
  (fun k v acc -> List.fold_left (fun acc v -> fn (LString.to_string k) v acc) acc v)
  h acc
let of_list l = List.fold_left (fun h (k,v) -> add h k v) (init ()) l

let to_list h = List.rev (fold (fun k v acc -> (k,v)::acc) h [])
let header_line k v = Printf.sprintf "%s: %s\r\n" k v
let to_lines h = List.rev (fold (fun k v acc -> (header_line k v)::acc) h [])

let to_frames =
  let to_frame k v acc = (Printf.sprintf "%s: %s" k v) :: acc in
  fun h -> List.rev (fold to_frame h [])

let to_string h =
  let b = Buffer.create 128 in
  h |> iter (fun k v ->
    v |> List.iter (fun v ->
      Buffer.add_string b k;
      Buffer.add_string b ": ";
      Buffer.add_string b v;
      Buffer.add_string b "\r\n"
    );
  );
  Buffer.add_string b "\r\n";
  Buffer.contents b

let parse_content_range s =
  try
    let start, fini, total =
      Scanf.sscanf s "bytes %Ld-%Ld/%Ld" (fun start fini total -> start, fini, total) in
    Some (start, fini, total)
  with Scanf.Scan_failure _ -> None

(* If we see a "Content-Range" header, than we should limit the
   number of bytes we attempt to read *)
let get_content_range headers =
  match get headers "content-length" with
  | Some clen -> (try Some (Int64.of_string clen) with _ -> None)
  | None -> begin
    match get headers "content-range" with
    | Some range_s -> begin
      match parse_content_range range_s with
      | Some (start, fini, total) ->
        (* some sanity checking before we act on these values *)
        if fini < total && start <= total && 0L <= start && 0L <= total
        then (
          let num_bytes_to_read = Int64.add (Int64.sub fini start) 1L in
          Some num_bytes_to_read
        ) else None
      | None -> None
    end
    | None -> None
  end

let get_connection_close headers =
  match get headers "connection" with
  | Some "close" -> true
  | _ -> false


let media_type_re =
  let re = Re.Emacs.re ~case:true "[ \t]*\\([^ \t;]+\\)" in
  Re.(compile (seq ([start; re])))

let get_first_match _re s =
  try
    let subs = Re.exec ~pos:0 media_type_re s in
    let (start, stop) = Re.Group.offset subs 1 in
    Some (String.sub s start (stop - start))
  with Not_found -> None

(* Grab "foo/bar" from " foo/bar ; charset=UTF-8" *)
let get_media_type headers =
  match get headers "content-type" with
  | Some s -> get_first_match media_type_re s
  | None -> None

let get_acceptable_media_ranges headers =
  Accept.media_ranges (get headers "accept")

let get_acceptable_charsets headers =
  Accept.charsets (get headers "accept-charset")

let get_acceptable_encodings headers =
  Accept.encodings (get headers "accept-encoding")

let get_acceptable_languages headers =
  Accept.languages (get headers "accept-language")

(* Parse the transfer-encoding and content-length headers to
 * determine how to decode a body *)
let get_transfer_encoding headers =
  match get headers "transfer-encoding" with
  | Some "chunked" -> Transfer.Chunked
  | Some _ | None -> begin
    match get_content_range headers with
    |Some len -> Transfer.Fixed len
    |None -> Transfer.Unknown
  end

let add_transfer_encoding headers enc =
  let open Transfer in
  (* Only add a header if one doesnt already exist, e.g. from the app *)
  match get_transfer_encoding headers, enc with
  |Fixed _,_  (* App has supplied a content length, so use that *)
  |Chunked, _ -> headers (* TODO: this is a protocol violation *)
  |Unknown, Chunked -> add headers "transfer-encoding" "chunked"
  |Unknown, Fixed len -> add headers "content-length" (Int64.to_string len)
  |Unknown, Unknown -> headers

let add_authorization_req headers challenge =
  add headers "www-authenticate" (Auth.string_of_challenge challenge)

let add_authorization headers cred =
  add headers "authorization" (Auth.string_of_credential cred)

let get_authorization headers =
  match get headers "authorization" with
  |None -> None
  |Some v -> Some (Auth.credential_of_string v)

let is_form headers =
  get_media_type headers = (Some "application/x-www-form-urlencoded")

let get_location headers =
  match get headers "location" with
  | None -> None
  | Some u -> Some (Uri.of_string u)

let get_links headers =
  List.rev (List.fold_left
    (fun list link_s -> List.rev_append (Link.of_string link_s) list)
    [] (get_multi headers "link"))

let add_links headers links =
  add_multi headers "link" (List.map Link.to_string links)

let prepend_user_agent headers user_agent =
  let k = "user-agent" in
  match get headers k with
    | Some ua -> replace headers k (user_agent^" "^ua)
    | None -> add headers k user_agent

let connection h =
  match get h "connection" with
  | Some v when v = "keep-alive" -> Some `Keep_alive
  | Some v when v = "close" -> Some `Close
  | Some x -> Some (`Unknown x)
  | _ -> None

open Sexplib0.Sexp_conv

let sexp_of_t t =
  sexp_of_list (sexp_of_pair sexp_of_string sexp_of_string) (to_list t)

let t_of_sexp s =
  of_list (list_of_sexp (pair_of_sexp string_of_sexp string_of_sexp) s)

let pp_hum ppf h =
  Format.fprintf ppf "%s" (h |> sexp_of_t |> Sexplib0.Sexp.to_string_hum)
