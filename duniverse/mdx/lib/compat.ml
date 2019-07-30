(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

module String = struct
  include String

#if OCAML_MAJOR = 4 && OCAML_MINOR < 3
    let equal x y = String.compare x y = 0
#endif

#if OCAML_MAJOR = 4 && OCAML_MINOR < 4
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
          r := String.sub s (i + 1) (!j - i - 1) :: !r;
          j := i
        end
    done;
    String.sub s 0 !j :: !r
#endif
end

module Filename = struct
  include Filename

#if OCAML_MAJOR = 4 && OCAML_MINOR < 4
  let is_dir_sep s i = s.[i] = '/' (* Unix only *)

  let extension_len name =
    let rec check i0 i =
      if i < 0 || is_dir_sep name i then 0
      else if name.[i] = '.' then check i0 (i - 1)
      else String.length name - i0
    in
    let rec search_dot i =
      if i < 0 || is_dir_sep name i then 0
      else if name.[i] = '.' then check i (i - 1)
      else search_dot (i - 1)
    in
    search_dot (String.length name - 1)

  let extension name =
    let l = extension_len name in
    if l = 0 then "" else String.sub name (String.length name - l) l
#endif
end

module List = struct
  include List

#if OCAML_MAJOR = 4 && OCAML_MINOR < 6
  let rec init_aux i n f =
    if i >= n then []
    else (f i) :: init_aux (i+1) n f

  let init n f = init_aux 0 n f
#endif

#if OCAML_MAJOR = 4 && OCAML_MINOR < 5
  let rec find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else find_opt p l

  let rec compare_length_with l n =
    match l with
    | [] ->
       if n = 0 then 0 else
         if n > 0 then -1 else 1
    | _ :: l ->
       if n <= 0 then 1 else
         compare_length_with l (n-1)
#endif
end

module Warnings = struct
  include Warnings

#if OCAML_MAJOR = 4 && OCAML_MINOR < 3
  (* Can't be overriden *)
  let reset_fatal () = ()
#endif
end

#if OCAML_MAJOR = 4 && OCAML_MINOR < 4
module Env = struct
  include Env

  (* Can't be overriden *)
  let without_cmis f x = f x
end
#endif

#if OCAML_MAJOR = 4 && OCAML_MINOR < 9
let init_path () = Compmisc.init_path true
#else
let init_path () = Compmisc.init_path ()
#endif
