(*
 * Copyright (c) 2019 Nathan Rebours <nathan.p.rebours@gmail.com>
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

open Result

type t = { base_name : string; sub_lib : string option }

let compare t t' =
  let compare_opt cmp o o' =
    match (o, o') with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some x, Some x' -> cmp x x'
  in
  let { base_name; sub_lib } = t in
  let { base_name = base_name'; sub_lib = sub_lib' } = t' in
  match String.compare base_name base_name' with
  | 0 -> compare_opt String.compare sub_lib sub_lib'
  | c -> c

let equal t t' = compare t t' = 0

let pp fmt { base_name; sub_lib } =
  let cst s = Fmt.(const string s) in
  Fmt.string fmt "{ ";
  Fmt.(pair ~sep:(cst "; ") string (option ~none:(cst "None") string))
    fmt (base_name, sub_lib);
  Fmt.string fmt " }"

let from_string s =
  let invalid () = Error (Printf.sprintf "Invalid library name: %S" s) in
  match Astring.String.cuts ~sep:"." s with
  | [ "" ] | [ ""; _ ] | [ _; "" ] -> invalid ()
  | [ base_name ] -> Ok { base_name; sub_lib = None }
  | base_name :: sl -> Ok { base_name; sub_lib = Some (String.concat "." sl) }
  | [] -> (* String.cuts invariant *) assert false

module Set = struct
  include Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let to_package_set t =
    fold
      (fun t acc -> Astring.String.Set.add t.base_name acc)
      t Astring.String.Set.empty
end
