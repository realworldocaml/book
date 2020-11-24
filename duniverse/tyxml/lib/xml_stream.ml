(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2018 Gabriel Radanne
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

type name = string * string

(** Input *)

type signal = [
  | `Comment of string
  | `End_element
  | `Start_element of name * (name * string) list
  | `Text of string list
]

exception Malformed_stream

module Import
    (Xml : Xml_sigs.T)
= struct

  let of_list l =
    List.fold_right
      (fun a b -> Xml.W.(cons (return a) b))
      l (Xml.W.nil ())

  let mk_attribs attrs =
    (* TODO: This is not very structured *)
    let f ((_,name), v) = Xml.string_attrib name (Xml.W.return v) in
    List.map f attrs

  let rec mk children (seq : signal Seq.t) = match seq () with
    | Cons (`Comment s, q) ->
      mk (Xml.comment s :: children) q
    | Cons (`Text s, q) ->
      mk (List.map (fun x -> Xml.pcdata @@ Xml.W.return x) s @ children) q
    | Cons (`Start_element ((_, name), attrs), q) ->
      let a = mk_attribs attrs in
      let sub_children, rest = mk [] q in
      mk (Xml.node ~a name sub_children :: children) rest
    | Cons (`End_element, rest) ->
      of_list (List.rev children), rest
    | Nil ->
      of_list (List.rev children), Seq.empty

  let of_seq seq =
    let l, rest = mk [] seq in
    match rest () with
    | Seq.Nil -> l
    | _ -> raise Malformed_stream

end
