(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

type lookup_result_found = Component_table.lookup_result_found =
  { root : Odoc_model.Root.t; hidden : bool }

type lookup_result = Component_table.lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type msg = [ `Msg of string ]

(** Internal. Used to handled errors from [fetch_unit] and [fetch_page] *)
exception Fetch_failed of msg

let core_types = Odoc_model.Predefined.core_types

let core_exceptions = Odoc_model.Predefined.core_exceptions

type resolver = Resolve.resolver

let build_resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  let fetch_unit root =
    match fetch_unit root with
    | Ok unit -> unit
    | Error e -> raise (Fetch_failed e)
  and fetch_page root =
    match fetch_page root with
    | Ok page -> page
    | Error e -> raise (Fetch_failed e)
  in
  Resolve.build_resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page

let resolve resolver unit =
  try Ok (Resolve.resolve resolver unit)
  with Fetch_failed (`Msg _ as e) -> Error e

let resolve_page resolver page =
  try Ok (Resolve.resolve_page resolver page)
  with Fetch_failed (`Msg _ as e) -> Error e

type expander = Expand.t

let build_expander ?equal ?hash lookup fetch =
  let fetch ~root root' =
    match fetch ~root root' with
    | Ok unit -> unit
    | Error e -> raise (Fetch_failed e)
  in
  Expand.build_expander ?equal ?hash lookup fetch

let expand expander unit =
  try Ok (Expand.expand expander unit)
  with Fetch_failed (`Msg _ as e) -> Error e

module Lookup = Lookup
