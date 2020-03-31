(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Sigs

let force o =
  match o with Some x -> x | None -> assert false

module Make (M : IMPERATIVE_MAPS) = struct

  type t = M.key

  (* Create a generator of fresh integers. *)

  open Gensym

  let g =
    generator()

  let current () =
    current g

  (* Memoizing the function [fun _ -> fresh g] yields the function [encode],
     which maps keys to unique integers. We use [visibly_memoize] so as to
     have access to the memoization table. This allows us to use operations
     such as [M.find] and [M.iter] below. *)

  let (encode : t -> int), (table : int M.t) =
    let module Memo = Memoize.Make(M) in
    Memo.visibly_memoize (fun (_ : t) -> fresh g)

  (* Testing whether a key has been encountered already. *)

  let has_been_encoded (x : t) : bool =
    (* We do not have [M.mem], so we re-implement it in terms of [M.find]. *)
    try
      let _ = M.find x table in
      true
    with Not_found ->
      false

  (* Building a mapping of integer codes back to keys. *)

  let reverse_mapping () : t array =
    let n = current() in
    let reverse : t option array = Array.make n None in
    M.iter (fun x i ->
      reverse.(i) <- Some x
    ) table;
    Array.map force reverse

  module Done () = struct

    type t = M.key

    let n = current()

    let encode x =
      (* It would be an error to try and encode new keys now. Thus, if
         [x] has not been encountered before, the client is at fault.
         Fail with a nice informative message. *)
      if has_been_encoded x then
        encode x
      else
        Printf.sprintf
          "Fix.Numbering: invalid argument passed to \"encode\".\n%s\n"
          __LOC__
        |> invalid_arg

    let reverse =
      reverse_mapping()

    let decode i =
      if 0 <= i && i < n then
        reverse.(i)
      else
        Printf.sprintf
          "Fix.Numbering: invalid argument passed to \"decode\".\n\
           The index %d is not in the range [0, %d).\n%s\n"
          i n __LOC__
        |> invalid_arg

  end

end

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))
