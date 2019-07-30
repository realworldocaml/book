(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type MUTEX =
sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val try_lock : t -> bool
  val unlock : t -> unit
end

module HashPhysical = Hashtbl.Make
  (struct
    type t = Obj.t
    let hash = Hashtbl.hash
    let equal = (==)
   end)

module Make (Mutex : MUTEX) = struct

  (* Map integer identifiers to functions. *)
  let function_by_id : (int, Obj.t) Hashtbl.t = Hashtbl.create 10

  (* Map functions (not closures) to identifiers. *)
  let id_by_function : int HashPhysical.t = HashPhysical.create 10

  (* A single mutex guards both tables *)
  let tables_lock = Mutex.create ()

  (* (The caller must hold tables_lock) *)
  let store_non_closure_function fn boxed_fn id =
    try
      (* Return the existing identifier, if any. *)
      HashPhysical.find id_by_function fn
    with Not_found ->
      (* Add entries to both tables *)
      HashPhysical.add id_by_function fn id;
      Hashtbl.add function_by_id id boxed_fn;
      id

  let fresh () = Oo.id (object end)

  let finalise key =
    (* GC can be triggered while the lock is already held, in which case we
       abandon the attempt and re-install the finaliser. *)
    let rec cleanup fn =
      begin
        if Mutex.try_lock tables_lock then begin
          Hashtbl.remove function_by_id key;
          Mutex.unlock tables_lock;
        end
        else Gc.finalise cleanup fn;
      end
    in cleanup

  let try_finalise f x =
    try Gc.finalise f x; true
    with Invalid_argument _ -> false

  let record closure boxed_closure : int =
    let key = fresh () in
    (* For closures we add an entry to function_by_id and a finaliser that
       removes the entry. *)
    if try_finalise (finalise key) closure then begin
      Mutex.lock tables_lock;
      Hashtbl.add function_by_id key boxed_closure;
      Mutex.unlock tables_lock;
      key
    end
    else begin
      (* For non-closures we add entries to function_by_id and
         id_by_function. *)
      Mutex.lock tables_lock;
      let id = store_non_closure_function closure boxed_closure key in
      Mutex.unlock tables_lock;
      id
    end

  let retrieve id =
    begin
      Mutex.lock tables_lock;
      let f =
        try Hashtbl.find function_by_id id
        with Not_found ->
          Mutex.unlock tables_lock;
          raise Not_found
      in begin
        Mutex.unlock tables_lock;
        f
      end
    end
end
