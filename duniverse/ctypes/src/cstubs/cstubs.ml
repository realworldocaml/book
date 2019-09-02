(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

[@@@warning "-27-32"]

module type FOREIGN = Ctypes.FOREIGN

module type FOREIGN' = FOREIGN with type 'a result = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

type concurrency_policy = [ `Sequential | `Lwt_jobs | `Lwt_preemptive | `Unlocked ]

type errno_policy = [ `Ignore_errno | `Return_errno ]

let gen_c ~concurrency ~errno prefix fmt : (module FOREIGN') =
  (module
   struct
     let counter = ref 0
     let var prefix name = incr counter;
       Printf.sprintf "%s_%d_%s" prefix !counter name
     type 'a fn = 'a Ctypes.fn
     type 'a return = 'a
     type 'a result = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~concurrency ~errno
         ~cname ~stub_name:(var prefix cname) fmt fn
     let foreign_value cname typ =
       Cstubs_generate_c.value ~cname ~stub_name:(var prefix cname) fmt typ
     let returning = Ctypes.returning
     let (@->) = Ctypes.(@->)
   end)

type bind = Bind : string * string * ('a -> 'b) Ctypes.fn -> bind
type val_bind = Val_bind : string * string * 'a Ctypes.typ -> val_bind

let write_return :
  concurrency:concurrency_policy -> errno:errno_policy ->
  Format.formatter -> unit =
  fun ~concurrency ~errno fmt -> match concurrency, errno with
      (`Sequential|`Unlocked), `Ignore_errno -> Format.fprintf fmt "type 'a return = 'a@\n"
    | (`Sequential|`Unlocked), `Return_errno -> Format.fprintf fmt "type 'a return = 'a * Signed.sint@\n"
    | (`Lwt_jobs|`Lwt_preemptive), `Ignore_errno ->
      begin
        Format.fprintf fmt "type 'a return = { lwt: 'a Lwt.t }@\n";
        Format.fprintf fmt "let box_lwt lwt = {lwt}@\n";
      end
    | (`Lwt_jobs|`Lwt_preemptive), `Return_errno ->
      begin
        Format.fprintf fmt "type 'a return = { lwt: ('a * Signed.sint) Lwt.t }@\n";
        Format.fprintf fmt "let box_lwt lwt = {lwt}@\n";
      end

let write_fn ~concurrency ~errno fmt =
  begin
    Format.fprintf fmt "type 'a fn =@\n";
    Format.fprintf fmt " | Returns  : 'a CI.typ   -> 'a return fn@\n";
    Format.fprintf fmt " | Function : 'a CI.typ * 'b fn  -> ('a -> 'b) fn@\n"
  end

let write_map_result ~concurrency ~errno fmt =
  match concurrency, errno with
    (`Sequential|`Unlocked), `Ignore_errno ->
    Format.fprintf fmt "let map_result f x = f x@\n"
  | (`Sequential|`Unlocked), `Return_errno ->
    Format.fprintf fmt "let map_result f (x, y) = (f x, y)@\n"
  | (`Lwt_jobs|`Lwt_preemptive), `Ignore_errno ->
    Format.fprintf fmt "let map_result f x = Lwt.map f x@\n"
  | (`Lwt_jobs|`Lwt_preemptive), `Return_errno ->
    Format.fprintf fmt "let map_result f v = Lwt.map (fun (x, y) -> (f x, y)) v@\n"

let write_foreign ~concurrency ~errno fmt bindings val_bindings =
  Format.fprintf fmt
    "type 'a result = 'a@\n";
  write_return ~concurrency ~errno fmt;
  write_fn ~concurrency ~errno fmt;
  write_map_result ~concurrency ~errno fmt;
  Format.fprintf fmt
    "let returning t = Returns t@\n";
  Format.fprintf fmt
    "let (@@->) f p = Function (f, p)@\n";
  Format.fprintf fmt
    "let foreign : type a b. string -> (a -> b) fn -> (a -> b) =@\n";
  Format.fprintf fmt
    "  fun name t -> match t, name with@\n@[<v>";
  ListLabels.iter bindings
    ~f:(fun (Bind (stub_name, external_name, fn)) ->
      Cstubs_generate_ml.case ~concurrency ~errno ~stub_name ~external_name fmt fn);
  Format.fprintf fmt "@[<hov 2>@[|@ _,@ s@ ->@]@ ";
  Format.fprintf fmt
    " @[Printf.ksprintf@ failwith@ \"No match for %%s\" s@]@]@]@.@\n";
  Format.fprintf fmt
    "@\n";
  Format.fprintf fmt
    "let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =@\n";
  Format.fprintf fmt
    "  fun name t -> match t, name with@\n@[<v>";
  ListLabels.iter val_bindings
    ~f:(fun (Val_bind (stub_name, external_name, typ)) ->
      Cstubs_generate_ml.val_case ~stub_name ~external_name fmt typ);
  Format.fprintf fmt "@[<hov 2>@[|@ _,@ s@ ->@]@ ";
  Format.fprintf fmt
    " @[Printf.ksprintf@ failwith@ \"No match for %%s\" s@]@]@]@.@\n"

let gen_ml ~concurrency ~errno prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and val_bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a fn = 'a Ctypes.fn
     type 'a return = 'a
     let (@->) = Ctypes.(@->)
     let returning = Ctypes.returning

     type 'a result = unit
     let foreign cname fn =
       let name = var prefix cname in
       bindings := Bind (cname, name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~concurrency ~errno
         ~stub_name:name ~external_name:name fmt fn
     let foreign_value cname typ =
       let name = var prefix cname in
       Cstubs_generate_ml.extern ~concurrency:`Sequential ~errno:`Ignore_errno
         ~stub_name:name ~external_name:name fmt
         Ctypes.(void @-> returning (ptr void));
       val_bindings := Val_bind (cname, name, typ) :: !val_bindings
     let returning = Ctypes.returning
     let (@->) = Ctypes.(@->)
   end),
  fun () ->
    write_foreign ~concurrency ~errno fmt !bindings !val_bindings

let sequential = `Sequential
let lwt_jobs = `Lwt_jobs
let lwt_preemptive = `Lwt_preemptive
let ignore_errno = `Ignore_errno
let return_errno = `Return_errno
let unlocked = `Unlocked

let concurrency_headers = function
    `Sequential -> []
  | `Lwt_jobs | `Lwt_preemptive ->   ["\"lwt_unix.h\"";  "<caml/memory.h>"]
  | `Unlocked -> ["<caml/threads.h>"]

let errno_headers = function
    `Ignore_errno -> []
  | `Return_errno -> ["<errno.h>"]

let headers : concurrency_policy -> errno_policy -> string list =
  fun concurrency errno ->
    ["\"ctypes_cstubs_internals.h\""] @
    errno_headers errno @
    concurrency_headers concurrency

let write_c ?(concurrency=`Sequential) ?(errno=`Ignore_errno)
    fmt ~prefix (module B : BINDINGS) =
  List.iter (Format.fprintf fmt "#include %s@\n") (headers concurrency errno);
  let module M = B((val gen_c ~concurrency ~errno prefix fmt)) in ()

let write_ml ?(concurrency=`Sequential) ?(errno=`Ignore_errno)
    fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml ~concurrency ~errno prefix fmt in
  let () = Format.fprintf fmt "module CI = Cstubs_internals@\n@\n" in
  let module M = B((val foreign)) in
  finally ()

module Types = Cstubs_structs
