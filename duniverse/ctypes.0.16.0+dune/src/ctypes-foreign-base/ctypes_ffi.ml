(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-9-27"]

module type CLOSURE_PROPERTIES =
sig
  val record : Obj.t -> Obj.t -> int
  (** [record c v] links the lifetimes of [c] and [v], ensuring that [v] is not
      collected while [c] is still live.  The return value is a key
      that can be used to retrieve [v] while [v] is still live. *)

  val retrieve : int -> Obj.t
  (** [retrieve v] retrieves a value using a key returned by [record], or raises
      [Not_found] if [v] is no longer live. *)
end

module Make(Closure_properties : CLOSURE_PROPERTIES) =
struct

  open Ctypes_static
  open Libffi_abi

  (* Register the closure lookup function with C. *)
  let () = Ctypes_ffi_stubs.set_closure_callback Closure_properties.retrieve

  type _ ccallspec =
      Call : bool * (Ctypes_ptr.voidp -> 'a) -> 'a ccallspec
    | WriteArg : ('a -> Ctypes_ptr.voidp -> (Obj.t * int) array -> Obj.t) * 'b ccallspec ->
                 ('a -> 'b) ccallspec

  type arg_type = ArgType : 'a Ctypes_ffi_stubs.ffitype -> arg_type

  (* keep_alive ties the lifetimes of objects together.

     [keep_alive w ~while_live:v] ensures that [w] is not collected while [v] is
     still live.

     If the object v in the call [keep_alive w ~while_live:v] is
     static -- for example, if it is a top-level function -- then it
     is not possible to attach a finaliser to [v] and [w] should be
     kept alive indefinitely, which we achieve by adding it to the
     list [kept_alive_indefinitely].
  *)
  let kept_alive_indefinitely = ref []
  let keep_alive w ~while_live:v =
    try Gc.finalise (fun _ -> Ctypes_memory_stubs.use_value w; ()) v
    with Invalid_argument _ ->
      kept_alive_indefinitely := Obj.repr w :: !kept_alive_indefinitely

  let report_unpassable what =
    let msg = Printf.sprintf "libffi does not support passing %s" what in
    raise (Unsupported msg)

  let rec arg_type : type a. a typ -> arg_type = function
    | Void                                -> ArgType (Ctypes_ffi_stubs.void_ffitype ())
    | Primitive p as prim                 -> let ffitype = Ctypes_ffi_stubs.primitive_ffitype p in
                                             if ffitype = Ctypes_ptr.Raw.null
                                             then report_unpassable
                                               (Ctypes_type_printing.string_of_typ prim)
                                             else ArgType ffitype
    | Pointer _                           -> ArgType (Ctypes_ffi_stubs.pointer_ffitype ())
    | Funptr _                            -> ArgType (Ctypes_ffi_stubs.pointer_ffitype ())
    | OCaml _                             -> ArgType (Ctypes_ffi_stubs.pointer_ffitype ())
    | Union _                             -> report_unpassable "unions"
    | Struct ({ spec = Complete _ } as s) -> struct_arg_type s
    | View { ty }                         -> arg_type ty
    | Array _                             -> report_unpassable "arrays"
    | Bigarray _                          -> report_unpassable "bigarrays"
    | Abstract _                          -> (report_unpassable
                                                "values of abstract type")
    (* The following case should never happen; incomplete types are excluded
       during type construction. *)
    | Struct { spec = Incomplete _ }      -> report_unpassable "incomplete types"
  and struct_arg_type : type s. s structure_type -> arg_type =
     fun ({fields} as s) ->
       let bufspec = Ctypes_ffi_stubs.allocate_struct_ffitype (List.length fields) in
       (* Ensure that `bufspec' stays alive as long as the type does. *)
       keep_alive bufspec ~while_live:s;
       List.iteri
         (fun i (BoxedField {ftype; foffset}) ->
           let ArgType t = arg_type ftype in
           Ctypes_ffi_stubs.struct_type_set_argument bufspec i t)
         fields;
       Ctypes_ffi_stubs.complete_struct_type bufspec;
       ArgType (Ctypes_ffi_stubs.ffi_type_of_struct_type bufspec)

  (*
    call addr callspec
     (fun buffer ->
          write arg_1 buffer v_1
          write arg buffer v
          ...
          write arg_n buffer v_n)
     read_return_value
  *)
  let rec invoke : type a b m.
    string option ->
    a ccallspec ->
    (Ctypes_ptr.voidp -> (Obj.t * int) array -> Obj.t) list ->
    Ctypes_ffi_stubs.callspec ->
    (m, b fn) Ctypes_ptr.Fat.t ->
    a
    = fun name -> function
      | Call (check_errno, read_return_value) ->
        let name = match name with Some name -> name | None -> "" in
        fun writers callspec addr ->
          let r = ref [] in
          let v = Ctypes_ffi_stubs.call name addr callspec
              (fun buf arr -> List.iter (fun w -> r := w buf arr :: !r) writers)
              read_return_value
          in 
          Ctypes_memory_stubs.use_value r;
          v
      | WriteArg (write, ccallspec) ->
        let next = invoke name ccallspec in
        fun writers callspec addr v ->
          next (write v :: writers) callspec addr

  let add_argument : type a. Ctypes_ffi_stubs.callspec -> a typ -> int
    = fun callspec -> function
      | Void -> 0
      | ty   -> let ArgType ffitype = arg_type ty in
                Ctypes_ffi_stubs.add_argument callspec ffitype

  let prep_callspec callspec abi ty =
    let ArgType ctype = arg_type ty in
    Ctypes_ffi_stubs.prep_callspec callspec (abi_code abi) ctype

  let rec box_function : type a. abi -> a fn -> Ctypes_ffi_stubs.callspec -> a Ctypes_weak_ref.t ->
      Ctypes_ffi_stubs.boxedfn
    = fun abi fn callspec -> match fn with
      | Returns ty ->
        let () = prep_callspec callspec abi ty in
        let write_rv = Ctypes_memory.write ty in
        fun f ->
          let w = write_rv (Ctypes_weak_ref.get f) in
          Ctypes_ffi_stubs.Done ((fun p -> w (Ctypes_ptr.Fat.make
                                                ~managed:None ~reftyp:Void p)),
                          callspec)
      | Function (p, f) ->
        let _ = add_argument callspec p in
        let box = box_function abi f callspec in
        let read = Ctypes_memory.build p in
        fun f -> Ctypes_ffi_stubs.Fn (fun buf ->
          let f' =
            try Ctypes_weak_ref.get f (read (Ctypes_ptr.Fat.make
                                               ~managed:None ~reftyp:Void buf))
            with Ctypes_weak_ref.EmptyWeakReference ->
              raise Ctypes_ffi_stubs.CallToExpiredClosure
          in
          let v = box (Ctypes_weak_ref.make f') in
          let () = Gc.finalise (fun _ -> Ctypes_memory_stubs.use_value f') v in
          v)

  let rec write_arg : type a. a typ -> offset:int -> idx:int -> a ->
                  Ctypes_ptr.voidp -> (Obj.t * int) array -> Obj.t =
    let ocaml_arg elt_size =
      fun ~offset ~idx (OCamlRef (disp, obj, _)) dst mov ->
        mov.(idx) <- (Obj.repr obj, disp * elt_size);
        Obj.repr obj
    in function
    | OCaml String     -> ocaml_arg 1
    | OCaml Bytes      -> ocaml_arg 1
    | OCaml FloatArray -> ocaml_arg (Ctypes_primitives.sizeof Ctypes_primitive_types.Double)
    | View { write = w; ty } ->
      (fun ~offset ~idx v dst mov -> 
         let wv = w v in
         let wa = write_arg ty ~offset ~idx wv dst mov in
         Obj.repr (wv, wa))
    | ty -> (fun ~offset ~idx v dst mov -> 
        Ctypes_memory.write ty v
          (Ctypes_ptr.Fat.(add_bytes (make ~managed:None ~reftyp:Void dst) offset));
        Obj.repr v)

  (*
    callspec = allocate_callspec ()
    add_argument callspec arg1
    add_argument callspec arg2
    ...
    add_argument callspec argn
    prep_callspec callspec rettype
  *)
  let rec build_ccallspec : type a. abi:abi -> check_errno:bool -> ?idx:int -> a fn ->
    Ctypes_ffi_stubs.callspec -> a ccallspec
    = fun ~abi ~check_errno ?(idx=0) fn callspec -> match fn with
      | Returns t ->
        let () = prep_callspec callspec abi t in
        let b = Ctypes_memory.build t in
        Call (check_errno, (fun p -> b (Ctypes_ptr.Fat.make ~managed:None ~reftyp:Void p)))
      | Function (p, f) ->
        let offset = add_argument callspec p in
        let rest = build_ccallspec ~abi ~check_errno ~idx:(idx+1) f callspec in
        WriteArg (write_arg p ~offset ~idx, rest)

  let build_function ?name ~abi ~release_runtime_lock ~check_errno fn =
    let c = Ctypes_ffi_stubs.allocate_callspec ~check_errno
      ~runtime_lock:release_runtime_lock
      ~thread_registration:false
    in
    let e = build_ccallspec ~abi ~check_errno fn c in
    invoke name e [] c

  let funptr_of_rawptr fn raw_ptr =
    Static_funptr (Ctypes_ptr.Fat.make ~managed:None ~reftyp:fn raw_ptr)

  let function_of_pointer ?name ~abi ~check_errno ~release_runtime_lock fn =
    if release_runtime_lock && has_ocaml_argument fn
    then raise (Unsupported "Unsupported argument type when releasing runtime lock")
    else fun (Static_funptr p) ->
         build_function ?name ~abi ~check_errno ~release_runtime_lock fn p

  let pointer_of_function_internal ~abi ~acquire_runtime_lock ~thread_registration fn =
    let cs' = Ctypes_ffi_stubs.allocate_callspec
      ~check_errno:false
      ~runtime_lock:acquire_runtime_lock
      ~thread_registration
    in
    let cs = box_function abi fn cs' in
    fun f ->
      let boxed = cs (Ctypes_weak_ref.make f) in
      let id = Closure_properties.record (Obj.repr f) (Obj.repr boxed) in
      Ctypes_ffi_stubs.make_function_pointer cs' id

  let pointer_of_function ~abi ~acquire_runtime_lock ~thread_registration fn =
    let make_funptr = pointer_of_function_internal ~abi ~acquire_runtime_lock ~thread_registration fn in
    fun f ->
      let funptr = make_funptr f in
      (* TODO: use a more intelligent strategy for keeping function pointers
         associated with top-level functions alive (e.g. cache function
         pointer creation by (function, type), or possibly even just by
         function, since the C arity and types must be the same in each case.)
         See the note by [kept_alive_indefinitely].

         [dynamic_funptr_of_fun] allows for explicit life cycle management. *)
      let () = keep_alive funptr ~while_live:f in
      funptr_of_rawptr fn
        (Ctypes_ffi_stubs.raw_address_of_function_pointer funptr)

  type 'a funptr =
    { mutable gc_root : unit Ctypes.ptr
    ; fn : 'a Ctypes.static_funptr
    }

  let free_funptr t =
    if Ctypes.is_null t.gc_root then
       failwith "This funptr was previously freed"
    else (
      Ctypes.Root.release t.gc_root;
      t.gc_root <- Ctypes.null;
    )

  let report_leaked_funptr : (string -> unit) ref = ref (fun msg ->
    Printf.eprintf "%s\n%!" msg)

  let retain_funptr_root_to_avoid_segfaults_when_not_freed_correctly = ref []

  let create_funptr gc_root fn =
    let t = { gc_root = Ctypes.Root.create gc_root; fn } in
    Gc.finalise (fun t ->
      if Ctypes.is_null t.gc_root then
        ()
      else (
        retain_funptr_root_to_avoid_segfaults_when_not_freed_correctly :=
          t.gc_root :: !retain_funptr_root_to_avoid_segfaults_when_not_freed_correctly;
        t.gc_root <- Ctypes.null;
        !report_leaked_funptr
          "WARN: a ctypes function pointer was not explicitly released.\n\
           Releasing a function pointer or the associated OCaml closure while \n\
           the function pointer is still in use from C will cause segmentation faults.\n\
           Please call [Foreign.Funptr.free] explicitly when the funptr is no longer needed.\n\
           To avoid a segmentation fault we are preventing this funptr from\n\
           being garbage collected. Please use [Foreign.Funptr.free].\n%!")) t;
    t

  let funptr_of_fun ~abi ~acquire_runtime_lock ~thread_registration fn =
    let make_funptr = pointer_of_function_internal ~abi ~acquire_runtime_lock ~thread_registration fn in
    (fun f -> 
       let funptr = make_funptr f in
       create_funptr (f,funptr) (funptr_of_rawptr fn (Ctypes_ffi_stubs.raw_address_of_function_pointer funptr)))

  let funptr_of_static_funptr fp =
    create_funptr () fp

  let funptr_to_static_funptr t =
    if Ctypes.is_null t.gc_root then
      failwith "This funptr was previously freed"
    else t.fn
end
