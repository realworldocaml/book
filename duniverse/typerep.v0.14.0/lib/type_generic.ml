open Std_internal

module Variant_and_record_intf = Variant_and_record_intf

module Helper (A : Variant_and_record_intf.S) (B : Variant_and_record_intf.S) = struct

  type map = { map : 'a. 'a A.t -> 'a B.t }

  let map_variant (type variant) { map } (variant : variant A.Variant.t) =
    let map_create = function
      | A.Tag.Args fct -> B.Tag_internal.Args fct
      | A.Tag.Const k -> B.Tag_internal.Const k
    in
    let map_tag tag =
      match tag with
      | A.Variant.Tag tag ->
        let label = A.Tag.label tag in
        let rep = map (A.Tag.traverse tag) in
        let arity = A.Tag.arity tag in
        let args_labels = A.Tag.args_labels tag in
        let index = A.Tag.index tag in
        let ocaml_repr = A.Tag.ocaml_repr tag in
        let tyid = A.Tag.tyid tag in
        let create = map_create (A.Tag.create tag) in
        B.Variant_internal.Tag (B.Tag.internal_use_only {
          B.Tag_internal.label; rep; arity; args_labels; index; ocaml_repr; tyid; create;
        })
    in
    let typename = A.Variant.typename_of_t variant in
    let polymorphic = A.Variant.is_polymorphic variant in
    let tags = Array.init (A.Variant.length variant)
      (fun index -> map_tag (A.Variant.tag variant index))
    in
    let value (a : variant) =
      match A.Variant.value variant a with
      | A.Variant.Value (atag, a) ->
        (fun (type args) (atag : (variant, args) A.Tag.t) (a : args) ->
          let (B.Variant_internal.Tag btag) = tags.(A.Tag.index atag) in
          (fun (type ex) (btag : (variant, ex) B.Tag.t) ->
            let Type_equal.T =
              Typename.same_witness_exn (A.Tag.tyid atag) (B.Tag.tyid btag)
            in
            let btag = (btag : (variant, args) B.Tag.t) in
            B.Variant_internal.Value (btag, a)
          ) btag
        ) atag a
    in
    B.Variant.internal_use_only {
      B.Variant_internal.typename; tags; polymorphic; value;
    }

  let map_record (type record) { map } (record : record A.Record.t) =
    let map_field field =
      match field with
      | A.Record.Field field ->
        let label = A.Field.label field in
        let rep = map (A.Field.traverse field) in
        let index = A.Field.index field in
        let is_mutable = A.Field.is_mutable field in
        let tyid = A.Field.tyid field in
        let get = A.Field.get field in
        B.Record_internal.Field (B.Field.internal_use_only {
          B.Field_internal.label; rep; index; is_mutable; tyid; get;
        })
    in
    let typename = A.Record.typename_of_t record in
    let has_double_array_tag = A.Record.has_double_array_tag record in
    let fields = Array.init (A.Record.length record)
      (fun index -> map_field (A.Record.field record index))
    in
    let create { B.Record_internal.get } =
      let get (type a) (afield : (_, a) A.Field.t) =
        match fields.(A.Field.index afield) with
        | B.Record_internal.Field bfield ->
          (fun (type ex) (bfield : (record, ex) B.Field.t) ->
            let Type_equal.T =
              Typename.same_witness_exn (A.Field.tyid afield) (B.Field.tyid bfield)
            in
            let bfield = (bfield : (record, a) B.Field.t) in
            get bfield
          ) bfield
      in
      A.Record.create record { A.Record.get }
    in
    B.Record.internal_use_only {
      B.Record_internal.typename; fields; has_double_array_tag; create;
    }
end

module type Named = sig
  type 'a computation
  module Context : sig
    type t
    val create : unit -> t
  end
  type 'a t
  val init : Context.t -> 'a Typename.t -> 'a t
  val get_wip_computation : 'a t -> 'a computation
  val set_final_computation : 'a t -> 'a computation -> 'a computation
  val share : _ Typerep.t -> bool
end

module type Computation = sig
  type 'a t

  include Variant_and_record_intf.S with type 'a t := 'a t

  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val nativeint : nativeint t
  val char : char t
  val float : float t
  val string : string t
  val bytes : bytes t
  val bool : bool t
  val unit : unit t
  val option : 'a t -> 'a option t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val lazy_t : 'a t -> 'a lazy_t t
  val ref_ : 'a t -> 'a ref t
  val function_ : 'a t -> 'b t -> ('a -> 'b) t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val record : 'a Record.t -> 'a t
  val variant : 'a Variant.t -> 'a t

  module Named : Named with type 'a computation := 'a t
end

(* special functor application for computation as closure of the form [a -> b] *)
module Make_named_for_closure (X : sig
  type 'a input
  type 'a output
  type 'a t = 'a input -> 'a output
end) = struct

  module Context = struct
    type t = unit
    let create = ignore
  end

  type 'a t = {
    runtime_dereference : 'a X.t;
    runtime_reference : 'a X.t ref;
    compiletime_dereference : 'a X.t option ref;
  }

  exception Undefined of string

  let init () name =
    let path = Typename.Uid.name (Typename.uid name) in
    let r = ref (fun _ -> raise (Undefined path)) in
    {
      runtime_dereference = (fun input -> !r input);
      runtime_reference = r;
      compiletime_dereference = ref None;
    }

  let get_wip_computation shared =
    match shared.compiletime_dereference.contents with
    | Some clos -> clos
    | None -> shared.runtime_dereference

  let set_final_computation shared computation =
    let compiletime_dereference = shared.compiletime_dereference in
    match compiletime_dereference.contents with
    | Some _ -> assert false
    | None ->
      if Base.phys_equal shared.runtime_dereference computation then assert false;
      compiletime_dereference := Some computation;
      shared.runtime_reference := computation;
      computation

  let share _ = true
end

module Ident = struct
  type t = {
    name : string;
    implements : Typename.Uid.t -> bool;
  }
  exception Broken_dependency of string
  let check_dependencies name required =
    match required with
    | [] -> (fun _ -> ())
    | _ ->
      (fun uid ->
        List.iter (fun { name = name'; implements } ->
          if not (implements uid) then begin
            (* something is wrong with the set up, this is an error during the
               initialization of the program, we rather fail with a human
               readable output *)
            let message =
              Printf.sprintf "Type_generic %S requires %S for uid %S\n"
                name name' (Typename.Uid.name uid)
            in
            prerr_endline message;
            raise (Broken_dependency message)
          end
        ) required)
end

(* Extending an existing generic *)
module type Extending = sig

  type 'a t
  type 'a computation = 'a t

  val ident : Ident.t

  (* generic_ident * typename or info *)
  exception Not_implemented of string * string

  module type S = sig
    type t
    include Typerepable.S with type t := t
    val compute : t computation
  end

  module type S1 = sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val compute : 'a computation -> 'a t computation
  end

  module type S2 = sig
    type ('a, 'b) t
    include Typerepable.S2 with type ('a, 'b) t := ('a, 'b) t
    val compute : 'a computation -> 'b computation -> ('a, 'b) t computation
  end

  module type S3 = sig
    type ('a, 'b, 'c) t
    include Typerepable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    val compute :
      'a computation
      -> 'b computation
      -> 'c computation
      -> ('a, 'b, 'c) t computation
  end

  module type S4 = sig
    type ('a, 'b, 'c, 'd) t
    include Typerepable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
    val compute :
      'a computation
      -> 'b computation
      -> 'c computation
      -> 'd computation
      -> ('a, 'b, 'c, 'd) t computation
  end

  module type S5 = sig
    type ('a, 'b, 'c, 'd, 'e) t
    include Typerepable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
    val compute :
      'a computation
      -> 'b computation
      -> 'c computation
      -> 'd computation
      -> 'e computation
      -> ('a, 'b, 'c, 'd, 'e) t computation
  end

  val register0 : (module S) -> unit
  val register1 : (module S1) -> unit
  val register2 : (module S2) -> unit
  val register3 : (module S3) -> unit
  val register4 : (module S4) -> unit
  val register5 : (module S5) -> unit

  (* special less scary type when the type has no parameters *)
  val register : 'a Typerep.t -> 'a computation -> unit

  (*
    Essentially because we cannot talk about a variable of kind * -> k
    val register1 : 'a 't Typerep.t -> ('a computation -> 'a 't computation) -> unit
    ...
  *)
end

(* Implementing a new generic *)
module type S_implementation = sig

  include Extending

  (* raise using the current ident *)
  val raise_not_implemented : string -> 'a

  type implementation = {
    generic : 'a. 'a Typerep.t -> 'a computation;
  }

  (*
    Standard case, find a extended_implementation, or look in the content
  *)
  val _using_extended_implementation :
    implementation
    -> 'a Typerep.Named.t
    -> 'a Typerep.t lazy_t option
    -> 'a computation

  (*
    This function allows you more control on what you want to do
  *)
  val find_extended_implementation :
    implementation -> 'a Typerep.Named.t -> 'a computation option
end

module type S = sig
  include Extending
  val of_typerep : 'a Typerep.t -> [ `generic of 'a computation ]
  module Computation : Computation with type 'a t = 'a t
end

module Make_S_implementation(X : sig
  type 'a t
  val name : string
  val required : Ident.t list
end) : S_implementation with type 'a t = 'a X.t = struct
  type 'a t = 'a X.t
  type 'a computation = 'a t

  include Type_generic_intf.M(struct type 'a t = 'a computation end)

  (* we do not use core since we are earlier in the dependencies graph *)
  module Uid_table = struct
    include Hashtbl.Make(Typename.Uid)
    let find table key =
      if Lazy.is_val table then
        let table = Lazy.force table in
        try Some (find table key) with Base.Not_found_s _ | Caml.Not_found -> None
      else None
    let check_dependencies = Ident.check_dependencies X.name X.required
    let replace table key value =
      check_dependencies key;
      replace (Lazy.force table) key value
    let mem table key =
      if Lazy.is_val table then
        let table = Lazy.force table in
        mem table key
      else false
  end

  let size = 256
  let table0 = lazy (Uid_table.create size)
  let table1 = lazy (Uid_table.create size)
  let table2 = lazy (Uid_table.create size)
  let table3 = lazy (Uid_table.create size)
  let table4 = lazy (Uid_table.create size)
  let table5 = lazy (Uid_table.create size)

  let is_registered uid =
    Uid_table.mem table0 uid
    || Uid_table.mem table1 uid
    || Uid_table.mem table2 uid
    || Uid_table.mem table3 uid
    || Uid_table.mem table4 uid
    || Uid_table.mem table5 uid

  let ident = { Ident.
    name = X.name;
    implements = is_registered;
  }

  module Find0(T : Typerep.Named.T0) : sig
    val compute : unit -> T.named computation option
  end = struct
    let compute () =
      match Uid_table.find table0 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S = (val rep : S) in
        let witness = Typename.same_witness_exn S.typename_of_t T.typename_of_named in
        let module L = Type_equal.Lift(struct
          type 'a t = 'a computation
        end) in
        Some (Type_equal.conv (L.lift witness) S.compute)
  end

  module Find1(T : Typerep.Named.T1) : sig
    val compute : unit -> (T.a computation -> T.a T.named computation) option
  end = struct
    let compute () =
      match Uid_table.find table1 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S1 = (val rep : S1) in
        let module Conv = Typename.Same_witness_exn_1(S1)(struct
          type 'a t = 'a T.named
          let typename_of_t = T.typename_of_named
        end) in
        let module L = Type_equal.Lift(struct
          type 'a t = T.a computation -> 'a computation
        end) in
        Some (Type_equal.conv (L.lift Conv.(witness.eq)) S1.compute)
  end

  module Find2(T : Typerep.Named.T2) : sig
    val compute : unit
      -> (T.a computation
          -> T.b computation
          -> (T.a, T.b) T.named computation) option
  end = struct
    let compute () =
      match Uid_table.find table2 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S2 = (val rep : S2) in
        let module Conv = Typename.Same_witness_exn_2(S2)(struct
          type ('a, 'b) t = ('a, 'b) T.named
          let typename_of_t = T.typename_of_named
        end) in
        let module L = Type_equal.Lift(struct
          type 'a t =
            T.a computation
            -> T.b computation
            -> 'a computation
        end) in
        Some (Type_equal.conv (L.lift Conv.(witness.eq)) S2.compute)
  end

  module Find3(T : Typerep.Named.T3) : sig
    val compute : unit
      -> (T.a computation
          -> T.b computation
          -> T.c computation
          -> (T.a, T.b, T.c) T.named computation) option
  end = struct
    let compute () =
      match Uid_table.find table3 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S3 = (val rep : S3) in
        let module Conv = Typename.Same_witness_exn_3(S3)(struct
          type ('a, 'b, 'c) t = ('a, 'b, 'c) T.named
          let typename_of_t = T.typename_of_named
        end) in
        let module L = Type_equal.Lift(struct
          type 'a t =
            T.a computation
            -> T.b computation
            -> T.c computation
            -> 'a computation
        end) in
        Some (Type_equal.conv (L.lift Conv.(witness.eq)) S3.compute)
  end

  module Find4(T : Typerep.Named.T4) : sig
    val compute : unit
      -> (T.a computation
          -> T.b computation
          -> T.c computation
          -> T.d computation
          -> (T.a, T.b, T.c, T.d) T.named computation) option
  end = struct
    let compute () =
      match Uid_table.find table4 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S4 = (val rep : S4) in
        let module Conv = Typename.Same_witness_exn_4(S4)(struct
          type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) T.named
          let typename_of_t = T.typename_of_named
        end) in
        let module L = Type_equal.Lift(struct
          type 'a t =
            T.a computation
            -> T.b computation
            -> T.c computation
            -> T.d computation
            -> 'a computation
        end) in
        Some (Type_equal.conv (L.lift Conv.(witness.eq)) S4.compute)
  end

  module Find5(T : Typerep.Named.T5) : sig
    val compute : unit
      -> (T.a computation
          -> T.b computation
          -> T.c computation
          -> T.d computation
          -> T.e computation
          -> (T.a, T.b, T.c, T.d, T.e) T.named computation) option
  end = struct
    let compute () =
      match Uid_table.find table5 (Typename.uid T.typename_of_t) with
      | None -> None
      | Some rep ->
        let module S5 = (val rep : S5) in
        let module Conv = Typename.Same_witness_exn_5(S5)(struct
          type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) T.named
          let typename_of_t = T.typename_of_named
        end) in
        let module L = Type_equal.Lift(struct
          type 'a t =
            T.a computation
            -> T.b computation
            -> T.c computation
            -> T.d computation
            -> T.e computation
            -> 'a computation
        end) in
        Some (Type_equal.conv (L.lift Conv.(witness.eq)) S5.compute)
  end

  let unit = Typename.static

  let register0 compute =
    let module S = (val compute : S) in
    let uid = Typename.uid S.typename_of_t in
    Uid_table.replace table0 uid compute

  let register1 compute =
    let module S1 = (val compute : S1) in
    let uid = Typename.uid (S1.typename_of_t unit) in
    Uid_table.replace table1 uid compute

  let register2 compute =
    let module S2 = (val compute : S2) in
    let uid = Typename.uid (S2.typename_of_t unit unit) in
    Uid_table.replace table2 uid compute

  let register3 compute =
    let module S3 = (val compute : S3) in
    let uid = Typename.uid (S3.typename_of_t unit unit unit) in
    Uid_table.replace table3 uid compute

  let register4 compute =
    let module S4 = (val compute : S4) in
    let uid = Typename.uid (S4.typename_of_t unit unit unit unit) in
    Uid_table.replace table4 uid compute

  let register5 compute =
    let module S5 = (val compute : S5) in
    let uid = Typename.uid (S5.typename_of_t unit unit unit unit unit) in
    Uid_table.replace table5 uid compute

  let register (type a) typerep_of_a compute =
    let module S = struct
      type t = a
      let typename_of_t = Typerep.typename_of_t typerep_of_a
      let typerep_of_t = typerep_of_a
      let compute = compute
    end in
    register0 (module S : S)

  (* IMPLEMENTATION *)

  type implementation = {
    generic : 'a. 'a Typerep.t -> 'a computation;
  }

  let find_extended_implementation (type a) aux = function
    | Typerep.Named.T0 rep -> begin
      let module T = (val rep : Typerep.Named.T0 with type t = a) in
      let module Custom = Find0(T) in
      match Custom.compute () with
      | Some custom ->
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

    | Typerep.Named.T1 rep -> begin
      let module T = (val rep : Typerep.Named.T1 with type t = a) in
      let module Custom = Find1(T) in
      match Custom.compute () with
      | Some custom ->
        let custom = (custom (aux.generic T.a) : T.a T.named computation) in
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

    | Typerep.Named.T2 rep -> begin
      let module T = (val rep : Typerep.Named.T2 with type t = a) in
      let module Custom = Find2(T) in
      match Custom.compute () with
      | Some custom ->
        let custom =
          (custom
             (aux.generic T.a)
             (aux.generic T.b)
             : (T.a, T.b) T.named computation) in
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

    | Typerep.Named.T3 rep -> begin
      let module T = (val rep : Typerep.Named.T3 with type t = a) in
      let module Custom = Find3(T) in
      match Custom.compute () with
      | Some custom ->
        let custom =
          (custom
             (aux.generic T.a)
             (aux.generic T.b)
             (aux.generic T.c)
             : (T.a, T.b, T.c) T.named computation) in
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

    | Typerep.Named.T4 rep -> begin
      let module T = (val rep : Typerep.Named.T4 with type t = a) in
      let module Custom = Find4(T) in
      match Custom.compute () with
      | Some custom ->
        let custom =
          (custom
             (aux.generic T.a)
             (aux.generic T.b)
             (aux.generic T.c)
             (aux.generic T.d)
             : (T.a, T.b, T.c, T.d) T.named computation) in
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

    | Typerep.Named.T5 rep -> begin
      let module T = (val rep : Typerep.Named.T5 with type t = a) in
      let module Custom = Find5(T) in
      match Custom.compute () with
      | Some custom ->
        let custom =
          (custom
             (aux.generic T.a)
             (aux.generic T.b)
             (aux.generic T.c)
             (aux.generic T.d)
             (aux.generic T.e)
             : (T.a, T.b, T.c, T.d, T.e) T.named computation) in
        let Type_equal.T = T.witness in
        Some (custom : a computation)
      | None -> None
    end

  exception Not_implemented of string * string

  let raise_not_implemented string = raise (Not_implemented (X.name, string))

  let _using_extended_implementation aux rep content =
    match find_extended_implementation aux rep with
    | Some computation -> computation
    | None -> begin
      match content with
      | Some (lazy content) -> aux.generic content
      | None ->
        let typename = Typerep.Named.typename_of_t rep in
        let name = Typename.Uid.name (Typename.uid typename) in
        raise_not_implemented name
    end
end

module Key_table = Hashtbl.Make(Typename.Key)

module Make(X : sig
  type 'a t
  val name : string
  val required : Ident.t list
  include Computation with type 'a t := 'a t
end) = struct

  module Computation = X

  include Make_S_implementation(X)

  module Memo = Typename.Table(struct type 'a t = 'a X.Named.t end)

  module Helper = Helper(Typerep)(Computation)

  let of_typerep rep =
    let context = X.Named.Context.create () in
    let memo_table = Memo.create 32 in
    let rec of_typerep : type a. a Typerep.t -> a t = function
      | Typerep.Int         -> X.int
      | Typerep.Int32       -> X.int32
      | Typerep.Int64       -> X.int64
      | Typerep.Nativeint   -> X.nativeint
      | Typerep.Char        -> X.char
      | Typerep.Float       -> X.float
      | Typerep.String      -> X.string
      | Typerep.Bytes       -> X.bytes
      | Typerep.Bool        -> X.bool
      | Typerep.Unit        -> X.unit
      | Typerep.Option rep  -> X.option (of_typerep rep)
      | Typerep.List rep    -> X.list   (of_typerep rep)
      | Typerep.Array rep   -> X.array  (of_typerep rep)
      | Typerep.Lazy rep    -> X.lazy_t (of_typerep rep)
      | Typerep.Ref rep     -> X.ref_   (of_typerep rep)
      | Typerep.Function (dom, rng) ->
        X.function_ (of_typerep dom) (of_typerep rng)
      | Typerep.Tuple tuple -> begin
        (* do NOT write [X.tuple2 (of_typerep a) (of_typerep b)]
           because of_typerep can contain a side effect and [a] should be executed
           before [b] *)
        match tuple with
        | Typerep.Tuple.T2 (a, b) ->
          let ra = of_typerep a in
          let rb = of_typerep b in
          X.tuple2 ra rb
        | Typerep.Tuple.T3 (a, b, c) ->
          let ra = of_typerep a in
          let rb = of_typerep b in
          let rc = of_typerep c in
          X.tuple3 ra rb rc
        | Typerep.Tuple.T4 (a, b, c, d) ->
          let ra = of_typerep a in
          let rb = of_typerep b in
          let rc = of_typerep c in
          let rd = of_typerep d in
          X.tuple4 ra rb rc rd
        | Typerep.Tuple.T5 (a, b, c, d, e) ->
          let ra = of_typerep a in
          let rb = of_typerep b in
          let rc = of_typerep c in
          let rd = of_typerep d in
          let re = of_typerep e in
          X.tuple5 ra rb rc rd re
      end
      | Typerep.Record record ->
        X.record (Helper.map_record { Helper.map = of_typerep } record)
      | Typerep.Variant variant ->
        X.variant (Helper.map_variant { Helper.map = of_typerep } variant)
      | Typerep.Named (named, content) -> begin
        let typename = Typerep.Named.typename_of_t named in
        match Memo.find memo_table typename with
        | Some shared ->
          X.Named.get_wip_computation shared
        | None -> begin
          match find_extended_implementation { generic = of_typerep } named with
          | Some computation -> computation
          | None -> begin
            match content with
            | None ->
              let name = Typename.Uid.name (Typename.uid typename) in
              raise_not_implemented name
            | Some (lazy content) ->
              if X.Named.share content
              then
                let shared = X.Named.init context typename in
                Memo.set memo_table typename shared;
                let computation = of_typerep content in
                X.Named.set_final_computation shared computation
              else
                of_typerep content
          end
        end
      end
    in
    let computation = of_typerep rep in
    `generic computation
end
