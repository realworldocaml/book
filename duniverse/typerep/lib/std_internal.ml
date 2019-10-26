module Name_of = struct
  let typename_of_int =
    let module M = Typename.Make0(struct type t = int let name = "int" end) in
    M.typename_of_t

  let typename_of_int32 =
    let module M = Typename.Make0(struct type t = int32 let name = "int32" end) in
    M.typename_of_t

  let typename_of_int64 =
    let module M = Typename.Make0(struct type t = int64 let name = "int64" end) in
    M.typename_of_t

  let typename_of_nativeint =
    let module M = Typename.Make0(struct
      type t = nativeint
      let name = "nativeint"
    end) in
    M.typename_of_t

  let typename_of_char =
    let module M = Typename.Make0(struct type t = char let name = "char" end) in
    M.typename_of_t

  let typename_of_float =
    let module M = Typename.Make0(struct type t = float let name = "float" end) in
    M.typename_of_t

  let typename_of_string =
    let module M = Typename.Make0(struct type t = string let name = "string" end) in
    M.typename_of_t

  let typename_of_bytes =
    let module M = Typename.Make0(struct type t = bytes let name = "bytes" end) in
    M.typename_of_t

  let typename_of_bool =
    let module M = Typename.Make0(struct type t = bool let name = "bool" end) in
    M.typename_of_t

  let typename_of_unit =
    let module M = Typename.Make0(struct type t = unit let name = "unit" end) in
    M.typename_of_t

  module M_option = Typename.Make1(struct type 'a t = 'a option let name = "option" end)
  let typename_of_option = M_option.typename_of_t

  module M_list = Typename.Make1(struct type 'a t = 'a list let name = "list" end)
  let typename_of_list = M_list.typename_of_t

  module M_array = Typename.Make1(struct type 'a t = 'a array let name = "array" end)
  let typename_of_array = M_array.typename_of_t

  module M_lazy_t = Typename.Make1(struct type 'a t = 'a lazy_t let name = "lazy_t" end)
  let typename_of_lazy_t = M_lazy_t.typename_of_t

  module M_ref = Typename.Make1(struct type 'a t = 'a ref let name = "ref" end)
  let typename_of_ref = M_ref.typename_of_t

  module M_function = Typename.Make2(struct
    type ('a, 'b) t = 'a -> 'b
    let name = "function"
  end)
  let typename_of_function = M_function.typename_of_t

  type tuple0 = unit
  module M_tuple0 = Typename.Make0(struct type t = tuple0 let name = "tuple0" end)
  let typename_of_tuple0 = M_tuple0.typename_of_t

  module M_tuple2 = Typename.Make2(struct
    type ('a, 'b) t = 'a * 'b
    let name = "tuple2"
  end)
  let typename_of_tuple2 = M_tuple2.typename_of_t

  module M_tuple3 = Typename.Make3(struct
    type ('a, 'b, 'c) t = 'a * 'b * 'c
    let name = "tuple3"
  end)
  let typename_of_tuple3 = M_tuple3.typename_of_t

  module M_tuple4 = Typename.Make4(struct
    type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd
    let name = "tuple4"
  end)
  let typename_of_tuple4 = M_tuple4.typename_of_t

  module M_tuple5 = Typename.Make5(struct
    type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c *'d * 'e
    let name = "tuple5"
  end)
  let typename_of_tuple5 = M_tuple5.typename_of_t
end

module rec Typerep : sig

  type _ t =
    | Int        : int t
    | Int32      : int32 t
    | Int64      : int64 t
    | Nativeint  : nativeint t
    | Char       : char t
    | Float      : float t
    | String     : string t
    | Bytes      : bytes t
    | Bool       : bool t
    | Unit       : unit t
    | Option     : 'a t -> 'a option t
    | List       : 'a t -> 'a list t
    | Array      : 'a t -> 'a array t
    | Lazy       : 'a t -> 'a lazy_t t
    | Ref        : 'a t -> 'a ref t
    | Function   : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple      : 'a Typerep.Tuple.t -> 'a t
    | Record     : 'a Typerep.Record.t -> 'a t
    | Variant    : 'a Typerep.Variant.t -> 'a t
    | Named      : ('a Typerep.Named.t * 'a t lazy_t option) -> 'a t

  type packed = T : 'a t -> packed

  module Named : sig
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Typerep.t
      type t
      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type e val e : e Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
    val name : _ t -> string
  end

  module Tuple : sig
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose that way *)
    type _ t =
    | T2 : ('a Typerep.t * 'b Typerep.t)
      -> ('a * 'b) t
    | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
  end

  include Variant_and_record_intf.S with type 'a t := 'a Typerep.t

  val same : _ t -> _ t -> bool
  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val typename_of_t : 'a t -> 'a Typename.t
  val head : 'a t -> 'a t
end = struct

  type _ t =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Nativeint : nativeint t
    | Char : char t
    | Float : float t
    | String : string t
    | Bytes : bytes t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | List : 'a t -> 'a list t
    | Array : 'a t -> 'a array t
    | Lazy : 'a t -> 'a lazy_t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Typerep.Tuple.t -> 'a t
    | Record : 'a Typerep.Record.t -> 'a t
    | Variant : 'a Typerep.Variant.t -> 'a t
    | Named : ('a Typerep.Named.t * 'a t lazy_t option) -> 'a t

  type packed = T : 'a t -> packed

  module Named = struct
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Typerep.t
      type t
      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type e val e : e Typerep.t
      type t
      val typename_of_named :
        'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    let arity = function
      | T0 _ -> 0
      | T1 _ -> 1
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5

    let typename_of_t (type a) = function
      | T0 rep ->
        let module T = (val rep : T0 with type t = a) in
        T.typename_of_t
      | T1 rep ->
        let module T = (val rep : T1 with type t = a) in
        T.typename_of_t
      | T2 rep ->
        let module T = (val rep : T2 with type t = a) in
        T.typename_of_t
      | T3 rep ->
        let module T = (val rep : T3 with type t = a) in
        T.typename_of_t
      | T4 rep ->
        let module T = (val rep : T4 with type t = a) in
        T.typename_of_t
      | T5 rep ->
        let module T = (val rep : T5 with type t = a) in
        T.typename_of_t

    let name rep =
      Typename.Uid.name (Typename.uid (typename_of_t rep))
  end

  module Tuple = struct
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose this way *)
    type _ t =
    | T2 : ('a Typerep.t * 'b Typerep.t)
      -> ('a * 'b) t
    | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    let arity : type a. a t -> int = function
      | Typerep.Tuple.T2 _ -> 2
      | Typerep.Tuple.T3 _ -> 3
      | Typerep.Tuple.T4 _ -> 4
      | Typerep.Tuple.T5 _ -> 5

    let typename_of_t : type a. a t -> a Typename.t = function
      | T2 (a, b) ->
        Name_of.typename_of_tuple2
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
      | T3 (a, b, c) ->
        Name_of.typename_of_tuple3
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
      | T4 (a, b, c, d) ->
        Name_of.typename_of_tuple4
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
      | T5 (a, b, c, d, e) ->
        Name_of.typename_of_tuple5
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
          (Typerep.typename_of_t e)
  end

  include Variant_and_record_intf.M (struct type 'a rep = 'a t type 'a t = 'a rep end)

  let rec typename_of_t : type a. a t -> a Typename.t = function
    | Int        -> Name_of.typename_of_int
    | Int32      -> Name_of.typename_of_int32
    | Int64      -> Name_of.typename_of_int64
    | Nativeint  -> Name_of.typename_of_nativeint
    | Char       -> Name_of.typename_of_char
    | Float      -> Name_of.typename_of_float
    | String     -> Name_of.typename_of_string
    | Bytes      -> Name_of.typename_of_bytes
    | Bool       -> Name_of.typename_of_bool
    | Unit       -> Name_of.typename_of_unit

    | Option rep -> Name_of.typename_of_option (typename_of_t rep)
    | List rep   -> Name_of.typename_of_list   (typename_of_t rep)
    | Array rep  -> Name_of.typename_of_array  (typename_of_t rep)
    | Lazy rep   -> Name_of.typename_of_lazy_t (typename_of_t rep)
    | Ref rep    -> Name_of.typename_of_ref    (typename_of_t rep)

    | Function (dom, rng) ->
      Name_of.typename_of_function (typename_of_t dom) (typename_of_t rng)

    | Tuple rep -> Typerep.Tuple.typename_of_t rep

    | Record rep -> Typerep.Record.typename_of_t rep
    | Variant rep -> Typerep.Variant.typename_of_t rep

    | Named (name, _) -> Named.typename_of_t name
  ;;

  let rec same_witness : type a b. a t -> b t -> (a, b) Type_equal.t option = fun t1 t2 ->
    let module E = Type_equal in
    match t1, t2 with
    | Named (name1, r1), Named (name2, r2) -> begin
      match Typename.same_witness
        (Named.typename_of_t name1)
        (Named.typename_of_t name2) with
      | Some E.T as x -> x
      | None ->
        match r1, r2 with
        | Some (lazy t1), Some (lazy t2) -> same_witness t1 t2
        | Some (lazy t1), None           -> same_witness t1 t2
        | None, Some (lazy t2)           -> same_witness t1 t2
        | None, None -> None
    end
    | Named (_, r1), t2 -> begin
      match r1 with
      | Some (lazy t1) -> same_witness t1 t2
      | None -> None
    end
    | t1, Named (_, r2) -> begin
      match r2 with
      | Some (lazy t2) -> same_witness t1 t2
      | None -> None
    end
    | Int       , Int        -> Some E.T
    | Int32     , Int32      -> Some E.T
    | Int64     , Int64      -> Some E.T
    | Nativeint , Nativeint  -> Some E.T
    | Char      , Char       -> Some E.T
    | Float     , Float      -> Some E.T
    | String    , String     -> Some E.T
    | Bytes     , Bytes      -> Some E.T
    | Bool      , Bool       -> Some E.T
    | Unit      , Unit       -> Some E.T
    | Option r1, Option r2 -> begin
      match same_witness r1 r2 with
      | None     as x -> x
      | Some E.T as x -> x
    end
    | List r1, List r2 -> begin
      match same_witness r1 r2 with
      | None     as x -> x
      | Some E.T as x -> x
    end
    | Array r1, Array r2 -> begin
      match same_witness r1 r2 with
      | None     as x -> x
      | Some E.T as x -> x
    end
    | Lazy r1, Lazy r2 -> begin
      match same_witness r1 r2 with
      | None     as x -> x
      | Some E.T as x -> x
    end
    | Ref r1, Ref r2 -> begin
      match same_witness r1 r2 with
      | None     as x -> x
      | Some E.T as x -> x
    end
    | Function (dom1, rng1), Function (dom2, rng2) -> begin
      match same_witness dom1 dom2, same_witness rng1 rng2 with
      | Some E.T, Some E.T -> Some E.T
      | None, _ | _, None  -> None
    end
    | Tuple t1, Tuple t2 -> begin
      let module T = Typerep.Tuple in
      match t1, t2 with
      | T.T2 (a1, b1), T.T2 (a2, b2) -> begin
        match same_witness a1 a2, same_witness b1 b2 with
        | Some E.T, Some E.T -> Some E.T
        | None, _ | _, None  -> None
      end
      | T.T3 (a1, b1, c1), T.T3 (a2, b2, c2) -> begin
        match
          same_witness a1 a2,
          same_witness b1 b2,
          same_witness c1 c2
        with
        | Some E.T, Some E.T, Some E.T -> Some E.T
        | None, _, _
        | _, None, _
        | _, _, None
          -> None
      end
      | T.T4 (a1, b1, c1, d1), T.T4 (a2, b2, c2, d2) -> begin
        match
          same_witness a1 a2,
          same_witness b1 b2,
          same_witness c1 c2,
          same_witness d1 d2
        with
        | Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
        | None, _, _, _
        | _, None, _, _
        | _, _, None, _
        | _, _, _, None
          -> None
      end
      | T.T5 (a1, b1, c1, d1, e1), T.T5 (a2, b2, c2, d2, e2) -> begin
        match
          same_witness a1 a2,
          same_witness b1 b2,
          same_witness c1 c2,
          same_witness d1 d2,
          same_witness e1 e2
        with
        | Some E.T, Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
        | None, _, _, _, _
        | _, None, _, _, _
        | _, _, None, _, _
        | _, _, _, None, _
        | _, _, _, _, None
          -> None
      end
      | T.T2 _, _ -> None
      | T.T3 _, _ -> None
      | T.T4 _, _ -> None
      | T.T5 _, _ -> None
    end
    | Record r1, Record r2 ->
      Typename.same_witness
        (Typerep.Record.typename_of_t r1)
        (Typerep.Record.typename_of_t r2)
    | Variant r1, Variant r2 ->
      Typename.same_witness
        (Typerep.Variant.typename_of_t r1)
        (Typerep.Variant.typename_of_t r2)
    | Int, _         -> None
    | Int32, _       -> None
    | Int64, _       -> None
    | Nativeint, _   -> None
    | Char, _        -> None
    | Float, _       -> None
    | String, _      -> None
    | Bytes, _       -> None
    | Bool, _        -> None
    | Unit, _        -> None
    | Option _, _    -> None
    | List _, _      -> None
    | Array _, _     -> None
    | Lazy _, _      -> None
    | Ref _, _       -> None
    | Function _, _  -> None
    | Tuple _, _     -> None
    | Record _, _    -> None
    | Variant _, _   -> None
  ;;

  let same a b = same_witness a b <> None
  let same_witness_exn a b =
    match same_witness a b with
    | Some proof -> proof
    | None -> assert false

  let rec head = function
    | Typerep.Named (_, Some (lazy t)) -> head t
    | t -> t
end

let typerep_of_int        = Typerep.Int
let typerep_of_int32      = Typerep.Int32
let typerep_of_int64      = Typerep.Int64
let typerep_of_nativeint  = Typerep.Nativeint
let typerep_of_char       = Typerep.Char
let typerep_of_float      = Typerep.Float
let typerep_of_string     = Typerep.String
let typerep_of_bytes      = Typerep.Bytes
let typerep_of_bool       = Typerep.Bool
let typerep_of_unit       = Typerep.Unit

let typerep_of_option rep = Typerep.Option rep
let typerep_of_list   rep = Typerep.List   rep
let typerep_of_array  rep = Typerep.Array  rep
let typerep_of_lazy_t rep = Typerep.Lazy   rep
let typerep_of_ref    rep = Typerep.Ref    rep

let typerep_of_function dom rng = Typerep.Function (dom, rng)

let typerep_of_tuple0 = Typerep.Unit
let typerep_of_tuple2 a b = Typerep.Tuple (Typerep.Tuple.T2 (a, b))
let typerep_of_tuple3 a b c = Typerep.Tuple (Typerep.Tuple.T3 (a, b, c))
let typerep_of_tuple4 a b c d = Typerep.Tuple (Typerep.Tuple.T4 (a, b, c, d))
let typerep_of_tuple5 a b c d e = Typerep.Tuple (Typerep.Tuple.T5 (a, b, c, d, e))

include Name_of
let value_tuple0 = ()
