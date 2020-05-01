open! Base

module Location : sig
  include Identifiable.S
end = struct
  include String
end

module Uuid : sig
  include Identifiable.S
end = struct
  include String
end

let eval_fail loc fmt =
  Printf.ksprintf (fun s -> failwith (Printf.sprintf !"%{Location}: %s" loc s)) fmt
;;

let equal_option equal a b =
  match a, b with
  | Some _, None | None, Some _ -> false
  | None, None -> true
  | Some x, Some y -> equal x y
;;

module Sorted_table : sig
  type 'a t [@@deriving compare, sexp]

  val create : Location.t -> eq:('a -> 'a -> bool) -> (string * 'a) list -> 'a t
  val expose : 'a t -> (string * 'a) list
  val map : 'a t -> f:('a -> 'b) -> 'b t
end = struct
  type 'a t = { sorted : (string * 'a) list } [@@deriving compare, sexp]

  let merge_check_adjacent_dups
    :  eq:('a -> 'a -> bool) -> (string * 'a) list
      -> [ `Ok of (string * 'a) list | `Mismatch of string ]
    =
    fun ~eq ->
    let rec loop acc ~last_key ~last_value = function
      | [] -> `Ok (List.rev acc)
      | (key, value) :: xs ->
        if String.(last_key = key)
        then
          if eq last_value value
          then loop acc ~last_key ~last_value xs
          else `Mismatch key
        else loop ((key, value) :: acc) ~last_key:key ~last_value:value xs
    in
    function
    | [] -> `Ok []
    | (key, value) :: xs -> loop [ key, value ] ~last_key:key ~last_value:value xs
  ;;

  let create loc ~eq xs =
    let sorted = List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2) xs in
    match merge_check_adjacent_dups ~eq sorted with
    | `Ok sorted -> { sorted }
    | `Mismatch s ->
      eval_fail loc "Different shapes for duplicated polymorphic constructor: `%s" s ()
  ;;

  let expose t = t.sorted
  let map t ~f = { sorted = List.map t.sorted ~f:(fun (k, v) -> k, f v) }
end

module Digest : sig
  type t = Md5_lib.t [@@deriving compare, sexp]

  val to_md5 : t -> Md5_lib.t
  val of_md5 : Md5_lib.t -> t
  val to_hex : t -> string
  val constructor : string -> t list -> t
  val list : t list -> t
  val pair : t -> t -> t
  val string : string -> t
  val uuid : Uuid.t -> t
  val int : int -> t
  val option : t option -> t
end = struct
  include Md5_lib

  let to_md5 t = t
  let of_md5 t = t
  let sexp_of_t t = t |> to_hex |> sexp_of_string
  let t_of_sexp s = s |> string_of_sexp |> of_hex_exn
  let uuid u = string (Uuid.to_string u)
  let int x = string (Int.to_string x)
  let pair x y = string (to_binary x ^ to_binary y)
  let list l = string (String.concat ~sep:"" (List.map ~f:to_binary l))
  let constructor s l = string (s ^ to_binary (list l))

  let option = function
    | None -> constructor "none" []
    | Some x -> constructor "some" [ x ]
  ;;
end

module Canonical_exp_constructor = struct
  (* ['a t] is a non-recursive type, used to represent 1-layer of expression.  The
     recursive knot is tied below in [Canonical_full.Exp.t]. *)
  type 'a t =
    | Annotate of Uuid.t * 'a
    | Base of Uuid.t * 'a list
    | Tuple of 'a list
    | Record of (string * 'a) list
    | Variant of (string * 'a list) list
    (* Polymorphic variants are insensitive to the order the constructors are listed *)
    | Poly_variant of 'a option Sorted_table.t
    (* Left-hand-side of [Application] is a potentially recursive definition: it
       can refer to itself using [Rec_app (i, _)] where [i] is the depth of this
       application node (how many application nodes are above it).
       It also has its own scope of type variables so it can not refer to type variables
       of the enclosing scope.
    *)
    | Application of 'a * 'a list
    | Rec_app of int * 'a list
    | Var of int
  [@@deriving sexp, compare]

  let map x ~f =
    match x with
    | Annotate (u, x) -> Annotate (u, f x)
    | Base (s, xs) -> Base (s, List.map ~f xs)
    | Tuple xs -> Tuple (List.map ~f xs)
    | Record l -> Record (List.map l ~f:(fun (s, x) -> s, f x))
    | Variant l -> Variant (List.map l ~f:(fun (s, xs) -> s, List.map ~f xs))
    | Poly_variant t -> Poly_variant (Sorted_table.map t ~f:(Option.map ~f))
    | Application (x, l) -> Application (f x, List.map ~f l)
    | Rec_app (t, l) -> Rec_app (t, List.map ~f l)
    | Var v -> Var v
  ;;

  let to_string t = Sexp.to_string (sexp_of_t (fun _ -> Atom "...") t)
end

module Create_digest : sig
  (* Digest various expression forms *)

  val digest_layer : Digest.t Canonical_exp_constructor.t -> Digest.t
end = struct
  let digest_layer = function
    | Canonical_exp_constructor.Annotate (u, x) ->
      Digest.constructor "annotate" [ Digest.uuid u; x ]
    | Base (u, l) -> Digest.constructor "base" [ Digest.uuid u; Digest.list l ]
    | Tuple l -> Digest.constructor "tuple" [ Digest.list l ]
    | Record l ->
      Digest.constructor
        "record"
        [ Digest.list (List.map l ~f:(fun (s, t) -> Digest.pair (Digest.string s) t)) ]
    | Variant l ->
      Digest.constructor
        "variant"
        [ Digest.list
            (List.map l ~f:(fun (s, l) -> Digest.pair (Digest.string s) (Digest.list l)))
        ]
    | Poly_variant table ->
      Digest.constructor
        "poly_variant"
        [ Digest.list
            (List.map (Sorted_table.expose table) ~f:(fun (x, y) ->
               Digest.pair (Digest.string x) (Digest.option y)))
        ]
    | Application (x, l) -> Digest.constructor "application" [ x; Digest.list l ]
    | Rec_app (n, l) -> Digest.constructor "rec_app" [ Digest.int n; Digest.list l ]
    | Var n -> Digest.constructor "var" [ Digest.int n ]
  ;;
end

module Visibility = struct
  type visible = Visible
  type opaque = Opaque

  let _ = Visible
  let _ = Opaque
end

module type Canonical = sig
  type t

  val to_digest : t -> Digest.t

  module Exp1 : sig
    type _ t

    val var : int -> _ t
    val recurse : int -> _ t list -> _ t
    val apply : 'a t -> 'a t list -> _ t
    val opaque : _ t -> Visibility.opaque t

    val get_poly_variant
      :  Visibility.visible t
      -> (Visibility.opaque t option Sorted_table.t, string) Result.t
  end

  module Def : sig
    type t = Visibility.visible Exp1.t
  end

  module Create : sig
    val annotate : Uuid.t -> _ Exp1.t -> _ Exp1.t
    val basetype : Uuid.t -> _ Exp1.t list -> _ Exp1.t
    val tuple : _ Exp1.t list -> _ Exp1.t
    val poly_variant : Location.t -> (string * _ Exp1.t option) list -> _ Exp1.t
    val var : int -> _ Exp1.t
    val recurse : int -> _ Exp1.t list -> _ Exp1.t
    val apply : 'a Exp1.t -> 'a Exp1.t list -> _ Exp1.t
    val define : Visibility.visible Exp1.t -> Def.t
    val record : (string * _ Exp1.t) list -> _ Exp1.t
    val variant : (string * _ Exp1.t list) list -> _ Exp1.t
    val create : _ Exp1.t -> t
  end
end

module Canonical_digest : Canonical = struct
  type t = Canonical of Digest.t

  let to_digest (Canonical x) = x

  module CD = Create_digest

  module Exp1 = struct
    type opaque = Digest.t

    type 'a t =
      | Poly_variant of opaque option Sorted_table.t
      | Non_poly_variant of (string * opaque)
      | Opaque : opaque -> Visibility.opaque t

    let to_digest (type a) (x : a t) =
      match x with
      | Opaque x -> x
      | Non_poly_variant (_, x) -> x
      | Poly_variant x -> CD.digest_layer (Poly_variant x)
    ;;

    let equal (type a) (x : a t) (y : a t) =
      Digest.compare (to_digest x) (to_digest y) = 0
    ;;

    let opaque x = Opaque (to_digest x)

    let create x =
      let x = Canonical_exp_constructor.map ~f:to_digest x in
      let desc = Canonical_exp_constructor.to_string x in
      match x with
      | Canonical_exp_constructor.Poly_variant l -> Poly_variant l
      | Base _ -> Non_poly_variant (desc, CD.digest_layer x)
      | Annotate _ ->
        (* It's unsafe to use deriving bin_io when inheriting from a polymorphic variant
           that has a custom bin_io.  If we forbid that, we can happily reject here
           anything that's annotated. *)
        Non_poly_variant (desc, CD.digest_layer x)
      | Application _ ->
        (* Application can really be a poly-variant you can inherit from!  But it's a
           rare situation that mostly (only?) arises with inheritance from recursive
           polymorpic variants, which we've not seen anywhere yet.  So we reject it. *)
        Non_poly_variant (desc, CD.digest_layer x)
      | Rec_app _ ->
        (* You can only get the [Rec_app] constructor for type-references within the
           mutual group being defined. References which
           follow after the current group will always be [Application]s.

           And since ocaml rejects references in `inheritance' position to types within
           the current group (see example) with:

           Error: The type constructor t
           is not yet completely defined

           then its ok to say that a rec-app is something that can't be inherited from and
           return [Non_poly_variant].

           And unlike the [Application] case, it should never be possible to see
           an error message with the [desc] = [Rec_app].

           Example: [type t = [`a of [ | t] ]]
           Here, [| t] would be an example of inheritance from a Rec_app, which
           is rejected by the compiler.
        *)
        Non_poly_variant (desc, CD.digest_layer x)
      | Var _ | Tuple _ | Record _ | Variant _ ->
        Non_poly_variant (desc, CD.digest_layer x)
    ;;

    let var x = create (Var x)
    let apply def l = create (Application (def, l))
    let recurse tid l = create (Rec_app (tid, l))

    let get_poly_variant (x : Visibility.visible t) =
      match x with
      | Non_poly_variant (desc, _) -> Error desc
      | Poly_variant l -> Ok (Sorted_table.map ~f:(Option.map ~f:(fun x -> Opaque x)) l)
    ;;
  end

  module Def = struct
    type t = Visibility.visible Exp1.t
  end

  module Create = struct
    let annotate u x = Exp1.create (Annotate (u, x))
    let basetype u l = Exp1.create (Base (u, l))
    let tuple l = Exp1.create (Tuple l)

    let poly_variant loc l =
      Exp1.create
        (Poly_variant (Sorted_table.create loc ~eq:(equal_option Exp1.equal) l))
    ;;

    let var x = Exp1.create (Var x)
    let apply x l = Exp1.create (Application (x, l))
    let recurse t l = Exp1.create (Rec_app (t, l))
    let define x = x
    let record l = Exp1.create (Record l)
    let variant l = Exp1.create (Variant l)
    let create e = Canonical (Exp1.to_digest e)
  end
end

module Canonical_full : sig
  type t [@@deriving compare, sexp]

  include Canonical with type t := t

  val to_string_hum : t -> string
end = struct
  module CD = Create_digest

  module Exp1 = struct
    type t0 = Exp of t0 Canonical_exp_constructor.t [@@deriving compare, sexp]

    let equal_t0 x y = compare_t0 x y = 0

    type 'a t = t0 [@@deriving compare, sexp]

    let var x = Exp (Canonical_exp_constructor.Var x)
    let apply d xs = Exp (Canonical_exp_constructor.Application (d, xs))
    let recurse r xs = Exp (Canonical_exp_constructor.Rec_app (r, xs))

    let poly_variant loc xs =
      Exp
        (Canonical_exp_constructor.Poly_variant
           (Sorted_table.create loc ~eq:(equal_option equal_t0) xs))
    ;;

    let get_poly_variant = function
      | Exp (Poly_variant tab) -> Ok tab
      | Exp cc -> Error (Canonical_exp_constructor.to_string cc)
    ;;

    let opaque t = t

    let rec to_digest = function
      | Exp e -> CD.digest_layer (Canonical_exp_constructor.map ~f:to_digest e)
    ;;
  end

  module Def = struct
    (* A [Def.t] is an expression which may be applied *)
    type t = Exp1.t0 [@@deriving compare, sexp]
  end

  (* A canonical shape [t] is an [Exp1.t]. *)
  type t = Exp1.t0 [@@deriving compare, sexp]

  let to_digest e = Exp1.to_digest e

  module Create = struct
    let annotate u x = Exp1.Exp (Annotate (u, x))
    let basetype u xs = Exp1.Exp (Base (u, xs))
    let tuple xs = Exp1.Exp (Tuple xs)
    let poly_variant loc xs = Exp1.poly_variant loc xs
    let var n = Exp1.Exp (Var n)
    let recurse r xs = Exp1.recurse r xs
    let apply d xs = Exp1.apply d xs
    let define x = x
    let record xs = Exp1.Exp (Record xs)
    let variant xs = Exp1.Exp (Variant xs)
    let create exp = exp
  end

  let to_string_hum t = Sexp.to_string_hum (sexp_of_t t)
end

module Tid : sig
  include Identifiable.S
end = struct
  include String
end

module Vid : sig
  include Identifiable.S
end = struct
  include String
end

module Gid : sig
  (* unique group-id, used as key for Tenv below *)
  type t [@@deriving compare, sexp_of]

  val create : unit -> t
end = struct
  type t = int [@@deriving compare, sexp_of]

  let r = ref 0

  let create () =
    let u = !r in
    r := 1 + u;
    u
  ;;
end

module Expression = struct
  type 't poly_constr =
    [ `Constr of string * 't option
    | `Inherit of Location.t * 't
    ]
  [@@deriving compare, sexp_of]

  module Group : sig
    type 'a t [@@deriving compare, sexp_of]

    val create : Location.t -> (Tid.t * Vid.t list * 'a) list -> 'a t
    val id : 'a t -> Gid.t
    val lookup : 'a t -> Tid.t -> Vid.t list * 'a
  end = struct
    type 'a t =
      { gid : Gid.t
      ; loc : Location.t
      ; members : (Tid.t * (Vid.t list * 'a)) list
      }
    [@@deriving compare, sexp_of]

    let create loc trips =
      let gid = Gid.create () in
      let members = List.map trips ~f:(fun (x, vs, t) -> x, (vs, t)) in
      { gid; loc; members }
    ;;

    let id g = g.gid

    let lookup g tid =
      match List.Assoc.find g.members ~equal:Tid.( = ) tid with
      | Some scheme -> scheme
      | None ->
        eval_fail
          g.loc
          !"impossible: lookup_group, unbound type-identifier: %{Tid}"
          tid
          ()
    ;;
  end

  type t =
    | Annotate of Uuid.t * t
    | Base of Uuid.t * t list
    | Record of (string * t) list
    | Variant of (string * t list) list
    | Tuple of t list
    | Poly_variant of (Location.t * t poly_constr list)
    | Var of (Location.t * Vid.t)
    | Rec_app of Tid.t * t list
    | Top_app of t Group.t * Tid.t * t list
  [@@deriving variants, sexp_of]

  type group = t Group.t

  let group = Group.create

  type poly_variant_row = t poly_constr

  let constr s t = `Constr (s, t)
  let inherit_ loc t = `Inherit (loc, t)
  let var loc t = Var (loc, t)
  let poly_variant loc xs = Poly_variant (loc, xs)
  let basetype = base

  (* "VR" stands for "variant or record" *)

  let is_cyclic_0 ~(via_VR : bool) : group -> Tid.t -> bool =
    fun group tid ->
    let set = ref [] in
    let visited tid = List.mem !set tid ~equal:Tid.equal in
    let add tid = set := tid :: !set in
    let rec trav = function
      (* We look for cycles by traversing the structure of type-expressions *)
      | Annotate (_, t) -> trav t
      | Base (_, ts) | Tuple ts | Top_app (_, _, ts) -> List.iter ts ~f:trav
      (* ..including poly-variants *)
      | Poly_variant (_, cs) ->
        List.iter cs ~f:(function
          | `Constr (_, None) -> ()
          | `Constr (_, Some t) -> trav t
          | `Inherit (_loc, t) -> trav t)
      (* .. and records & (normal) variants *)
      | Record xs -> if via_VR then List.iter xs ~f:(fun (_, t) -> trav t) else ()
      | Variant xs ->
        if via_VR then List.iter xs ~f:(fun (_, ts) -> List.iter ~f:trav ts) else ()
      (* We dont follow type-vars *)
      | Var _ -> ()
      (* traverse (recursive) type-apps when first encountered *)
      | Rec_app (tid, ts) ->
        if visited tid
        then ()
        else (
          add tid;
          trav_tid tid);
        List.iter ts ~f:trav
    and trav_tid tid =
      let _, body = Group.lookup group tid in
      trav body
    in
    trav_tid tid;
    let res = visited tid in
    (*let _ss = String.concat ~sep:"," (List.map (!set) ~f:(sprintf !"%{Tid}")) in*)
    (*Printf.printf !"is_cylic: %{Tid} --> (%s) %b  -- %s%!" tid _ss res (Group.loc group);*)
    res
  ;;

  let is_cyclic = is_cyclic_0 ~via_VR:true
  let is_cyclic_with_no_intervening_VR = is_cyclic_0 ~via_VR:false
end

include Expression

module Evaluation (Canonical : Canonical) = struct
  (* [Venv.t]
     Environment for resolving type-vars *)
  module Venv : sig
    type t

    val lookup : t -> Vid.t -> Visibility.visible Canonical.Exp1.t option
    val create : (Vid.t * Visibility.visible Canonical.Exp1.t) list -> t
  end = struct
    type t = Visibility.visible Canonical.Exp1.t Map.M(Vid).t

    let create =
      List.fold
        ~init:(Map.empty (module Vid))
        ~f:(fun t (k, v) -> Map.set ~key:k ~data:v t)
    ;;

    let lookup t k = Map.find t k
  end

  module Applicand = struct
    type t =
      | Recursion_level of int
      | Definition of Canonical.Def.t
  end

  (* [Tenv.t]
     Environment for resolving type-definitions *)
  module Tenv : sig
    type key = Gid.t * Tid.t
    type t

    val find : t -> key -> [ `Recursion_level of int ] option
    val empty : t
    val extend : t -> key -> [ `Recursion_level of int ] -> t
  end = struct
    module Key = struct
      module T = struct
        type t = Gid.t * Tid.t [@@deriving compare, sexp_of]
      end

      include T
      include Comparator.Make (T)
    end

    type key = Key.t
    type t = [ `Recursion_level of int ] Map.M(Key).t

    let find t k = Map.find t k
    let empty = Map.empty (module Key)
    let extend t k v = Map.set ~key:k ~data:v t
  end

  (* [Defining.t]
     Monad for managing un-rolling depth, and maintaing a [Tenv.t] *)
  module Defining : sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val look_env : Tenv.key -> Applicand.t option t
    val extend_new_tid : Tenv.key -> Canonical.Def.t t -> Applicand.t t
    val exec : 'a t -> 'a
  end = struct
    type 'a t = depth:int -> Tenv.t -> 'a

    let return x ~depth:_ _tenv = x

    let bind t f ~depth tenv =
      let x = t ~depth tenv in
      (f x) ~depth tenv
    ;;

    let look_env key ~depth:_ tenv =
      let result = Tenv.find tenv key in
      Option.map ~f:(fun (`Recursion_level x) -> Applicand.Recursion_level x) result
    ;;

    let extend_new_tid key def_t ~depth tenv =
      Applicand.Definition
        (let value = `Recursion_level depth in
         let tenv = Tenv.extend tenv key value in
         def_t ~depth:(depth + 1) tenv)
    ;;

    let exec t = t ~depth:0 Tenv.empty
  end

  type 'a defining = 'a Defining.t

  let ( >>= ) = Defining.bind
  let return = Defining.return

  let sequence_defining : 'a list -> f:('a -> 'b defining) -> 'b list defining =
    fun xs ~f ->
    let rec loop acc_ys = function
      | [] -> return (List.rev acc_ys)
      | x :: xs -> f x >>= fun y -> loop (y :: acc_ys) xs
    in
    loop [] xs
  ;;

  (*
     Shape evaluation.

     Shapes are evaluated to canonical-shape (expressions), with additional defs collected
     in the [defining] monad, which also manages generation/mapping to [Canonical.Tid.t]

     There is downwards context of [group] and [Venv.t]
     The (current) [group] changes when the case for [Top_app] calls [eval_app].

     The current [Venv.t] is abandoned when [eval_app] is called, and then re-created after
     the decision has been made to either inline the type-application, or make a reference
     to a type-definition, which is created at most once for each (Gid.t * Tid.t).

     We make a type-definition always for Records and Variants, and in addition for any
     other cyclic type-definition.
  *)

  let rec eval : group -> Venv.t -> t -> Visibility.visible Canonical.Exp1.t defining =
    fun group venv t ->
      match t with
      | Record binds ->
        sequence_defining binds ~f:(fun (s, x) ->
          eval group venv x >>= fun y -> return (s, y))
        >>= fun binds -> return (Canonical.Create.record binds)
      | Variant alts ->
        sequence_defining alts ~f:(fun (s, xs) ->
          eval_list group venv xs >>= fun ys -> return (s, ys))
        >>= fun alts -> return (Canonical.Create.variant alts)
      | Var (loc, vid) ->
        (match Venv.lookup venv vid with
         | Some x -> return x
         | None -> eval_fail loc !"Free type variable: '%{Vid}" vid ())
      | Annotate (s, t) ->
        eval group venv t >>= fun v -> return (Canonical.Create.annotate s v)
      | Base (s, ts) ->
        eval_list group venv ts >>= fun vs -> return (Canonical.Create.basetype s vs)
      | Tuple ts ->
        eval_list group venv ts >>= fun vs -> return (Canonical.Create.tuple vs)
      | Top_app (in_group, tid, args) ->
        eval_list group venv args
        >>= fun args ->
        (* args evaluated in current group *)
        eval_app in_group tid args
      (* group changed here *)
      | Rec_app (tid, args) ->
        eval_list group venv args >>= fun args -> eval_app group tid args
      | Poly_variant (loc, cs) ->
        sequence_defining ~f:(eval_poly_constr group venv) cs
        >>= fun xss -> return (Canonical.Create.poly_variant loc (List.concat xss))

  and eval_list : group -> Venv.t -> t list -> _ Canonical.Exp1.t list defining =
    fun group venv ts -> sequence_defining ts ~f:(eval group venv)

  and eval_poly_constr
    :  group -> Venv.t -> t poly_constr
      -> (string * Visibility.opaque Canonical.Exp1.t option) list defining
    =
    fun group venv c ->
      match c with
      | `Constr (s, None) -> return [ s, None ]
      | `Constr (s, Some t) ->
        eval group venv t >>= fun v -> return [ s, Some (Canonical.Exp1.opaque v) ]
      | `Inherit (loc, t) ->
        eval group venv t
        >>= fun v ->
        (match Canonical.Exp1.get_poly_variant v with
         | Ok tab -> return (Sorted_table.expose tab)
         | Error desc ->
           eval_fail
             loc
             "The shape for an inherited type is not described as a polymorphic-variant: %s"
             desc
             ())

  and eval_definition : group -> Vid.t list -> t -> Canonical.Def.t defining =
    fun group formals body ->
      let venv = Venv.create (List.mapi formals ~f:(fun i x -> x, Canonical.Exp1.var i)) in
      eval group venv body >>= fun v -> return (Canonical.Create.define v)

  and eval_app : group -> Tid.t -> _ Canonical.Exp1.t list -> _ Canonical.Exp1.t defining
    =
    fun group tid args ->
      let gid = Group.id group in
      let formals, body = Group.lookup group tid in
      let record_or_normal_variant =
        match body with
        | Record _ | Variant _ -> true
        | Tuple _ | Annotate _ | Base _ | Poly_variant _ | Var _ | Rec_app _ | Top_app _ ->
          false
      in
      let cyclic = is_cyclic group tid in
      let cyclic_no_VR = is_cyclic_with_no_intervening_VR group tid in
      if (record_or_normal_variant && cyclic) || cyclic_no_VR
      then
        Defining.look_env (gid, tid)
        >>= (function
          | Some recurse -> return recurse
          | None ->
            Defining.extend_new_tid (gid, tid) (eval_definition group formals body))
        >>= function
        | Recursion_level r -> return (Canonical.Exp1.recurse r args)
        | Definition def -> return (Canonical.Exp1.apply def args)
      else (
        let venv =
          match List.zip formals args with
          | Ok x -> Venv.create x
          | Unequal_lengths -> failwith "apply, incorrect type application arity"
        in
        eval group venv body)
  ;;

  (* top level entry point for evaluation *)
  let eval : t -> Canonical.t =
    fun t ->
      let group = group (Location.of_string "top-level") [] in
      let venv = Venv.create [] in
      let v = Defining.exec (eval group venv t) in
      Canonical.Create.create v
  ;;
end

module Canonical = struct
  include Canonical_full

  module Exp = struct
    type t = Visibility.visible Exp1.t
  end
end

include Evaluation (Canonical_full)
module Canonical_selected = Canonical_digest
module Evaluation_to_digest = Evaluation (Canonical_selected)

let eval_to_digest exp = Canonical_selected.to_digest (Evaluation_to_digest.eval exp)
let eval_to_digest_string exp = Digest.to_hex (eval_to_digest exp)

module For_typerep = struct
  exception Not_a_tuple of t [@@deriving sexp_of]

  let deconstruct_tuple_exn t =
    match t with
    | Tuple ts -> ts
    | _ -> raise (Not_a_tuple t)
  ;;
end
