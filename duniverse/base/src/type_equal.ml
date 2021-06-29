open! Import

type ('a, 'b) t = T : ('a, 'a) t [@@deriving_inline sexp_of]

let sexp_of_t
  : type a b.
    (a -> Ppx_sexp_conv_lib.Sexp.t)
    -> (b -> Ppx_sexp_conv_lib.Sexp.t)
    -> (a, b) t
    -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_a _of_b -> function
    | T -> Ppx_sexp_conv_lib.Sexp.Atom "T"
;;

[@@@end]

type ('a, 'b) equal = ('a, 'b) t

let refl = T
let sym (type a b) (T : (a, b) t) : (b, a) t = T
let trans (type a b c) (T : (a, b) t) (T : (b, c) t) : (a, c) t = T
let conv (type a b) (T : (a, b) t) (a : a) : b = a

module Lift (X : sig
    type 'a t
  end) =
struct
  let lift (type a b) (T : (a, b) t) : (a X.t, b X.t) t = T
end

module Lift2 (X : sig
    type ('a1, 'a2) t
  end) =
struct
  let lift (type a1 b1 a2 b2) (T : (a1, b1) t) (T : (a2, b2) t)
    : ((a1, a2) X.t, (b1, b2) X.t) t
    =
    T
  ;;
end

module Lift3 (X : sig
    type ('a1, 'a2, 'a3) t
  end) =
struct
  let lift (type a1 b1 a2 b2 a3 b3) (T : (a1, b1) t) (T : (a2, b2) t) (T : (a3, b3) t)
    : ((a1, a2, a3) X.t, (b1, b2, b3) X.t) t
    =
    T
  ;;
end

let detuple2 (type a1 a2 b1 b2) (T : (a1 * a2, b1 * b2) t) : (a1, b1) t * (a2, b2) t =
  T, T
;;

let tuple2 (type a1 a2 b1 b2) (T : (a1, b1) t) (T : (a2, b2) t) : (a1 * a2, b1 * b2) t =
  T
;;

module type Injective = sig
  type 'a t

  val strip : ('a t, 'b t) equal -> ('a, 'b) equal
end

module type Injective2 = sig
  type ('a1, 'a2) t

  val strip : (('a1, 'a2) t, ('b1, 'b2) t) equal -> ('a1, 'b1) equal * ('a2, 'b2) equal
end

module Composition_preserves_injectivity (M1 : Injective) (M2 : Injective) = struct
  type 'a t = 'a M1.t M2.t

  let strip e = M1.strip (M2.strip e)
end

module Obj = struct
  module Extension_constructor = struct
    [@@@ocaml.warning "-3"]

    let id = Caml.Obj.extension_id
    let of_val = Caml.Obj.extension_constructor
  end
end

module Id = struct
  module Uid = Int

  module Witness = struct
    module Key = struct
      type _ t = ..
      type type_witness_int = [ `type_witness of int ] [@@deriving_inline sexp_of]

      let sexp_of_type_witness_int =
        (function
          | `type_witness v0 ->
            Ppx_sexp_conv_lib.Sexp.List
              [ Ppx_sexp_conv_lib.Sexp.Atom "type_witness"; sexp_of_int v0 ]
            : type_witness_int -> Ppx_sexp_conv_lib.Sexp.t)
      ;;

      [@@@end]

      let sexp_of_t _sexp_of_a t =
        `type_witness (Obj.Extension_constructor.id (Obj.Extension_constructor.of_val t))
        |> sexp_of_type_witness_int
      ;;
    end

    module type S = sig
      type t
      type _ Key.t += Key : t Key.t
    end

    type 'a t = (module S with type t = 'a)

    let sexp_of_t (type a) sexp_of_a (module M : S with type t = a) =
      M.Key |> Key.sexp_of_t sexp_of_a
    ;;

    let create (type t) () =
      let module M = struct
        type nonrec t = t
        type _ Key.t += Key : t Key.t
      end
      in
      (module M : S with type t = t)
    ;;

    let uid (type a) (module M : S with type t = a) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val M.Key)
    ;;

    (* We want a constant allocated once that [same] can return whenever it gets the same
       witnesses.  If we write the constant inside the body of [same], the native-code
       compiler will do the right thing and lift it out.  But for clarity and robustness,
       we do it ourselves. *)
    let some_t = Some T

    let same (type a b) (a : a t) (b : b t) : (a, b) equal option =
      let module A = (val a : S with type t = a) in
      let module B = (val b : S with type t = b) in
      match A.Key with
      | B.Key -> some_t
      | _ -> None
    ;;
  end


  type 'a t =
    { witness : 'a Witness.t
    ; name : string
    ; to_sexp : 'a -> Sexp.t
    }

  let sexp_of_t _ { witness; name; to_sexp } : Sexp.t =
    if am_testing
    then Atom name
    else
      List
        [ List [ Atom "name"; Atom name ]
        ; List [ Atom "witness"; witness |> Witness.sexp_of_t to_sexp ]
        ]
  ;;

  let to_sexp t = t.to_sexp
  let name t = t.name
  let create ~name to_sexp = { witness = Witness.create (); name; to_sexp }
  let uid t = Witness.uid t.witness
  let hash t = uid t
  let hash_fold_t s t = hash_fold_int s (uid t)
  let same_witness t1 t2 = Witness.same t1.witness t2.witness
  let same t1 t2 = Option.is_some (same_witness t1 t2)

  let same_witness_exn t1 t2 =
    match same_witness t1 t2 with
    | Some w -> w
    | None ->
      Error.raise_s
        (Sexp.message
           "Type_equal.Id.same_witness_exn got different ids"
           [ ( ""
             , sexp_of_pair (sexp_of_t sexp_of_opaque) (sexp_of_t sexp_of_opaque) (t1, t2)
             )
           ])
  ;;
end
