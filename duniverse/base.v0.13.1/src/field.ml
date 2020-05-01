(* The type [t] should be abstract to make the fset and set functions unavailable
   for private types at the level of types (and not by putting None in the field).
   Unfortunately, making the type abstract means that when creating fields (through
   a [create] function) value restriction kicks in. This is worked around by instead
   not making the type abstract, but forcing anyone breaking the abstraction to use
   the [For_generated_code] module, making it obvious to any reader that something ugly
   is going on.
   t_with_perm (and derivatives) is the type that users really use. It is a constructor
   because:
   1. it makes type errors more readable (less aliasing)
   2. the typer in ocaml 4.01 allows this:

   {[
     module A = struct
       type t = {a : int}
     end
     type t = A.t
     let f (x : t) = x.a
   ]}

   (although with Warning 40: a is used out of scope)
   which means that if [t_with_perm] was really an alias on [For_generated_code.t],
   people could say [t.setter] and break the abstraction with no indication that
   something ugly is going on in the source code.
   The warning is (I think) for people who want to make their code compatible with
   previous versions of ocaml, so we may very well turn it off.

   The type t_with_perm could also have been a [unit -> For_generated_code.t] to work
   around value restriction and then [For_generated_code.t] would have been a proper
   abstract type, but it looks like it could impact performance (for example, a fold on a
   record type with 40 fields would actually allocate the 40 [For_generated_code.t]'s at
   every single fold.) *)

module For_generated_code = struct
  type ('perm, 'record, 'field) t =
    { force_variance : 'perm -> unit
    ; (* force [t] to be contravariant in ['perm], because phantom type variables on
         concrete types don't work that well otherwise (using :> can remove them easily) *)
      name : string
    ; setter : ('record -> 'field -> unit) option
    ; getter : 'record -> 'field
    ; fset : 'record -> 'field -> 'record
    }

  let opaque_identity = Sys0.opaque_identity
end

type ('perm, 'record, 'field) t_with_perm =
  | Field of ('perm, 'record, 'field) For_generated_code.t
[@@unboxed]

type ('record, 'field) t = ([ `Read | `Set_and_create ], 'record, 'field) t_with_perm
type ('record, 'field) readonly_t = ([ `Read ], 'record, 'field) t_with_perm

let name (Field field) = field.name
let get (Field field) r = field.getter r
let fset (Field field) r v = field.fset r v
let setter (Field field) = field.setter

type ('perm, 'record, 'result) user =
  { f : 'field. ('perm, 'record, 'field) t_with_perm -> 'result }

let map (Field field) r ~f = field.fset r (f (field.getter r))

let updater (Field field) =
  match field.setter with
  | None -> None
  | Some setter -> Some (fun r ~f -> setter r (f (field.getter r)))
;;
