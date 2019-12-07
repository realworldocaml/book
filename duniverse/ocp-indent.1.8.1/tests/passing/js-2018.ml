(* New issues reported as of 2018 *)

(* include.ml *)

module M : sig
  include module type of struct
    include I
  end

  val f : unit -> unit
end

(* record.ml *)
let _ =
  { a_field : int =
      3
  ; another_field : int =
      3
  }

let _ =
  { a_field =
      3
  ; another_field =
      3
  }

(* polyvariant.mli *)
module type S = sig
  val a :
    something:int
    -> non_optional:
         int list
           list
           list
    -> ?optional:
         int
    -> int
end

module type S = sig
  val a
    :  something:int
    -> non_optional:
         [ `A
         | `B
         ]
    -> ?optional:
         [ `A
         | `B
         ]
    -> int
end

(* type_annot_ext.ml *)
let x =
  let v : [%ext : int] = w in
  "hello"

let f a =
  match (a : Nothing.t) with
  | _ -> .

let g () =
  1

;;

(* let_module_functor_application.ml *)
let module X = Make (struct
    let i = 10
  end)

(* gadts.ml *)
type 'a t =
  | Foo :
      int list list list list
      * string list
      * float list
      * bool list
      * 'a option list list
      -> 'a option list list t

(* inline_record_indentation.ml *)
type t =
  | Clause of {
    field : ty;
  }
  | Clause of {
      field : ty;
    }
  | Clause of {
      field : ty;
    }

(* constraint.ml *)
type 'a t = 'b constraint 'a = < foo : 'b >

let x = 8

(* custom_delim_in_comments.ml *)
(* some comment {|"|} *)
let f x = x
(* {|"|} *)
