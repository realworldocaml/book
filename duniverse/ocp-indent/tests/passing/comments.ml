(* A *)

type x = 
  (* A *)
  | Foo

  (* B *)

  | Bar

(* AA *)

(* D *)
let x = 3

module M = struct
  (* M1 *)
  let x =
    a
  (* M2 *)
  let y =
    b

  (* M3 *)
  (* M4 *)
end

let f x =
  if true then
    0
    (* comment *)
  else if false then
    1

let g x =
  if true then
    0

  (* comment *)
  else if false then
    1

let _ =
  f x
    (* bla *) y
    (* bla *) (z)

module M_bad : sig
  type _ t =
    | A : a -> a t
    | B : b -> b t
    (** Indented correctly *)

  type 'a t =
    | A of 'a
    (** Indented correctly *)

  type 'a t =
    | A of 'a
    | B of 'a
    (** Indented too far *)
end

module M_ok : sig
  type _ t =
    | A : a -> a t
    | B : b -> b t
    (** Indented correctly *)

  type 'a t =
    | A of 'a
    (** Indented correctly *)

  type 'a t =
    | A of 'a
    | B of 'a
    (** Indented correctly! *)

  val x : int
end

module M = struct
  type _ t =
    | A : a -> a t
    | B : b -> b t
    (** Indented too far *)
end

module type M = sig
  type _ t =
    | A : a -> a t
    | B : b -> b t
    (** Indented correctly! *)

  val x : int
end

module M : sig
  type _ t =
    | A : a -> a t
    (** Indented correctly *)
end

type _ t =
  | A : a -> a t
  | B : b -> b t
  (** Indented correctly *)

(* ending comments *)

