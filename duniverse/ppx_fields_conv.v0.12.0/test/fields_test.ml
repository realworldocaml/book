module Simple : sig
  type t = { x : int; w : int } [@@deriving fields]
end = struct
  type t = { x : int; w : int } [@@deriving fields]

  let%test _ = Fields.create ~x:2 ~w:4 = { x = 2; w = 4; }

  let all_even = { x = 2; w = 4 }
  let some_even = { x = 1; w = 4 }
  let none_even = { x = 1; w = 3 }

  let is_even t field = Fieldslib.Field.get field t mod 2 = 0
  let%test _ = Fields.for_all ~x:(is_even all_even) ~w:(is_even all_even) = true
  let%test _ = Fields.for_all ~x:(is_even some_even) ~w:(is_even some_even) = false
  let%test _ = Fields.exists  ~x:(is_even some_even) ~w:(is_even some_even) = true
  let%test _ = Fields.exists  ~x:(is_even none_even) ~w:(is_even none_even) = false

  let is_even field t n = assert (Fieldslib.Field.get field t = n); n mod 2 = 0
  let%test _ = Fields.Direct.for_all  all_even ~x:is_even ~w:is_even = true
  let%test _ = Fields.Direct.for_all some_even ~x:is_even ~w:is_even = false
  let%test _ = Fields.Direct.exists  some_even ~x:is_even ~w:is_even = true
  let%test _ = Fields.Direct.exists  none_even ~x:is_even ~w:is_even = false

  let t = { x = 1; w = 3 }

  let add_one t field = Fieldslib.Field.get field t + 1
  let%test _ = Fields.map ~x:(add_one t) ~w:(add_one t) = { x = 2; w = 4; }
  let%test _ = Fields.to_list ~x:(add_one t) ~w:(add_one t) = [ 2; 4 ]

  let add_one field t n = assert (Fieldslib.Field.get field t = n); n + 1
  let%test _ = Fields.Direct.map t ~x:add_one ~w:add_one = { x = 2; w = 4; }
  let%test _ = Fields.Direct.to_list t ~x:add_one ~w:add_one = [ 2; 4 ]

  let fold_one t acc field = Fieldslib.Field.get field t + 1 :: acc
  let%test _ = Fields.fold ~init:[] ~x:(fold_one t) ~w:(fold_one t) = [ 4; 2 ]

  let fold_one acc field t n = assert (Fieldslib.Field.get field t = n); (n + 1) :: acc
  let%test _ = Fields.Direct.fold t ~init:[] ~x:fold_one ~w:fold_one = [ 4; 2 ]

  let iter_one t buf field = buf := (Fieldslib.Field.get field t + 1) :: !buf
  let%test _ =
    let buf = ref [] in
    Fields.iter ~x:(iter_one t buf) ~w:(iter_one t buf);
    !buf = [ 4; 2 ]
  ;;

  let iter_one buf field t n = assert (Fieldslib.Field.get field t = n); buf := (n + 1) :: !buf
  let%test _ =
    let buf = ref [] in
    Fields.Direct.iter t ~x:(iter_one buf) ~w:(iter_one buf);
    !buf = [ 4; 2 ]
  ;;
end

module Rec = struct
  type a = {
    something1 : b;
  }
  and b = A of a
  [@@deriving fields]

  let _ = something1
end

module Multiple_names = struct
  type a = {
    a : int;
  }
  and b = {
    b : int;
  }
  [@@deriving fields]
  let%test _ = b { b = 1 } = 1
  let%test _ = a { a = 1 } = 1
  let _ = Fields_of_a.a
  let _ = Fields_of_b.b
  let _ = (Fields_of_a.a : (_, _) Fieldslib.Field.t :> (_, _) Fieldslib.Field.readonly_t)
end

module Private : sig
  type t = private { a : int; mutable b : int }
  [@@deriving fields]
end = struct
  type u = { a : int; mutable b : int }
  type t = u = private { a : int; mutable b : int }
  [@@deriving fields]
  (* let _ = Fieldslib.Field.setter Fields.a *)
end
(* let _ = Fieldslib.Field.setter Private.Fields.a *)
let _ = Private.Fields.fold
let _ = Private.Fields.a
let _ = Fieldslib.Field.name Private.Fields.a
let _ = Fieldslib.Field.get Private.Fields.a
let _ = Private.Fields.map_poly
  { Fieldslib.Field.f = (fun f -> let _ = Fieldslib.Field.get f in ())}

module Warnings : sig
  (* could generate an unused warning but for crazy reasons, only
     when the type is private *)
  type t = private { foo : int } [@@deriving fields]
  val foo : string
end = struct
  type t = { foo : int } [@@deriving fields]
  let foo = "a"
end

let%test_module "set_all_mutable_fields" = (module struct
  module M : sig
    type 'a t = { mutable a : int; b : string; mutable c : 'a }
    [@@deriving fields]
  end = struct
    type 'a t = { mutable a : int; b : string; mutable c : 'a }
    [@@deriving fields]
  end
  open M

  let%test_unit _ =
    let t : _ t = { a = 0; b = ""; c = nan; } in
    let final_t : _ t = { a = 12; b = t.b ; c = 12.; } in
    Fields.Direct.set_all_mutable_fields t ~a:final_t.a ~c:final_t.c;
    assert (t = final_t)
  ;;
end)

(* Sometimes it's convenient for the type of the accumulator to change as you handle
   the individual fields. *)
module M
    (F1 : sig type t = { a : int; b : string; c : bool; } [@@deriving fields] end)
    (F2 : sig type t = { a : int; b : string; } [@@deriving fields] end) = struct

  let convert : F1.t -> F2.t =
    F1.Fields.Direct.fold ~init:F2.Fields.create
      ~a:(fun acc field x _ -> acc ~a:(Fieldslib.Field.get field x))
      ~b:(fun acc field x _ -> acc ~b:(Fieldslib.Field.get field x))
      ~c:(fun acc _field _x _ -> acc)

  let construct () : F1.t =
    F1.Fields.fold ~init:F1.Fields.create
      ~a:(fun f _ -> f ~a:8)
      ~b:(fun f _ -> f ~b:"foo")
      ~c:(fun f _ -> f ~c:true)
end
