type t =
  { x : int
  ; y : u
  }

and u = A of int | B of t
[@@deriving traverse]
[%%expect{|
type t = { x : int; y : u; }
and u = A of int | B of t
class virtual map :
  object
    method virtual int : int -> int
    method t : t -> t
    method u : u -> u
  end
class virtual iter :
  object
    method virtual int : int -> unit
    method t : t -> unit
    method u : u -> unit
  end
class virtual ['acc] fold :
  object
    method virtual int : int -> 'acc -> 'acc
    method t : t -> 'acc -> 'acc
    method u : u -> 'acc -> 'acc
  end
class virtual ['acc] fold_map :
  object
    method virtual int : int -> 'acc -> int * 'acc
    method t : t -> 'acc -> t * 'acc
    method u : u -> 'acc -> u * 'acc
  end
class virtual ['ctx] map_with_context :
  object
    method virtual int : 'ctx -> int -> int
    method t : 'ctx -> t -> t
    method u : 'ctx -> u -> u
  end
class virtual ['res] lift :
  object
    method virtual constr : string -> 'res list -> 'res
    method virtual int : int -> 'res
    method virtual record : (string * 'res) list -> 'res
    method t : t -> 'res
    method u : u -> 'res
  end
class virtual ['ctx, 'res] lift_map_with_context :
  object
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual int : 'ctx -> int -> int * 'res
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method t : 'ctx -> t -> t * 'res
    method u : 'ctx -> u -> u * 'res
  end
|}]

type t =
  { a : int
  ; b : Int.t
  ; c : (int, bool) Stdlib.Result.t
  ; d : int Map.Make(Int).t
  }
[@@deriving traverse_iter]
[%%expect{|
type t = {
  a : int;
  b : int;
  c : (int, bool) result;
  d : int Map.Make(Int).t;
}
class virtual iter :
  object
    method virtual bool : bool -> unit
    method virtual int : int -> unit
    method virtual int__t : int -> unit
    method virtual map__make_'int'__t :
      ('a -> unit) -> 'a Map.Make(Int).t -> unit
    method virtual stdlib__result__t :
      ('a -> unit) -> ('b -> unit) -> ('a, 'b) result -> unit
    method t : t -> unit
  end
|}]
