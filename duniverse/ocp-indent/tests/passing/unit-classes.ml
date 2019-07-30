(** ocaml classes
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual017.html)
*)

(* class types *)

class type c =
  object
  end

class type c =
  M.cl

class type c =
  ['a, 'b] M.cl

class type c =
  object
    ('ty)
    inherit cl
    val mutable virtual
      var : bool
    method private bar1 x ~y : bool
    method private virtual bar2 : 'a 'b.('a,'b) Hashtbl.t
    constraint
      'a = 'b
  end

(* class expressions *)

class c =
  ['a, 'b]
    M.cl

class c =
  fun a b ->
  object
  end

class c = object
  val x = true
end

class c =
  object
    (_ :
       'a)
    inherit Something.someclass
      as v
    val mutable
      var : bool
      = true
    val mutable virtual var2
      : string
    method private bar1 x ~y : bool =
      false
    method private virtual bar2 : 'a 'b.('a,'b) Hashtbl.t
    constraint
      'a = 'b
    initializer
      z
  end

(* method specific expressions *)

let e =
  var <- true

let e =
  {< var = false;
     var2 = true;
  >}


(* class definitions *)

class cl =
  object
    val x = true
  end
and
  virtual ['a, 'b]
    cl2 x y :
  object
    val x : bool
  end = fun x y ->
  object
    val x : bool = true
  end

class cl
  : object end

class type virtual ['a] clty = object
  method x : int
end

(* objects *)
val a :
  < >
let () = ()

val a :
  < .. >
let () = ()

val a :
  < meth: int option;
    meth2: 'a. 'a option;
    meth3: 'a 'b. ('a,'b) Hashtbl.t >
let () = ()

val a :
  < meth: int option;
    meth2: 'a. 'a option;
    meth3: 'a 'b. ('a,'b) Hashtbl.t;
    .. >
let () = ()

(* #-types *)
val a :
  #M.meth

val a :
  'a#M.meth

val a :
  ('a,'b*'c)
  #M.meth

(* object types *)
type a =
  < >
let () = ()

type a =
  < .. >
let () = ()

type a =
  < meth: int option;
    meth2: 'a. 'a option;
    meth3: 'a 'b. ('a,'b) Hashtbl.t >
let () = ()

type a =
  < meth: int option;
    meth2: 'a. 'a option;
    meth3: 'a 'b. ('a,'b) Hashtbl.t;
    .. >
let () = ()

type t =
  < a : int; b:
      < a: int; b: < c:int > >
  >
let () = ()

type t =
  < a : int; b:
      < a: int; b: < c: int -> int> >;
    c: int
  >
let () = ()

type 'a t =
  | Bla : < x : int > t
  | Blo : < y : int > t
