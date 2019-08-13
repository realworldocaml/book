
type ('a,'b) t = {
  dir : 'a * 'b;
  quantity : ('a , 'b) t;
  price : int * 'a;
  mutable cancelled : bool;
} [@@deriving fields]

type foo = {
  a : [`Bar | `Baz of string];
  b : int;
} [@@deriving fields]

module Private_in_mli = struct
  type ('a,'b) t = {
    dir : 'a * 'b;
    quantity : ('a , 'b) t;
    price : int * 'a;
    mutable cancelled : bool;
  } [@@deriving fields]
end

module Private_in_ml = struct
  type ('a,'b) t = ('a,'b) Private_in_mli.t = private {
    dir : 'a * 'b;
    quantity : ('a , 'b) t;
    price : int * 'a;
    mutable cancelled : bool;
  } [@@deriving fields]
end
