open Core

module Key = struct
  type t =
    [ `Read
    | `Write
    ]
  [@@deriving sexp]

  let flip = function
    | `Read -> `Write
    | `Write -> `Read
  ;;
end

type ('a, +'z) any =
  { mutable read : 'a
  ; mutable write : 'a
  }
[@@deriving sexp]

module Immutable = struct
  type 'a t = ('a, immutable) any [@@deriving sexp]
end

module Read_only = struct
  type 'a t = ('a, read) any [@@deriving sexp]
end

module Mutable = struct
  type 'a t = ('a, read_write) any [@@deriving sexp]
end

type 'a t = 'a Immutable.t [@@deriving sexp]

let create ~read ~write = { read; write }
let createi f = { read = f `Read; write = f `Write }
let create_both x = { read = x; write = x }
let create_fn f = { read = f (); write = f () }

let create_with key v ~zero =
  match key with
  | `Read -> { read = v; write = zero }
  | `Write -> { read = zero; write = v }
;;

let copy { read; write } = { read; write }
let exists t ~f = f t.read || f t.write
let for_all t ~f = f t.read && f t.write

let iteri t ~f =
  f `Read t.read;
  f `Write t.write
;;

let iter t ~f =
  f t.read;
  f t.write
;;

let map t ~f = { read = f t.read; write = f t.write }
let mapi t ~f = { read = f `Read t.read; write = f `Write t.write }
let foldi t init ~f = f (f init (`Read, t.read)) (`Write, t.write)
let fold t init ~f = f (f init t.read) t.write

let get t key =
  match key with
  | `Read -> t.read
  | `Write -> t.write
;;

let set t key value =
  match key with
  | `Read -> t.read <- value
  | `Write -> t.write <- value
;;

let replace t key ~f =
  match key with
  | `Read -> t.read <- f t.read
  | `Write -> t.write <- f t.write
;;

let replace_all t ~f =
  t.read <- f `Read t.read;
  t.write <- f `Write t.write
;;

module Export = struct
  type ('a, 'z) read_write_ = ('a, 'z) any =
    { mutable read : 'a
    ; mutable write : 'a
    }
end
