open Core

type t =
  { mutable total : Int63.t
  ; mutable char : Int63.t
  ; mutable fifo : Int63.t
  ; mutable file : Int63.t
  ; mutable socket : Int63.t
  }
[@@deriving sexp]

let create () =
  { total = Int63.zero
  ; char = Int63.zero
  ; fifo = Int63.zero
  ; file = Int63.zero
  ; socket = Int63.zero
  }
;;

let update t ~(kind : Fd.Kind.t) ~bytes =
  t.total <- Int63.(t.total + bytes);
  match kind with
  | Char -> t.char <- Int63.( + ) t.char bytes
  | Fifo -> t.fifo <- Int63.( + ) t.fifo bytes
  | File -> t.file <- Int63.( + ) t.file bytes
  | Socket _ -> t.socket <- Int63.( + ) t.socket bytes
;;

let total t = t.total

let get t ~(kind : Fd.Kind.t) =
  match kind with
  | Char -> t.char
  | Fifo -> t.fifo
  | File -> t.file
  | Socket _ -> t.socket
;;
