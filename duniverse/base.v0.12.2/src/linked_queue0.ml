open! Import0

type 'a t = 'a Caml.Queue.t

let create    = Caml.Queue.create
let clear     = Caml.Queue.clear
let copy      = Caml.Queue.copy
let is_empty  = Caml.Queue.is_empty
let length    = Caml.Queue.length
let peek      = Caml.Queue.peek
let pop       = Caml.Queue.pop
let push      = Caml.Queue.push
let transfer  = Caml.Queue.transfer

let iter t ~f       = Caml.Queue.iter f t
let fold t ~init ~f = Caml.Queue.fold f init t
