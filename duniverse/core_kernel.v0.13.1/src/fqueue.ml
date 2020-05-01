open! Import
include Fdeque

let enqueue = enqueue_back
let enqueue_top = enqueue_front
let bot_exn = peek_back_exn
let bot = peek_back
let top_exn = peek_front_exn
let top = peek_front
let dequeue_exn = dequeue_front_exn
let dequeue = dequeue_front
let discard_exn = drop_front_exn
let to_sequence = Front_to_back.to_sequence
let of_sequence = Front_to_back.of_sequence
