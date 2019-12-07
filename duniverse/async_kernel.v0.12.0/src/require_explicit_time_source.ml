open! Core_kernel
open! Import
module Clock_ns = Clock_ns
module Time_ns = Time_ns
module Scheduler = Scheduler

let after = Clock_ns.after
let at = Clock_ns.at
let every = Clock_ns.every
let with_timeout = Clock_ns.with_timeout
