open! Core

module type Fd = Io_intf.Fd
module type S = Io_intf.S

module Make (Fd : Fd) : S with module Fd := Fd
