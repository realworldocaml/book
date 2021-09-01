open! Core
open! Async
include Io.S with type Fd.t = Reader.t * Writer.t
