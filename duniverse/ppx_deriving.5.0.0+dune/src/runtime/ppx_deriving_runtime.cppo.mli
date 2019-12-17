(** A module collecting all predefined OCaml types, exceptions and
    modules operating on them, so that ppx_deriving plugins operate
    in a well-defined environment. *)

(** {2 Predefined types} *)
type nonrec int = int
type nonrec char = char
type nonrec string = string
type nonrec float = float
type nonrec bool = bool
type nonrec unit = unit
type nonrec exn = exn
type nonrec 'a array = 'a array
type nonrec 'a list = 'a list
type nonrec 'a option = 'a option
type nonrec nativeint = nativeint
type nonrec int32 = int32
type nonrec int64 = int64
type nonrec 'a lazy_t = 'a lazy_t
type nonrec bytes = bytes

(** {2 Predefined modules}
    {3 Operations on predefined types} *)

#if OCAML_VERSION >= (4, 08, 0)
include (module type of Stdlib with
  type fpclass = Stdlib.fpclass and
  type in_channel = Stdlib.in_channel and
  type out_channel = Stdlib.out_channel and
  type open_flag = Stdlib.open_flag and
  type 'a ref = 'a Stdlib.ref and
  type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6 and
  type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4 and
  type ('a, 'b, 'c) format = ('a, 'b, 'c) Stdlib.format
)

module Result : sig
  type ('a, 'b) t = ('a, 'b) Result.t =
    | Ok of 'a
    | Error of 'b

  (* we also expose Result.result for backward-compatibility
     with the Result package! *)
  type ('a, 'b) result = ('a, 'b) Result.t =
    | Ok of 'a
    | Error of 'b
end
#else
module Pervasives : (module type of Pervasives with
  type fpclass = Pervasives.fpclass and
  type in_channel = Pervasives.in_channel and
  type out_channel = Pervasives.out_channel and
  type open_flag = Pervasives.open_flag and
  type 'a ref = 'a Pervasives.ref and
  type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Pervasives.format6 and
  type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Pervasives.format4 and
  type ('a, 'b, 'c) format = ('a, 'b, 'c) Pervasives.format)

module Stdlib = Pervasives

include (module type of Pervasives with
  type fpclass = Pervasives.fpclass and
  type in_channel = Pervasives.in_channel and
  type out_channel = Pervasives.out_channel and
  type open_flag = Pervasives.open_flag and
  type 'a ref = 'a Pervasives.ref and
  type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Pervasives.format6 and
  type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Pervasives.format4 and
  type ('a, 'b, 'c) format = ('a, 'b, 'c) Pervasives.format)

module Char : (module type of Char)
module String : (module type of String)
module Printexc : (module type of Printexc with
  type raw_backtrace = Printexc.raw_backtrace and
  type backtrace_slot = Printexc.backtrace_slot and
  type location = Printexc.location)
module Array : (module type of Array)
module List : (module type of List)
module Nativeint : (module type of Nativeint)
module Int32 : (module type of Int32)
module Int64 : (module type of Int64)
module Lazy : (module type of Lazy)
module Bytes : (module type of Bytes)

(** {3 Data structures} *)

module Hashtbl : (module type of Hashtbl with
  type ('a, 'b) t = ('a, 'b) Hashtbl.t and
  type statistics = Hashtbl.statistics)
module Queue : (module type of Queue with
  type 'a t = 'a Queue.t)
module Stack : (module type of Stack with
  type 'a t = 'a Stack.t)
module Set : (module type of Set)
module Map : (module type of Map)
module Weak : (module type of Weak with
  type 'a t = 'a Weak.t)
module Buffer : (module type of Buffer with
  type t = Buffer.t)
module Result : sig
  type ('a, 'b) t = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b

  (* we also expose Result.result for backward-compatibility *)
  type ('a, 'b) result = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b
end

(** {3 Formatting} *)

module Printf : (module type of Printf)
module Format : (module type of Format with
  type formatter_out_functions = Format.formatter_out_functions and
  type formatter_tag_functions = Format.formatter_tag_functions and
  type formatter = Format.formatter)
#endif
