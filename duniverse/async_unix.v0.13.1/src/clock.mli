(** A {{!Async_kernel.Clock_intf.Clock}[Clock]} based on [Core.Time]. *)

open! Core
open! Import

include Async_kernel.Clock_ns.Clock with module Time := Time (** @open *)
