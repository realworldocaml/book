(** Provides a {{!Async_kernel.Clock_intf.Clock}[Clock]} with [Time_ns] as the unit. *)

open! Core_kernel
open! Import

module type Clock = Clock_intf.Clock
module type Clock_deprecated = Clock_intf.Clock_deprecated

include Clock with module Time := Time_ns (** @open *)
