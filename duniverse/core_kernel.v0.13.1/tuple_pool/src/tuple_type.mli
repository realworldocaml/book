(** Tuple-like types used in [Pool].

    See {!Tuple_type_intf} for documentation. *)

open! Core_kernel
open! Import
open Tuple_type_intf

module type Slot = Slot
module type Slots = Slots

module Slots : Slots

