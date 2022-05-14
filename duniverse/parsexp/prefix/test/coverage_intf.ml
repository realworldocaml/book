open! Core
open! Import

module Saw_prefix = struct
  type t = { f : prefix:string -> unit }
end

module Saw_state = struct
  type t = { f : 'u 's. ('u, 's) Automaton.t -> unit }
end

module type Coverage = sig
  module Saw_prefix = Saw_prefix
  module Saw_state = Saw_state

  val with_prefix_coverage : string -> f:(Saw_prefix.t -> unit) -> unit
  val with_state_coverage : f:(Saw_state.t -> unit) -> unit
end
