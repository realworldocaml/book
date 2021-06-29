open! Import
open Utils

module Context : sig
  type 'a t =
    | Extension          of 'a Extension.Context.t
    | Floating_attribute of 'a Attribute.Floating.Context.t
end

module Replacement : sig
  type t

  val make
    :  context:'a Context.t
    -> start:Lexing.position
    -> stop:Lexing.position
    -> repl:'a Context_free.Generated_code_hook.single_or_many
    -> unit
    -> t

  val make_text
    :  start:Lexing.position
    -> stop:Lexing.position
    -> repl:string
    -> unit
    -> t
end

type mode =
  | Using_line_directives
  | Delimiting_generated_blocks

type target =
  | Output of mode
  | Corrected

val reconcile
  :  ?styler:string
  -> Replacement.t list
  -> kind:Kind.t
  -> contents:string
  -> input_filename:string
  -> output:string option
  -> input_name:string
  -> target:target
  -> unit
