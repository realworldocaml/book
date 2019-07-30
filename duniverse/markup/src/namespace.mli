(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

module Parsing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    unit Error.handler ->
    context ->
    string -> (string * string) list ->
      (name * (name * string) list) cps
  val pop : context -> unit
  val expand_element : unit Error.handler -> context -> string -> name cps

  val parse : string -> string * string
end

module Writing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    unit Error.handler ->
    context ->
    name -> (name * string) list ->
      (string * (string * string) list) cps
  val pop : context -> unit
end
