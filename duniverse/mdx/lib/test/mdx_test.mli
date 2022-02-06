exception Test_block_failure of Mdx.Block.t * string

module Package : sig
  val unix : string
  val findlib_top : string
  val findlib_internal : string
  val compilerlibs_toplevel : string
end

module Predicate : sig
  val byte : string
  val toploop : string
end

val run_exn :
  non_deterministic:bool ->
  silent_eval:bool ->
  record_backtrace:bool ->
  syntax:Mdx.Syntax.t option ->
  silent:bool ->
  verbose_findlib:bool ->
  prelude:string list ->
  prelude_str:string list ->
  file:string ->
  section:string option ->
  root:string option ->
  force_output:bool ->
  output:[ `File of string | `Stdout ] option ->
  directives:Mdx_top.directive list ->
  packages:string list ->
  predicates:string list ->
  int
