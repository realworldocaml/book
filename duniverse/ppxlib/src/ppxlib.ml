(** Standard library for ppx rewriters *)

(** Make sure code using Ppxlib doesn't refer to compiler-libs without being
    explicit about it:

    @closed *)
include struct
  [@@@warning "-3"]

  open Ocaml_shadow

  include (
    Ocaml_shadow :
      module type of struct
        include Ocaml_shadow
      end
      with module Ast_helper := Ast_helper
      with module Asttypes := Asttypes
      with module Docstrings := Docstrings
      with module Identifiable := Identifiable
      with module Lexer := Lexer
      with module Location := Location
      with module Longident := Longident
      with module Parse := Parse
      with module Parsetree := Parsetree
      with module Pprintast := Pprintast
      with module Syntaxerr := Syntaxerr)
end

(** Expose some modules from {!Ppxlib_ast}; in particular, overwrite some of the
    modules above *)

module Ast = Ppxlib_ast.Ast
module Ast_helper = Ppxlib_ast.Ast_helper
module Asttypes = Ppxlib_ast.Asttypes
module Parse = Ppxlib_ast.Parse
module Parsetree = Ppxlib_ast.Parsetree
module Pprintast = Ppxlib_ast.Pprintast
module Selected_ast = Ppxlib_ast.Selected_ast

include Ast
(** Include all the Ast definitions since we need them in every single ppx

    @closed *)

module Ast_builder = Ast_builder
module Ast_pattern = Ast_pattern
module Ast_traverse = Ast_traverse
module Attribute = Attribute
module Code_path = Code_path
module Caller_id = Caller_id
module Context_free = Context_free
module Deriving = Deriving
module Driver = Driver
module Expansion_context = Expansion_context
module Extension = Extension
module File_path = File_path
module Keyword = Keyword
module Loc = Loc
module Location = Location
module Longident = Longident
module Merlin_helpers = Merlin_helpers
module Reserved_namespaces = Name.Reserved_namespaces
module Spellcheck = Spellcheck
module Quoter = Quoter
module Ast_io = Utils.Ast_io.Read_bin

include Common
(** @closed *)

(**/**)

(* For tests and Ppx_core compatibility layer *)
module Ppxlib_private = struct
  module Common = Common
  module Name = Name
end
