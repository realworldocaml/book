open Import

module type OCaml_version = Versions.OCaml_version

module Ast            = Ast
module Ast_helper     = Ast_helper
module Ast_magic      = Selected_ast.Ast.Config
module Asttypes       = Asttypes
module Compiler_version = Versions.OCaml_current
module Js             = Js
module Find_version   = Versions.Find_version
module Convert        = Versions.Convert
module Extra_warnings = Warn
module Lexer          = Lexer
module Location_error = Location_error
module Parse          = Parse
module Parser         = Parser
module Parsetree      = Parsetree
module Pprintast      = Pprintast
module Select_ast     = Select_ast
module Selected_ast   = Selected_ast
module Syntaxerr      = Syntaxerr

module Import_for_core = Import
