(** Top-level utilities *)

val read_lexbuf :
  ?expand:bool ->
  ?keep_poly:bool ->
  ?xdebug:bool ->
  ?inherit_fields:bool ->
  ?inherit_variants:bool ->
  ?pos_fname:string ->
  ?pos_lnum:int ->
  Lexing.lexbuf -> Ast.full_module * Expand.original_types
  (** Read an ATD file from a lexbuf. See also [read_channel], [load_file]
      and [load_string].

      If expand is true, the second part of the return value will contain
      a hash table mapping the types generated during monomorphization back
      to their original polymorphic types. See {!Expand.original_types}
      for more information about this table. If expand is false, the value
      will be the empty hash table.

      @param expand
             Perform monomorphization by creating specialized
             type definitions starting with an underscore.
             Default is false. See also {!Expand}.
             This corresponds to the [-x] option of [atdcat].
      @param keep_poly
             Preserve left-hand-side of all original type definitions
             instead of removing parametrized definitions.
             This option only applies when [expand = true].
             Default is false. See also {!Expand}.
             This corresponds to the [-xk] option of [atdcat].
      @param xdebug
             Debugging option producing meaningful but non ATD-compliant
             type names when new types names are created.
             Default is false.
             This corresponds to the [-xd] option of [atdcat].
      @param inherit_fields
             Expand [inherit] statements in record types.
             Default is false. See also {!Atd_inherit}.
             This corresponds to the [-if] option of [atdcat].
      @param inherit_variants
             Expand [inherit] statements in sum types.
             Default is false. See also {!Atd_inherit}.
             This corresponds to the [-iv] option of [atdcat].

      @param pos_fname
             Set the file name for use in error messages.
             Default is [""].

      @param pos_lnum
             Set the number of the first line for use in error messages.
             Default is [1].
  *)

val read_channel :
  ?expand:bool ->
  ?keep_poly:bool ->
  ?xdebug:bool ->
  ?inherit_fields:bool ->
  ?inherit_variants:bool ->
  ?pos_fname:string ->
  ?pos_lnum:int ->
  in_channel -> Ast.full_module * Expand.original_types
  (** Read an ATD file from an [in_channel]. Options: see [read_lexbuf].
      The default [pos_fname] is set to ["<stdin>"] when appropriate. *)

val load_file :
  ?expand:bool ->
  ?keep_poly:bool ->
  ?xdebug:bool ->
  ?inherit_fields:bool ->
  ?inherit_variants:bool ->
  ?pos_fname:string ->
  ?pos_lnum:int ->
  string -> Ast.full_module * Expand.original_types
  (** Read an ATD file. Options: see [read_lexbuf].
      The default [pos_fname] is the given input file name. *)

val load_string :
  ?expand:bool ->
  ?keep_poly:bool ->
  ?xdebug:bool ->
  ?inherit_fields:bool ->
  ?inherit_variants:bool ->
  ?pos_fname:string ->
  ?pos_lnum:int ->
  string -> Ast.full_module * Expand.original_types
  (** Read ATD data from a string. Options: see [read_lexbuf]. *)

val tsort :
  Ast.module_body -> (bool * Ast.module_body) list
  (**
     Topological sort for dependency analysis.
     [tsort] splits definitions into mutually-recursive groups,
     ordered such that each group may only depend on type definitions
     of its own group or previous groups.
     The boolean flags indicate groups of one or more
     mutually recursive definitions.
  *)
