(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



class instrumenter : object
   inherit Ppxlib.Ast_traverse.map_with_expansion_context

   method transform_impl_file:
      Ppxlib.Expansion_context.Base.t ->
      Ppxlib.Parsetree.structure ->
         Ppxlib.Parsetree.structure
end
(**  This class implements an instrumenter to be used through the {i -ppx}
    command-line switch. *)

val bisect_file : string option ref
(** Default value for [BISECT_FILE]. *)

val bisect_silent : string option ref
(** Default value for [BISECT_SILENT]. *)

val bisect_sigterm : bool ref
(** Default value for [BISECT_SIGTERM]. *)
