(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



class instrumenter : Ppx_tools_411.Ast_mapper_class.mapper
(**  This class implements an instrumenter to be used through the {i -ppx}
    command-line switch. *)
