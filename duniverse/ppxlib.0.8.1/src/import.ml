include Base
include Stdio
include Ppxlib_ast

(* This is not re-exported by Base and we can't use [%here] in ppxlib *)
external __FILE__ : string = "%loc_FILE"

include Ast
