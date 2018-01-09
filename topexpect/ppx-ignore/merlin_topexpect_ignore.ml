open Migrate_parsetree

(* Define the rewriter on OCaml 4.05 AST *)
open Ast_404
let ocaml_version = Versions.ocaml_404

(* Action of the rewriter: replace __HERE__ expression by a tuple ("filename",
   line, col) *)
let mapper _config _cookies =
  let open Ast_mapper in
  let open Location in
  let open Parsetree in
  let keep_item pstr = match pstr.pstr_desc with
    | Pstr_extension (({txt = ("expect" | "expect.nondeterministic")} , _), _)
    | Pstr_attribute ({txt = "part"}, _)
      -> false
    | _ -> true
  in
  let structure mapper pstr =
    default_mapper.structure mapper (List.filter keep_item pstr)
  in
  {default_mapper with structure}

(* Register the rewriter in the driver *)
let () =
  match Sys.getenv "__MERLIN_MASTER_PID" with
  | exception Not_found -> ()
  | _ ->
    Driver.register ~name:"merlin-topexpect-ignore-ppx" ocaml_version mapper
