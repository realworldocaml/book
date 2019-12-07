open! Base
open Ppxlib

(* Avoids adding timing definitions in toplevel contexts like utop (which uses the
   filename "//toplevel//") and toplevel expect tests (which have ".mlt" files).

   This is necessary to preserve interactions like the following:
   {v
     utop # 1;;
     - : int = 1
   v}

   To print the type and value of a result, the toplevel has to detect lone expressions.
   Wrapping an expression in module timing definitions spoils this special case. So we
   turn off this rewriter in toplevel contexts. *)
let is_ocaml_file string =
  String.is_suffix string ~suffix:".ml" || String.is_suffix string ~suffix:".mll"
;;

let enclose_impl = function
  | Some (loc : Location.t)
    when is_ocaml_file loc.loc_start.pos_fname ->
    ( [%str
      let () =
        Ppx_module_timer_runtime.record_start Ppx_module_timer_runtime.__MODULE__
      ;;]
    , [%str
      let () =
        Ppx_module_timer_runtime.record_until Ppx_module_timer_runtime.__MODULE__
      ;;] )
  | _ -> [], []
;;

let () = Driver.register_transformation "module_timer" ~enclose_impl
