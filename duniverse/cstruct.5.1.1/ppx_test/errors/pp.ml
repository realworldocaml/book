(* we can just use exit from 4.07 onwards, but 4.06
 * and earlier executed at_exit recursively *)
external sys_exit : int -> 'a = "caml_sys_exit"
let () = at_exit (fun () -> sys_exit 0)

#if OCAML_VERSION >= (4,8,0)
let () = Clflags.(error_style := Some Short)
#endif

let () = Migrate_parsetree.Driver.run_main ()
