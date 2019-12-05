(* This example relies on Ocamlbuild version 0.9.0 (specifically on PR#6794).
   Otherwise compiling bindings_stubs_gen.c, Step 4, will fail because the
   package information isn't passed to "ocamlfind ocamlc". *)
open Ocamlbuild_plugin

let () =
  let additional_rules = function
    | Before_hygiene  -> ()
    | After_hygiene   -> ()
    | Before_options  -> ()
    | After_options   -> ()
    | Before_rules    -> ()
    | After_rules     ->

      (* Generate stubs. Steps 1, 2, & 3 of Makefile (1 & 2 via built-in rules).
        ML -> C *)
      rule "cstubs: x_c_gen.native -> x_stubs_gen.c"
        ~dep:"%_c_gen.native"
        ~prod:"%_stubs_gen.c"
        (fun env _build -> Cmd (A (env "./%_c_gen.native")));

      (* Step 4. OCamlbuild (nor ocamlc/ocamlopt) has a built in rule for
         linking executables from C. Call out to 'cc'. *)
      rule "stub_gen 1: x_stubs_gen.o -> x_stubs_gen"
        ~dep:"%_stubs_gen.o"
        ~prod:"%_stubs_gen"
        (fun env _build ->
          Cmd (S [ A "cc"; A "-o"; A (env "%_stubs_gen"); A (env "%_stubs_gen.o") ]));

      (* Step 5. Generate ml stubs.  C -> ML  *)
      rule "stubs_gen 2: x_stubs_gen -> x_stubs.ml"
        ~dep:"%_stubs_gen"
        ~prod:"%_stubs.ml"
        (fun env _build -> Cmd (S[A (env "./%_stubs_gen"); Sh">"; A (env "%_stubs.ml")]));

  in
  dispatch additional_rules
