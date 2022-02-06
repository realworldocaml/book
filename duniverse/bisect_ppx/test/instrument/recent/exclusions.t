  $ echo > .ocamlformat
  $ echo "(lang dune 2.9)" > dune-project
  $ cat > dune <<'EOF'
  > (executables
  >  (names not_excluded excluded_1)
  >  (modes byte)
  >  (ocamlc_flags -dsource)
  >  (instrumentation
  >   (backend bisect_ppx --exclusions bisect.exclude) (deps bisect.exclude)))
  > EOF
  $ cat > bisect.exclude <<'EOF'
  > file "excluded_1.ml"
  > EOF
  $ cat > not_excluded.ml <<'EOF'
  > let _f () = ()
  > EOF
  $ cat > excluded_1.ml <<'EOF'
  > let _f () = ()
  > EOF
  $ dune build ./not_excluded.bc --instrument-with bisect_ppx 2>&1 | tail -n +2
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = []
    }]
  [@@@ocaml.text "/*"]
  module Bisect_visit___not_excluded___ml =
    struct
      let ___bisect_visit___ =
        let points = [|12|] in
        let `Visit visit =
          Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
            ~filename:"not_excluded.ml" ~points ~bisect_sigterm:false in
        visit
      let ___bisect_post_visit___ point_index result =
        ___bisect_visit___ point_index; result
    end
  open Bisect_visit___not_excluded___ml
  [@@@ocaml.text "/*"]
  let _f () = ___bisect_visit___ 0; ()

  $ dune build ./excluded_1.bc --instrument-with bisect_ppx 2>&1 | tail -n +2
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = []
    }]
  let _f () = ()

