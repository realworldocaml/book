let _ = Bisect_ppx.Register.conditional := true

let () = Ppxlib.Driver.run_as_ppx_rewriter ()
