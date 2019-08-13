(* Although this seems pointless, separating Signal_manager (whose .mli references the
   Signal module) from Raw_signal_manager prevents a dependency cycle when using
   ocamlbuild. *)

include Raw_signal_manager
