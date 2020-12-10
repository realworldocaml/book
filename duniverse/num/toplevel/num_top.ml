(* Taken from findlib.  Findlib is copyright 1999 by Gerd Stolpmann and
  distributed under the terms given in file LICENSE-findlib in this
  directory. *)

(*
  Printers for types defined in the "num" library. Meant to be used as printers
  in the ocaml toplevel. See num_top.mli.

  Copyright (C) 2003  Stefano Zacchiroli <zack@debian.org>

  Released under the same terms as findlib.

  Simplified implementation for OCaml 4 and up, Xavier.Leroy@inria.fr

*)

open Longident

let install_num_printer s =
  Topdirs.dir_install_printer Format.err_formatter
                              (Ldot(Lident "Num_top_printers", s))

let _ =
  List.iter install_num_printer Num_top_printers.printers
