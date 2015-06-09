open Core.Std

type t = {
  file: string;
  html: string;
} with sexp

type ts = (string * t) list with sexp
