open Core.Std

type part =
  |Basic
  |Practical
  |Advanced
and chapter = {
  part: part;
  name: string;
  file: string;
  public: bool;
} with sexp
