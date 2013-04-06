open Core.Std

type part =
| Basic
| Practical
| Advanced
| Appendix
| Preface
with sexp

type chapter = {
  part: part;
  name: string;
  file: string;
  public: bool with default(true);
  title: string sexp_option;
  note: string;
} with sexp
