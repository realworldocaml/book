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
  note: string;
} with sexp
