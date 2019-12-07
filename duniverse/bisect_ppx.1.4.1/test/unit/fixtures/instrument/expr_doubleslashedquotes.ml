type t =
  | Anthony
  | Caesar

let message = function
  | Anthony -> "foo\\"
  | Caesar  -> "\\bar"
