val printers : string list
val eval_string :
  ?print_outcome:bool -> ?err_formatter:Format.formatter -> string -> bool
val install_printers : string list -> bool
