(** Non-blocking, Async-friendly print functions. *)

val print_char : char -> unit
val prerr_char : char -> unit
val print_string : string -> unit
val prerr_string : string -> unit
val print_int : int -> unit
val prerr_int : int -> unit
val print_float : float -> unit
val prerr_float : float -> unit
val print_endline : string -> unit
val prerr_endline : string -> unit
val print_newline : unit -> unit
val prerr_newline : unit -> unit
val print_s : ?mach:unit -> Sexplib.Sexp.t -> unit
val printf : ('a, unit, string, unit) format4 -> 'a
val fprintf : Writer.t -> ('a, unit, string, unit) format4 -> 'a
val eprintf : ('a, unit, string, unit) format4 -> 'a
val eprint_s : ?mach:unit -> Sexplib.Sexp.t -> unit
