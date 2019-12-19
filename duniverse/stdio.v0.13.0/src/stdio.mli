open! Base
open! Import

module In_channel  = In_channel
module Out_channel = Out_channel

(** Same as {!In_channel.stdin} *)
val stdin : In_channel.t

(** Same as {!Out_channel.stdout} *)
val stdout : Out_channel.t

(** Same as {!Out_channel.stderr} *)
val stderr : Out_channel.t

(** Same as {!Out_channel.printf} *)
val printf : ('a, Out_channel.t, unit) format -> 'a

(** Same as {!Out_channel.print_s} *)
val print_s : ?mach : unit -> Sexp.t -> unit

(** Same as {!Out_channel.eprint_s} *)
val eprint_s : ?mach : unit -> Sexp.t -> unit

(** Same as {!Out_channel.eprintf} *)
val eprintf : ('a, Out_channel.t, unit) format -> 'a

(** Same as {!Out_channel.print_string} *)
val print_string : string -> unit

(** Same as {!Out_channel.print_endline} *)
val print_endline : string -> unit

(** Same as {!Out_channel.prerr_endline} *)
val prerr_endline : string -> unit
