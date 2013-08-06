
module type Base_logger : sig
  type t
  val log_sexp : t -> Time.t -> Sexp.t -> unit
  val log_line : t -> Time.t -> string -> unit
end

module type Zero_conf_logger : sig
  include Base_logger
  val create : unit -> t
end

module type Filename_conf_logger : sig
  include Base_logger
  val create : file:string -> t
end
