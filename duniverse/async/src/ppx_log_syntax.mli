(** @open *)
include
  Ppx_log_types.S
  with type t = Async_unix.Log.t
   and type return_type = unit
   and type Global.return_type = unit

(** If you wish you prevent global logging with ppx_log, you can open this module and any
    use of global logging will return a warning type
    `Do_not_use_because_it_will_not_log (and will not actually log anything). *)
module No_global : sig
  module Ppx_log_syntax :
    Ppx_log_types.S
    with type t = Async_unix.Log.t
     and type return_type = unit
     and type Global.return_type = [ `Do_not_use_because_it_will_not_log ]
end
