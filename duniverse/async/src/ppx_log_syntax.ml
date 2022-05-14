open Base
open Async_unix

module T = struct
  type t = Async_unix.Log.t
  type return_type = unit

  let would_log = Log.would_log

  let sexp ?level ?pos t msg =
    let tags =
      Option.map pos ~f:(fun pos -> [ "pos", Source_code_position.to_string pos ])
    in
    Log.sexp ?level ?tags t msg
  ;;

  let default = ()
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log = Log.Global.would_log

  let sexp ?level ?pos msg =
    let tags =
      Option.map pos ~f:(fun pos -> [ "pos", Source_code_position.to_string pos ])
    in
    Log.Global.sexp ?level ?tags msg
  ;;
end

module No_global = struct
  module Ppx_log_syntax = struct
    include T

    module Global = struct
      type return_type = [ `Do_not_use_because_it_will_not_log ]

      let default = `Do_not_use_because_it_will_not_log
      let would_log _ = false
      let sexp ?level:_ ?pos:_ _ = `Do_not_use_because_it_will_not_log
    end
  end
end
