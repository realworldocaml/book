open! Core
open! Async

include module type of struct include Async.Log end

module Console : sig
  (** returns a [Log.Output.t] given optional styles (i.e. values of type [Ansi.t list])
      for each of the [`Debug], [`Info], and [`Error] log levels. The default styling is
      to display debug messages in yellow, error messages in red, and info messages
      without any additional styling.

      [create] doesn't take a [format] argument because colorized output should be read
      by humans.
  *)
  val output :
    ?debug:Console.Ansi.attr list
    -> ?info:Console.Ansi.attr list
    -> ?error:Console.Ansi.attr list
    -> Writer.t
    -> Log.Output.t

  module Blocking : sig
    (** as [output] but for use with non-async logs *)
    val output :
      ?debug:Console.Ansi.attr list
      -> ?info:Console.Ansi.attr list
      -> ?error:Console.Ansi.attr list
      -> Out_channel.t
      -> Log.Blocking.Output.t
  end
end

module Syslog : sig
  (** [output ()] return a Log.Output.t for use with Async.Log. *)
  val output
    :  ?id:string                                    (** default is [Sys.argv.(0)] *)
    -> ?options:Syslog.Open_option.t list  (** default is [[PID; CONS]] *)
    -> ?facility:Syslog.Facility.t         (** default is [USER] *)
    -> unit
    -> Log.Output.t

  module Blocking : sig
    val output : unit -> Log.Blocking.Output.t
  end
end
