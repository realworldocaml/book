open! Core

module type Io = sig
  type 'a t
  type 'a fmt
  type out_channel

  include Monad.S with type 'a t := 'a t

  val output_string : out_channel -> string -> unit
  val output : out_channel -> buf:bytes -> pos:int -> len:int -> unit
  val stderr : out_channel
  val stdout : out_channel
  val flush : out_channel -> unit t
  val print_string : string -> unit
  val fprintf : attrs:string -> out_channel -> 'a fmt -> 'a
  val fold_left : 'a List.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val stdout_isatty : unit -> bool t
  val capable : unit -> bool t
end

module type S = sig
  type 'a io
  type 'a io_fmt
  type out_channel

  (** Handling of ansi codes. *)
  module Ansi : sig
    val kill_line : unit -> unit
    val erase_to_end_of_screen : unit -> unit
    val erase_to_start_of_screen : unit -> unit
    val erase_all : unit -> unit
    val bell : unit -> unit
    val home_cursor : unit -> unit
    val cursor_up : unit -> unit
    val cursor_down : unit -> unit
    val cursor_backward : unit -> unit
    val cursor_forward : unit -> unit
    val save_cursor : unit -> unit
    val unsave_cursor : unit -> unit

    type attr = Ansi_kernel.Attr.t

    val printf : attr list -> 'a io_fmt -> 'a
    val eprintf : attr list -> 'a io_fmt -> 'a
    val output_string : attr list -> out_channel -> string -> unit io
    val output : attr list -> out_channel -> Bytes.t -> int -> int -> unit io

    (* Create string with embedded formatting codes *)

    val string_with_attr : attr list -> string -> string
  end

  val is_color_tty : unit -> bool io

  (** The width in characters of the current output. Returns [`Not_a_tty] if
      stdout is not connected to a tty.*)
  val width : unit -> [ `Cols of int | `Not_a_tty | `Not_available ] io

  (** print a list in a columnize way (like the output of ls) *)
  val print_list : out_channel -> (string * Ansi.attr list) list -> unit io
end

(** Color printing in terminals  *)
module type Console = sig
  module type Io = Io
  module type S = S

  module Make (Io : Io) :
    S
    with type 'a io := 'a Io.t
     and type 'a io_fmt := 'a Io.fmt
     and type out_channel := Io.out_channel

  include
    S
    with type 'a io := 'a
     and type 'a io_fmt := ('a, Out_channel.t, unit) format
     and type out_channel := Out_channel.t
end
