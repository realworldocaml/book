open! Core

val warn_if_no_trailing_newline_in_both_default : bool

type t = private
  { output : Patdiff_core.Output.t
  ; rules : Patdiff_core.Format.Rules.t
  ; ext_cmp : string option
  ; float_tolerance : Percent.t option
  ; produce_unified_lines : bool
  ; unrefined : bool
  ; keep_ws : bool
  ; split_long_lines : bool
  ; interleave : bool
  ; assume_text : bool
  ; context : int
  ; line_big_enough : int
  ; word_big_enough : int
  ; shallow : bool
  ; quiet : bool
  ; double_check : bool
  ; mask_uniques : bool
  ; prev_alt : string option
  ; next_alt : string option
  ; location_style : Patdiff_core.Format.Location_style.t
  ; warn_if_no_trailing_newline_in_both : bool
  }
[@@deriving fields]

include Invariant.S with type t := t

val override
  :  ?output:Patdiff_core.Output.t
  -> ?rules:Patdiff_core.Format.Rules.t
  -> ?ext_cmp:string option
  -> ?float_tolerance:Percent.t option
  -> ?produce_unified_lines:bool
  -> ?unrefined:bool
  -> ?keep_ws:bool
  -> ?split_long_lines:bool
  -> ?interleave:bool
  -> ?assume_text:bool
  -> ?context:int
  -> ?line_big_enough:int
  -> ?word_big_enough:int
  -> ?shallow:bool
  -> ?quiet:bool
  -> ?double_check:bool
  -> ?mask_uniques:bool
  -> ?prev_alt:string option
  -> ?next_alt:string option
  -> ?location_style:Patdiff_core.Format.Location_style.t
  -> ?warn_if_no_trailing_newline_in_both:bool
  -> t
  -> t

val load : ?quiet_errors:bool -> string -> t option
val dark_bg : t Lazy.t
val light_bg : t Lazy.t

module Config : sig
  module V1 : sig
    type t
  end

  module V0 : sig
    type t

    val to_v1 : t -> V1.t
  end

  type t = V1.t [@@deriving sexp]
end

val parse : Config.t -> t

(** A default config, suitable for writing to [~/.patdiff] or passing to {[

      fun default -> parse ([%of_sexp: Config.t] (Sexp.of_string default))

    ]}. *)
val default : t Lazy.t

val save_default : filename:string -> unit

(** Reads a config from [filename], which by default is [~/.patdiff]. If [~filename:""] is
    passed or [filename] cannot be read, returns [default]. *)
val get_config : ?filename:string -> unit -> t
