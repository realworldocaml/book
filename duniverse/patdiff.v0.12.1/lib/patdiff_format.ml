open! Core
open! Import

module Color = struct
  module RGB6 : sig
    type t = private
      { r : int
      ; g : int
      ; b : int
      }
    [@@deriving compare, sexp]

    val create_exn : r:int -> g:int -> b:int -> t
  end = struct
    type t =
      { r : int
      ; g : int
      ; b : int
      }
    [@@deriving compare, sexp]

    let create_exn ~r ~g ~b =
      let check x = 0 <= x && x < 6 in
      if not (check r && check g && check b)
      then invalid_arg "RGB6 (r, g, b) -- expected (0 <= r, g, b < 6)";
      { r; g; b }
    ;;
  end

  module Gray24 : sig
    type t = private { level : int } [@@deriving compare, sexp]

    val create_exn : level:int -> t
  end = struct
    type t = { level : int } [@@deriving compare, sexp]

    let create_exn ~level =
      if not (0 <= level && level < 24)
      then invalid_arg "Gray24 level -- expected (0 <= level < 24)";
      { level }
    ;;
  end

  module T = struct
    type t =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White
      | Default
      | Gray
      | Bright_black
      | Bright_red
      | Bright_green
      | Bright_yellow
      | Bright_blue
      | Bright_magenta
      | Bright_cyan
      | Bright_white
      | RGB6 of RGB6.t
      | Gray24 of Gray24.t
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let rgb6_exn (r, g, b) = RGB6 (RGB6.create_exn ~r ~g ~b)
  let gray24_exn level = Gray24 (Gray24.create_exn ~level)
end

module Style = struct
  module T = struct
    type t =
      | Bold
      | Underline
      | Emph
      | Blink
      | Dim
      | Inverse
      | Hide
      | Reset
      | Foreground of Color.t
      | Fg of Color.t
      | Background of Color.t
      | Bg of Color.t
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

(* A rule consists of a styled prefix, a styled suffix, and a style. Rules
   are applied to strings using functions defined in Output_ops.
*)
module Rule = struct
  (* An annex is either a prefix or a suffix. *)
  module Annex = struct
    type t =
      { text : string
      ; styles : Style.t list
      }
    [@@deriving fields, sexp_of]

    let create ?(styles = []) text = { text; styles }
    let blank = create ""
    let strip_styles t = { t with styles = [] }
  end

  type t =
    { pre : Annex.t
    ; suf : Annex.t
    ; styles : Style.t list
    ; name : string
    }
  [@@deriving fields, sexp_of]

  (* Rule creation: Most rules have a style, and maybe a prefix. For
     instance, a line_new rule might have a bold "+" prefix and a green
     style.
  *)
  let create ?(pre = Annex.blank) ?(suf = Annex.blank) styles ~name =
    { pre; suf; styles; name }
  ;;

  let blank = create []

  let unstyled_prefix text ~name =
    let rule = blank ~name in
    { rule with pre = Annex.create text }
  ;;

  let strip_styles t =
    let f f field = f (Field.get field t) in
    Fields.map
      ~pre:(f Annex.strip_styles)
      ~suf:(f Annex.strip_styles)
      ~styles:(f (const []))
      ~name:(f Fn.id)
  ;;
end

(* Rules are configured in the configuration file.
   Default values are provided in Configuration.
*)
module Rules = struct
  type t =
    { line_same : Rule.t
    ; line_old : Rule.t
    ; line_new : Rule.t
    ; line_unified : Rule.t
    ; word_same_old : Rule.t
    ; word_same_new : Rule.t
    ; word_same_unified : Rule.t
    ; word_old : Rule.t
    ; word_new : Rule.t
    ; hunk : Rule.t
    ; header_old : Rule.t
    ; header_new : Rule.t
    }
  [@@deriving fields, sexp_of]

  let inner_line_change ~name text color =
    let style = Style.[ Fg color ] in
    let pre = Rule.Annex.create ~styles:Style.[ Bold; Fg color ] text in
    Rule.create ~pre style ~name
  ;;

  let line_unified =
    let pre = Rule.Annex.create ~styles:Style.[ Bold; Fg Color.Yellow ] "!|" in
    Rule.create ~pre [] ~name:"line_unified"
  ;;

  let word_change ~name color = Rule.create Style.[ Fg color ] ~name

  let default =
    let open Rule in
    { line_same = unstyled_prefix ~name:"line_same" "  "
    ; line_old = inner_line_change ~name:"line_old" "-|" Color.Red
    ; line_new = inner_line_change ~name:"line_new" "+|" Color.Green
    ; line_unified
    ; word_same_old = blank ~name:"word_same_old"
    ; word_same_new = blank ~name:"word_same_new"
    ; word_same_unified = blank ~name:"word_same_unified"
    ; word_old = word_change ~name:"word_old" Color.Red
    ; word_new = word_change ~name:"word_new" Color.Green
    ; hunk = blank ~name:"hunk"
    ; header_old = blank ~name:"hunk"
    ; header_new = blank ~name:"hunk"
    }
  ;;

  let strip_styles t =
    let f field = Rule.strip_styles (Field.get field t) in
    Fields.map
      ~line_same:f
      ~line_old:f
      ~line_new:f
      ~line_unified:f
      ~word_same_old:f
      ~word_same_new:f
      ~word_same_unified:f
      ~word_old:f
      ~word_new:f
      ~hunk:f
      ~header_old:f
      ~header_new:f
  ;;
end

module Location_style = struct
  type t =
    | Diff
    | Omake
  [@@deriving bin_io, compare, enumerate, sexp]

  let to_string = function
    | Diff -> "diff"
    | Omake -> "omake"
  ;;

  let of_string = function
    | "diff" -> Diff
    | "omake" -> Omake
    | other -> failwiths "invalid location style" other [%sexp_of: string]
  ;;

  let sprint t (hunk : string Patience_diff.Hunk.t) ~prev_filename ~rule =
    match t with
    | Diff ->
      rule
        (sprintf
           "-%i,%i +%i,%i"
           hunk.prev_start
           hunk.prev_size
           hunk.next_start
           hunk.next_size)
    (* omake locations must be parseable, so we can't let the user config insert
       arbitrary prefixes and suffixes and ANSI color rubbish. *)
    | Omake ->
      (* Print line number of first difference, skipping past context lines. *)
      let prev_start =
        with_return (fun r ->
          List.fold hunk.ranges ~init:hunk.prev_start ~f:(fun init -> function
            | Same s -> init + Array.length s
            | Prev _ | Next _ | Replace _ | Unified _ -> r.return init))
      in
      error_message_start ~file:prev_filename ~line:prev_start
  ;;
end
