
(** Color printing in terminals  *)
open Core

open Poly

(* http://www.termsys.demon.co.uk/vtansi.htm *)
module Ansi = struct
  let kill_line () = print_string "\027[2K"
  let bell () = print_string "\007"
  let home_cursor () = print_string "\027[0G"
  let save_cursor () = print_string "\027[s"
  let unsave_cursor () = print_string "\027[u"

  (* if it's good enough for git then it's good enough for us... *)
  let capable =
    lazy
      (Unix.isatty Unix.stdout
       &&
       match Sys.getenv "TERM" with
       | Some "dumb" | None -> false
       | Some _ -> true)
  ;;

  module Attr = struct
    type color =
      [ `Black
      | `Red
      | `Green
      | `Yellow
      | `Blue
      | `Magenta
      | `Cyan
      | `White
      ]

    type attr =
      [ `Reset
      | `Bright
      | `Dim
      | `Underscore
      | `Blink
      | `Reverse
      | `Hidden
      ]

    type t =
      [ attr
      | color
      | `Bg of color
      ]

    let attr_to_int : attr -> int = function
      | `Reset -> 0
      | `Bright -> 1
      | `Dim -> 2
      | `Underscore -> 4
      | `Blink -> 5
      | `Reverse -> 7
      | `Hidden -> 8
    ;;

    let fg_to_int : color -> int = function
      | `Black -> 30
      | `Red -> 31
      | `Green -> 32
      | `Yellow -> 33
      | `Blue -> 34
      | `Magenta -> 35
      | `Cyan -> 36
      | `White -> 37
    ;;

    let bg_to_int : color -> int = function
      | `Black -> 40
      | `Red -> 41
      | `Green -> 42
      | `Yellow -> 43
      | `Blue -> 44
      | `Magenta -> 45
      | `Cyan -> 46
      | `White -> 47
    ;;

    let to_int : t -> int = function
      | `Bg v -> bg_to_int v
      | #color as v -> fg_to_int v
      | #attr as v -> attr_to_int v
    ;;

    let list_to_string : t list -> string = function
      | [] -> ""
      | l ->
        Printf.sprintf
          "\027[%sm"
          (String.concat ~sep:";" (List.map l ~f:(fun att -> string_of_int (to_int att))))
    ;;
  end

  type color = Attr.color

  type attr =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | color
    | `Bg of color
    ]

  let string_with_attr style string =
    if style = []
    then string
    else
      String.concat
        [ Attr.list_to_string (style :> Attr.t list)
        ; string
        ; Attr.list_to_string [ `Reset ]
        ]
  ;;

  let output (style : attr list) oc s start len =
    if Lazy.force capable && style <> []
    then (
      Out_channel.output_string oc (Attr.list_to_string (style :> Attr.t list));
      Out_channel.output oc ~buf:s ~pos:start ~len;
      Out_channel.output_string oc (Attr.list_to_string [ `Reset ]);
      Out_channel.flush oc)
    else Out_channel.output oc ~buf:s ~pos:start ~len
  ;;

  let output_string (style : attr list) oc s =
    if Lazy.force capable && style <> []
    then (
      Out_channel.output_string oc (Attr.list_to_string (style :> Attr.t list));
      Out_channel.output_string oc s;
      Out_channel.output_string oc (Attr.list_to_string [ `Reset ]);
      Out_channel.flush oc)
    else Out_channel.output_string oc s
  ;;

  let fprintf (style : attr list) channel fmt =
    if Lazy.force capable && style <> []
    then
      Printf.fprintf
        channel
        ("%s" ^^ fmt ^^ "\027[0m%!")
        (Attr.list_to_string (style :> Attr.t list))
    else Printf.fprintf channel (fmt ^^ "%!")
  ;;

  let eprintf style fmt = fprintf style stderr fmt
  let printf style fmt = fprintf style stdout fmt
end

let is_color_tty () = Lazy.force Ansi.capable

module Columnize (In : sig
    type t

    val length : t -> int
  end) : sig
  val iter
    :  middle:(sep:In.t -> In.t -> int -> unit)
    -> last:(In.t -> int -> unit)
    -> sep:In.t
    -> In.t list
    -> int
    -> unit
end = struct
  let lines columns a = ((Array.length a - 1) / columns) + 1

  (** Size of an array printed out with this column configuration
      (lines*chars per column)
  *)
  let dim columns a =
    let lines = lines columns a in
    let rec loop cnt current acc =
      if cnt = Array.length a
      then List.rev (current :: acc)
      else if cnt mod lines = 0
      then loop (cnt + 1) (In.length a.(cnt)) (current :: acc)
      else loop (cnt + 1) (max (In.length a.(cnt)) current) acc
    in
    lines, loop 1 (In.length a.(0)) []
  ;;

  let rec line_len ~sep_len acc = function
    | [] -> acc
    | [ v ] -> acc + v
    | h :: t -> line_len ~sep_len (acc + sep_len + h) t
  ;;

  let find_dim ~sep_len a max_len =
    let rec loop lines cols cnt =
      let nlines, ncols = dim (cnt + 1) a in
      if nlines > lines
      || lines = 1
      (* we are not gaining in vertical space anymore *)
      || line_len ~sep_len 0 ncols > max_len
      (* we are overflowing *)
      then Array.of_list cols
      else loop nlines ncols (cnt + 1)
    in
    let lines, cols = dim 1 a in
    loop lines cols 1
  ;;

  let columnize a columns =
    let lines = lines columns a in
    let res = ref [] in
    for i = lines - 1 downto 0 do
      let line_acc = ref [] in
      for j = columns - 1 downto 0 do
        let pos = i + (j * lines) in
        if pos < Array.length a then line_acc := a.(pos) :: !line_acc
      done;
      res := !line_acc :: !res
    done;
    !res
  ;;

  let rec fold_line ~middle ~last sep acc padding line =
    match line, padding with
    | [ v ], len :: _ -> last ~acc v (len - In.length v)
    | h :: t, len :: tlen ->
      fold_line ~middle ~last sep (middle ~acc ~sep h (len - In.length h)) tlen t
    | _ -> assert false
  ;;

  let fold ~init ~middle ~last ~sep l max_len =
    if l = []
    then init
    else (
      let a = Array.of_list l in
      let columns = find_dim a ~sep_len:(In.length sep) max_len in
      let res = columnize a (Array.length columns) in
      List.fold_left
        res
        ~f:(fun acc line -> fold_line ~middle ~last sep acc (Array.to_list columns) line)
        ~init)
  ;;

  let iter ~middle ~last =
    fold ~init:() ~last:(fun ~acc:() -> last) ~middle:(fun ~acc:() -> middle)
  ;;
end

let width () =
  match Linux_ext.get_terminal_size with
  | Result.Error _ -> `Not_available
  | Result.Ok _ when not (Unix.isatty Unix.stdout) -> `Not_a_tty
  | Result.Ok get_size -> `Cols (snd (get_size `Controlling))
;;

let print_list oc l =
  match (width () :> [ `Cols of int | `Not_a_tty | `Not_available ]) with
  | `Not_a_tty | `Not_available -> List.iter l ~f:(fun (s, _) -> print_endline s)
  | `Cols cols ->
    let print_styled (s, style) = Ansi.output_string style oc s in
    let sep = "  ", [] in
    let last v _ =
      print_styled v;
      Out_channel.output_string oc "\n"
    and middle ~sep v pad_len =
      print_styled v;
      Out_channel.output_string oc (String.make pad_len ' ');
      print_styled sep
    in
    let module Col =
      Columnize (struct
        type t = string * Ansi.attr list

        let length (s, _) = String.length s
      end)
    in
    Col.iter ~sep ~last ~middle l cols
;;
