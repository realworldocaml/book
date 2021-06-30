include Console_intf


(** Color printing in terminals  *)
open Core

open Poly

module Make (Io : Io) = struct
  (* http://www.termsys.demon.co.uk/vtansi.htm *)
  module Ansi = struct
    let kill_line () = Io.print_string "\027[2K"
    let erase_to_end_of_screen () = Io.print_string "\027[J"
    let erase_to_start_of_screen () = Io.print_string "\027[1J"
    let erase_all () = Io.print_string "\027[2J"
    let bell () = Io.print_string "\007"
    let home_cursor () = Io.print_string "\027[0G"
    let cursor_up () = Io.print_string "\027[A"
    let cursor_down () = Io.print_string "\027[B"
    let cursor_backward () = Io.print_string "\027[D"
    let cursor_forward () = Io.print_string "\027[C"
    let save_cursor () = Io.print_string "\027[s"
    let unsave_cursor () = Io.print_string "\027[u"

    module All_attr = Ansi_kernel.With_all_attrs
    module Attr = Ansi_kernel.Attr

    type attr = Attr.t

    let string_with_attr style string =
      if style = []
      then string
      else
        String.concat
          [ All_attr.list_to_string (style :> All_attr.t list)
          ; string
          ; All_attr.list_to_string [ `Reset ]
          ]
    ;;

    let output (style : attr list) oc s start len =
      let open Io.Let_syntax in
      let%bind capable = Io.capable () in
      if capable && style <> []
      then (
        Io.output_string oc (All_attr.list_to_string (style :> All_attr.t list));
        Io.output oc ~buf:s ~pos:start ~len;
        Io.output_string oc (All_attr.list_to_string [ `Reset ]);
        Io.flush oc)
      else Io.return (Io.output oc ~buf:s ~pos:start ~len)
    ;;

    let output_string (style : attr list) oc s =
      let open Io.Let_syntax in
      let%bind capable = Io.capable () in
      if capable && style <> []
      then (
        Io.output_string oc (All_attr.list_to_string (style :> All_attr.t list));
        Io.output_string oc s;
        Io.output_string oc (All_attr.list_to_string [ `Reset ]);
        Io.flush oc)
      else Io.return (Io.output_string oc s)
    ;;

    let eprintf style fmt =
      Io.fprintf
        ~attrs:(All_attr.list_to_string (style :> All_attr.t list))
        Io.stderr
        fmt
    ;;

    let printf style fmt =
      Io.fprintf
        ~attrs:(All_attr.list_to_string (style :> All_attr.t list))
        Io.stdout
        fmt
    ;;
  end

  let is_color_tty () = Io.capable ()

  module Columnize (In : sig
      type t

      val length : t -> int
    end) : sig
    val iter
      :  middle:(sep:In.t -> In.t -> int -> unit Io.t)
      -> last:(In.t -> int -> unit Io.t)
      -> sep:In.t
      -> In.t list
      -> int
      -> unit Io.t
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
      let open Io.Let_syntax in
      match line, padding with
      | [ v ], len :: _ -> last ~acc v (len - In.length v)
      | h :: t, len :: tlen ->
        let%bind () = middle ~acc ~sep h (len - In.length h) in
        fold_line ~middle ~last sep acc tlen t
      | _ -> assert false
    ;;

    let fold ~init ~middle ~last ~sep l max_len =
      if l = []
      then Io.return init
      else (
        let a = Array.of_list l in
        let columns = find_dim a ~sep_len:(In.length sep) max_len in
        let res = columnize a (Array.length columns) in
        Io.fold_left
          res
          ~f:(fun acc line ->
            fold_line ~middle ~last sep acc (Array.to_list columns) line)
          ~init)
    ;;

    let iter ~middle ~last =
      fold ~init:() ~last:(fun ~acc:() -> last) ~middle:(fun ~acc:() -> middle)
    ;;
  end

  let width () =
    let open Io.Let_syntax in
    match Linux_ext.get_terminal_size with
    | Result.Error _ -> Io.return `Not_available
    | Result.Ok get_size ->
      if%map Io.stdout_isatty () then `Cols (snd (get_size `Controlling)) else `Not_a_tty
  ;;

  let print_list oc l : unit Io.t =
    let open Io.Let_syntax in
    match%bind (width () :> [ `Cols of int | `Not_a_tty | `Not_available ] Io.t) with
    | `Not_a_tty | `Not_available ->
      List.iter l ~f:(fun (s, _) -> Io.print_string (s ^ "\n"));
      return ()
    | `Cols cols ->
      let print_styled (s, style) = Ansi.output_string style oc s in
      let sep = "  ", [] in
      let last v _ =
        let%map () = print_styled v in
        Io.output_string oc "\n"
      and middle ~sep v pad_len =
        let%bind () = print_styled v in
        Io.output_string oc (String.make pad_len ' ');
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
end

include Make (struct
    include Monad.Ident

    type 'a fmt = ('a, Out_channel.t, unit) format
    type out_channel = Out_channel.t

    let output_string = Out_channel.output_string
    let output = Out_channel.output
    let stderr = Out_channel.stderr
    let stdout = Out_channel.stdout
    let flush = Core.Out_channel.flush
    let print_string = print_string

    (* if it's good enough for git then it's good enough for us... *)
    let capable =
      lazy
        (Unix.isatty Unix.stdout
         &&
         match Sys.getenv "TERM" with
         | Some "dumb" | None -> false
         | Some _ -> true)
    ;;

    let capable () = Lazy.force capable

    let fprintf ~attrs channel fmt =
      if capable () && not (String.is_empty attrs)
      then Printf.fprintf channel ("%s" ^^ fmt ^^ "\027[0m%!") attrs
      else Printf.fprintf channel (fmt ^^ "%!")
    ;;

    let fold_left = List.fold_left
    let stdout_isatty () = Unix.isatty Unix.stdout
  end)
