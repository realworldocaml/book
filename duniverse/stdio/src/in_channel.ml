open! Base
open! Import

type t = Caml.in_channel

let equal (t1 : t) t2 = phys_equal t1 t2

let seek   = Caml.LargeFile.seek_in
let pos    = Caml.LargeFile.pos_in
let length = Caml.LargeFile.in_channel_length

let stdin = Caml.stdin

let create ?(binary = true) file =
  let flags = [Open_rdonly] in
  let flags = if binary then Open_binary :: flags else flags in
  Caml.open_in_gen flags 0o000 file
;;

let close = Caml.close_in

let with_file ?binary file ~f = Exn.protectx (create ?binary file) ~f ~finally:close

let may_eof f = try Some (f ()) with End_of_file -> None

let input t ~buf ~pos ~len = Caml.input t buf pos len
let really_input t ~buf ~pos ~len =
  may_eof (fun () -> Caml.really_input t buf pos len)
let really_input_exn t ~buf ~pos ~len =
  Caml.really_input t buf pos len
let input_byte t = may_eof (fun () -> Caml.input_byte t)
let input_char t = may_eof (fun () -> Caml.input_char t)
let input_binary_int t = may_eof (fun () -> Caml.input_binary_int t)
let unsafe_input_value t = may_eof (fun () -> Caml.input_value t)
let input_buffer t buf ~len = may_eof (fun () -> Caml.Buffer.add_channel buf t len)

let set_binary_mode = Caml.set_binary_mode_in

let input_all t =
  (* We use 65536 because that is the size of OCaml's IO buffers. *)
  let buf_size = 65536 in
  let buf = Bytes.create buf_size in
  let buffer = Buffer.create buf_size in
  let rec loop () =
    let len = input t ~buf ~pos:0 ~len:(Bytes.length buf) in
    if len > 0 then begin
      Buffer.add_subbytes buffer buf ~pos:0 ~len;
      loop ();
    end
  in
  loop ();
  Buffer.contents buffer;
;;

let trim ~fix_win_eol line =
  if fix_win_eol then begin
    let len = String.length line in
    if len > 0
    && Char.equal (String.get line (len - 1)) '\r'
    then String.sub line ~pos:0 ~len:(len - 1)
    else line
  end
  else line

let input_line ?(fix_win_eol = true) t =
  match may_eof (fun () -> Caml.input_line t) with
  | None -> None
  | Some line -> Some (trim ~fix_win_eol line)
;;

let input_line_exn ?(fix_win_eol = true) t =
  let line = Caml.input_line t in
  trim ~fix_win_eol line

let fold_lines ?fix_win_eol t ~init ~f =
  let rec loop ac =
    match input_line ?fix_win_eol t with
    | None -> ac
    | Some line -> loop (f ac line)
  in
  loop init
;;

let input_lines ?fix_win_eol t =
  List.rev
    (fold_lines ?fix_win_eol t ~init:[] ~f:(fun lines line -> line :: lines))
;;

let iter_lines ?fix_win_eol t ~f =
  fold_lines ?fix_win_eol t ~init:() ~f:(fun () line -> f line)
;;

let read_lines fname = with_file fname ~f:input_lines

let read_all fname = with_file fname ~f:input_all
