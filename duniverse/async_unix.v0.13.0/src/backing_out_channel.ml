open! Core
open! Import

type output_chars = bigstring -> len:int -> unit

type t =
  { mutable bigstring_buf : Bigstring.t
  ; output_char : char -> unit
  ; output_chars : output_chars
  ; flush : unit -> unit
  ; sexp : unit -> Sexp.t
  }
[@@deriving fields]

let sexp_of_t t = t.sexp ()

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let _check f = Invariant.check_field t f in
    Fields.iter
      ~bigstring_buf:ignore
      ~output_char:ignore
      ~output_chars:ignore
      ~flush:ignore
      ~sexp:ignore)
;;

let create ~output_char ~output_chars ~flush ~sexp =
  { bigstring_buf = Bigstring.create 0; output_char; output_chars; flush; sexp }
;;

let of_out_channel out_channel : t =
  let bytes_buf = Bytes.of_string "" |> ref in
  create
    ~output_char:(fun c -> Out_channel.output_char out_channel c)
    ~output_chars:(fun buf ~len ->
      if len > Bytes.length !bytes_buf then bytes_buf := Bytes.create (len * 2);
      Bigstring.To_bytes.blit ~len ~src:buf ~src_pos:0 ~dst:!bytes_buf ~dst_pos:0;
      Out_channel.output out_channel ~buf:!bytes_buf ~pos:0 ~len)
    ~flush:(fun () -> Out_channel.flush out_channel)
    ~sexp:(fun () -> [%sexp { out_channel : Out_channel.t }])
;;

let of_output_char output_char : t =
  create
    ~output_char
    ~flush:Fn.id
    ~sexp:(fun () -> [%sexp ()])
    ~output_chars:(fun buf ~len ->
      for i = 0 to len - 1 do
        output_char buf.{i}
      done)
;;

(* Implement the polymorphic [write_gen_* ~blit_to_string] API here rather than in every
   abstraction.  [create ~output_chars] is a simpler basis for this functionality with
   very little cost in abstraction - it only gives up buffer management details. *)
let output t ~blit_to_bigstring ~src ~src_len ~src_pos =
  if src_len > Bigstring.length t.bigstring_buf
  then t.bigstring_buf <- Bigstring.create (src_len * 2);
  blit_to_bigstring ~src ~src_pos ~dst:t.bigstring_buf ~dst_pos:0 ~len:src_len;
  t.output_chars t.bigstring_buf ~len:src_len
;;

let flush t = t.flush ()
