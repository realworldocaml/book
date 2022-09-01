open! Core
open! Import
module Blob = Bin_prot.Blob
module Opaque = Blob.Opaque
module Type_class = Bin_prot.Type_class

let buf = lazy (Bin_prot.Common.create_buf 10_000)

let bin_dump_to_string bin_a a =
  let buf = Bin_prot.Utils.bin_dump bin_a.Type_class.writer a in
  let len = Bin_prot.Common.buf_len buf in
  let str = Bytes.create len in
  Bin_prot.Common.blit_buf_string buf str ~len;
  Bytes.to_string str
;;

let bin_read_from_string bin_reader str =
  let buf = Lazy.force buf in
  let len = String.length str in
  Bin_prot.Common.blit_string_buf str buf ~len;
  let pos_ref = ref 0 in
  let a = bin_reader.Type_class.read buf ~pos_ref in
  assert (Int.( = ) !pos_ref len);
  a
;;

let print_bin_dump tag bin_a a =
  printf "%s : %s\n" tag (bin_dump_to_string bin_a a |> String.escaped)
;;

let run_stability_test (bin_a : 'a Type_class.t) equal a : unit =
  print_bin_dump "bin dump" bin_a a;
  print_bin_dump "bin dump (blob)" (Blob.bin_t bin_a) a;
  let buf = Lazy.force buf in
  let dump_and_read bin_t equal t =
    let str = bin_dump_to_string bin_t t in
    assert (equal t (bin_read_from_string bin_t.reader str));
    let (_ : Blob.Ignored.t) = bin_read_from_string Blob.Ignored.bin_reader_t str in
    str
  in
  let bin_dump_blob = dump_and_read (Blob.bin_t bin_a) equal a in
  let bin_dump_opaque_big_string =
    dump_and_read
      Opaque.Bigstring.bin_t
      Poly.equal
      (Opaque.Bigstring.to_opaque a bin_a.writer)
  in
  let bin_dump_opaque_string =
    dump_and_read
      Opaque.String.bin_t
      Poly.equal
      (Opaque.String.to_opaque ~buf a bin_a.writer)
  in
  printf
    !"bin dump as opaque big string and string are the same? %{Bool}\n"
    (String.( = ) bin_dump_blob bin_dump_opaque_big_string
     && String.( = ) bin_dump_blob bin_dump_opaque_string)
;;

let%test_module _ =
  (module struct
    open Blob
    open Poly

    module Mystery = struct
      type t =
        { name : string
        ; age : int
        ; favorite_colors : string list
        }
      [@@deriving bin_io]

      let value = { name = "Drew"; age = 25; favorite_colors = [ "Blue"; "Yellow" ] }
    end

    module T = struct
      type 'a t =
        { header : string
        ; mystery : 'a
        ; footer : string
        }
      [@@deriving bin_io]

      let value mystery = { header = "header"; mystery; footer = "footer" }
    end

    (* Some Rumsfeldian tests follow... *)
    module Known = struct
      type nonrec t = Mystery.t t T.t [@@deriving bin_io]

      let value = T.value Mystery.value
    end

    module Unknown = struct
      type t = Opaque.Bigstring.t T.t [@@deriving bin_io]

      let value = T.value (Opaque.Bigstring.to_opaque Mystery.value Mystery.bin_writer_t)
    end

    let convert bin_writer bin_reader value =
      let buffer = Bin_prot.Utils.bin_dump bin_writer value in
      bin_reader.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0)
    ;;

    let roundtrip { Bin_prot.Type_class.reader; writer; shape = _ } value =
      assert (convert writer reader value = value)
    ;;

    module Dropped = struct
      type t = Ignored.t T.t [@@deriving bin_read]

      let bin_size_t = T.bin_size_t Ignored.bin_size_t
    end

    let bigstring_to_string bigstring =
      let len = Bigarray.Array1.dim bigstring in
      String.init len ~f:(fun i -> bigstring.{i})
    ;;

    let%test_unit "roundtrip known" = roundtrip Known.bin_t Known.value
    let%test_unit "roundtrip unknown" = roundtrip Unknown.bin_t Unknown.value

    let%test_unit "opaque and wrapped serialize the same way" =
      let known_buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let unknown_buffer = Bin_prot.Utils.bin_dump Unknown.bin_writer_t Unknown.value in
      let known_s = bigstring_to_string known_buffer in
      let unknown_s = bigstring_to_string unknown_buffer in
      if known_s <> unknown_s then failwith (Printf.sprintf "%s <> %s" known_s unknown_s)
    ;;

    let%test_unit "serialized wrapped deserializes to the expected opaque" =
      let unknown_from_known =
        convert Known.bin_writer_t Unknown.bin_reader_t Known.value
      in
      assert (Unknown.value = unknown_from_known)
    ;;

    let%test_unit "serialized opaque deserializes to the expected wrapped" =
      let known_from_unknown =
        convert Unknown.bin_writer_t Known.bin_reader_t Unknown.value
      in
      assert (Known.value = known_from_unknown)
    ;;

    let%test_unit "Dropped" =
      let buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let value = Dropped.bin_reader_t.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0) in
      let ignored = value.mystery in
      (* The value deserialized with [Dropped] agrees with the value serialized by
         [Known], except for the ignored bit. *)
      assert ({ Known.value with mystery = ignored } = value);
      (* [Dropped] remembered the size of the ignored data. *)
      assert (Dropped.bin_size_t value = Known.bin_size_t Known.value)
    ;;
  end)
;;
