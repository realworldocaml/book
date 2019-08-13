open Bin_prot.Std
open Bin_prot.Blob

module Mystery = struct
  type t =
    { name : string
    ; age : int
    ; favorite_colors : string list
    } [@@deriving bin_io]

  let value =
    { name = "Drew"
    ; age = 25
    ; favorite_colors = [ "Blue"; "Yellow" ]
    }
end

module T = struct
  type 'a t =
    { header : string
    ; mystery : 'a
    ; footer : string
    } [@@deriving bin_io]

  let value mystery =
    { header = "header"
    ; mystery
    ; footer = "footer"
    }
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

let roundtrip { Bin_prot.Type_class. reader; writer; shape = _ } value =
  assert (convert writer reader value = value)

module Dropped = struct
  type t = Ignored.t T.t [@@deriving bin_read]

  let bin_size_t = T.bin_size_t Ignored.bin_size_t
end

let bigstring_to_string bigstring =
  let len = Bigarray.Array1.dim bigstring in
  String.init len (fun i -> bigstring.{i})

let test =
  let open OUnit in
  "Blob_test" >::: [
    "roundtrip known" >:: (fun () -> roundtrip Known.bin_t Known.value);
    "roundtrip unknown" >:: (fun () -> roundtrip Unknown.bin_t Unknown.value);

    "opaque and wrapped serialize the same way" >:: (fun () ->
      let known_buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let unknown_buffer = Bin_prot.Utils.bin_dump Unknown.bin_writer_t Unknown.value in
      let known_s = bigstring_to_string known_buffer in
      let unknown_s = bigstring_to_string unknown_buffer in
      if known_s <> unknown_s then
        failwith (Printf.sprintf "%s <> %s" known_s unknown_s));

    "serialized wrapped deserializes to the expected opaque" >:: (fun () ->
      let unknown_from_known =
        convert Known.bin_writer_t Unknown.bin_reader_t Known.value
      in
      assert (Unknown.value = unknown_from_known));

    "serialized opaque deserializes to the expected wrapped" >:: (fun () ->
      let known_from_unknown =
        convert Unknown.bin_writer_t Known.bin_reader_t Unknown.value
      in
      assert (Known.value = known_from_unknown));

    "Dropped" >:: (fun () ->
      let buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let value = Dropped.bin_reader_t.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0) in
      let ignored = value.mystery in
      (* The value deserialized with [Dropped] agrees with the value serialized by
         [Known], except for the ignored bit. *)
      assert ({ Known.value with mystery = ignored} = value );
      (* [Dropped] remembered the size of the ignored data. *)
      assert (Dropped.bin_size_t value = Known.bin_size_t Known.value));
  ]
