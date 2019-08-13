(* Utils: utility functions for user convenience *)

open Common
open Size
open Type_class

let size_header_length = 8

let bin_write_size_header = Write.bin_write_int_64bit
let bin_read_size_header = Read.bin_read_int_64bit

let bin_dump ?(header = false) writer v =
  let buf, pos, pos_len =
    let v_len = writer.size v in
    if header then
      let tot_len = v_len + size_header_length in
      let buf = create_buf tot_len in
      let pos = bin_write_size_header buf ~pos:0 v_len in
      buf, pos, pos + v_len
    else
      let buf = create_buf v_len in
      buf, 0, v_len
  in
  let pos = writer.write buf ~pos v in
  if pos = pos_len then buf
  else failwith "Bin_prot.Utils.bin_dump: size changed during writing"


(* Reading from streams *)

let bin_read_stream ?max_size ~read reader =
  let buf = create_buf size_header_length in
  read buf ~pos:0 ~len:size_header_length;
  let pos_ref = ref 0 in
  let len = bin_read_size_header buf ~pos_ref in
  match max_size with
  | Some max_size when len > max_size ->
    failwith (
      Printf.sprintf
        "Bin_prot.Utils.bin_read_stream: size exceeds max_size: %d > %d"
        len max_size)
  | _ ->
    let buf = if len > size_header_length then create_buf len else buf in
    read buf ~pos:0 ~len;
    pos_ref := 0;
    let res = reader.read buf ~pos_ref in
    if !pos_ref = len then res
    else
      let msg =
        Printf.sprintf
          "Bin_prot.Utils.bin_read_stream: \
           protocol lied about length of value: expected %d, received %d"
          len !pos_ref
      in
      failwith msg


(* Conversion of binable types *)

module type Make_binable_spec = sig
  module Binable : Binable.Minimal.S

  type t

  val to_binable : t -> Binable.t
  val of_binable : Binable.t -> t

end

module Of_minimal (S : Binable.Minimal.S) : Binable.S with type t := S.t = struct
  include S

  let bin_writer_t =
    {
      size = bin_size_t;
      write = bin_write_t;
    }

  let bin_reader_t =
    {
      read = bin_read_t;
      vtag_read = __bin_read_t__;
    }

  let bin_t =
    {
      shape = bin_shape_t;
      writer = bin_writer_t;
      reader = bin_reader_t;
    }
end

module Make_binable (S : Make_binable_spec) = struct
  include Of_minimal(struct
      module B = S.Binable

      type t = S.t

      let bin_shape_t = B.bin_shape_t

      let bin_size_t t = B.bin_size_t (S.to_binable t)
      let bin_write_t buf ~pos t = B.bin_write_t buf ~pos (S.to_binable t)
      let bin_read_t buf ~pos_ref = S.of_binable (B.bin_read_t buf ~pos_ref)

      let __bin_read_t__ buf ~pos_ref n =
        S.of_binable (B.__bin_read_t__ buf ~pos_ref n)
    end)
end

module type Make_binable1_spec = sig
  module Binable : Binable.Minimal.S1

  type 'a t

  val to_binable : 'a t -> 'a Binable.t
  val of_binable : 'a Binable.t -> 'a t
end

module Make_binable1 (S : Make_binable1_spec) = struct
  module B = S.Binable

  let bin_shape_t bin_shape_el = B.bin_shape_t bin_shape_el

  let bin_size_t bin_size_el t = B.bin_size_t bin_size_el (S.to_binable t)

  let bin_write_t bin_write_el buf ~pos t =
    B.bin_write_t bin_write_el buf ~pos (S.to_binable t)

  let bin_read_t bin_read_el buf ~pos_ref =
    S.of_binable (B.bin_read_t bin_read_el buf ~pos_ref)

  let __bin_read_t__ bin_read_el buf ~pos_ref n =
    S.of_binable (B.__bin_read_t__ bin_read_el buf ~pos_ref n)

  let bin_writer_t bin_writer =
    {
      size = (fun v -> bin_size_t bin_writer.size v);
      write = (fun buf ~pos v ->
        bin_write_t bin_writer.write buf ~pos v);
    }

  let bin_reader_t bin_reader =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t bin_reader.read buf ~pos_ref);
      vtag_read = (fun _buf ~pos_ref _n ->
        raise_variant_wrong_type
          "Bin_prot.Utils.Make_binable1.bin_reader_t"
          !pos_ref);
    }

  let bin_t type_class =
    {
      shape = bin_shape_t type_class.shape;
      writer = bin_writer_t type_class.writer;
      reader = bin_reader_t type_class.reader;
    }
end

module type Make_binable2_spec = sig
  module Binable : Binable.Minimal.S2

  type ('a, 'b) t

  val to_binable : ('a, 'b) t -> ('a, 'b) Binable.t
  val of_binable : ('a, 'b) Binable.t -> ('a, 'b) t
end

module Make_binable2 (S : Make_binable2_spec) = struct
  module B = S.Binable

  let bin_shape_t bin_shape_el1 bin_shape_el2 =
    B.bin_shape_t bin_shape_el1 bin_shape_el2

  let bin_size_t bin_size_el1 bin_size_el2 t =
    B.bin_size_t bin_size_el1 bin_size_el2 (S.to_binable t)

  let bin_write_t bin_write_el1 bin_write_el2 buf ~pos t =
    B.bin_write_t bin_write_el1 bin_write_el2 buf ~pos (S.to_binable t)

  let bin_read_t bin_read_el1 bin_read_el2 buf ~pos_ref =
    S.of_binable (B.bin_read_t bin_read_el1 bin_read_el2 buf ~pos_ref)

  let __bin_read_t__ bin_read_el1 bin_read_el2 buf ~pos_ref n =
    S.of_binable (B.__bin_read_t__ bin_read_el1 bin_read_el2 buf ~pos_ref n)

  let bin_writer_t bin_writer1 bin_writer2 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.write bin_writer2.write buf ~pos v);
    }

  let bin_reader_t bin_reader1 bin_reader2 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.read bin_reader2.read buf ~pos_ref);
      vtag_read = (fun _buf ~pos_ref _n ->
        raise_variant_wrong_type
          "Bin_prot.Utils.Make_binable2.bin_reader_t"
          !pos_ref);
    }

  let bin_t type_class1 type_class2 =
    {
      shape = bin_shape_t type_class1.shape type_class2.shape;
      writer = bin_writer_t type_class1.writer type_class2.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader;
    }
end

module type Make_binable3_spec = sig
  module Binable : Binable.Minimal.S3

  type ('a, 'b, 'c) t

  val to_binable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Binable.t
  val of_binable : ('a, 'b, 'c) Binable.t -> ('a, 'b, 'c) t
end

module Make_binable3 (S : Make_binable3_spec) = struct
  module B = S.Binable

  let bin_shape_t bin_shape_el1 bin_shape_el2 bin_shape_el3 =
    B.bin_shape_t bin_shape_el1 bin_shape_el2 bin_shape_el3

  let bin_size_t bin_size_el1 bin_size_el2 bin_size_el3 t =
    B.bin_size_t bin_size_el1 bin_size_el2 bin_size_el3 (S.to_binable t)

  let bin_write_t bin_write_el1 bin_write_el2 bin_write_el3 buf ~pos t =
    B.bin_write_t bin_write_el1 bin_write_el2 bin_write_el3 buf ~pos (S.to_binable t)

  let bin_read_t bin_read_el1 bin_read_el2 bin_read_el3 buf ~pos_ref =
    S.of_binable (B.bin_read_t bin_read_el1 bin_read_el2 bin_read_el3 buf ~pos_ref)

  let __bin_read_t__ bin_read_el1 bin_read_el2 bin_read_el3 buf ~pos_ref n =
    S.of_binable (B.__bin_read_t__ bin_read_el1 bin_read_el2 bin_read_el3 buf ~pos_ref n)

  let bin_writer_t bin_writer1 bin_writer2 bin_writer3 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size bin_writer3.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.write bin_writer2.write bin_writer3.write buf ~pos v);
    }

  let bin_reader_t bin_reader1 bin_reader2 bin_reader3 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.read bin_reader2.read bin_reader3.read buf ~pos_ref);
      vtag_read = (fun _buf ~pos_ref _n ->
        raise_variant_wrong_type
          "Bin_prot.Utils.Make_binable3.bin_reader_t"
          !pos_ref);
    }

  let bin_t type_class1 type_class2 type_class3 =
    {
      shape  = bin_shape_t  type_class1.shape  type_class2.shape  type_class3.shape;
      writer = bin_writer_t type_class1.writer type_class2.writer type_class3.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader type_class3.reader;
    }
end

module type Make_iterable_binable_spec = sig
  type t
  type el

  val caller_identity : Shape.Uuid.t
  val module_name : string option
  val length : t -> int
  val iter : t -> f : (el -> unit) -> unit
  val init : len:int -> next:(unit -> el) -> t
  val bin_size_el : el Size.sizer
  val bin_write_el : el Write.writer
  val bin_read_el : el Read.reader
  val bin_shape_el : Shape.t
end

let with_module_name f ~module_name function_name =
  match module_name with
  | None -> f function_name
  | Some module_name ->
    Printf.ksprintf f "%s.%s" module_name function_name

let raise_concurrent_modification = with_module_name raise_concurrent_modification

let raise_read_too_much =
  with_module_name (Printf.ksprintf failwith
                      "%s: tried to read more elements than available")

let raise_read_not_enough =
  with_module_name (Printf.ksprintf failwith
                      "%s: didn't read all elements")

module Make_iterable_binable (S : Make_iterable_binable_spec) = struct
  open S

  let bin_shape_t =
    Shape.(basetype caller_identity [
      basetype (Uuid.of_string "6592371a-4994-11e6-923a-7748e4182764") [S.bin_shape_el]])

  let bin_size_t t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification ~module_name "bin_size_t"

  let bin_write_t buf ~pos t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let pos_ref = ref (Write.bin_write_nat0 buf ~pos plen) in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      pos_ref := bin_write_el buf ~pos:!pos_ref el;
      incr cnt_ref);
    if !cnt_ref = len then
      !pos_ref
    else
      raise_concurrent_modification ~module_name "bin_write_t"

  let bin_read_t buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    let idx = ref 0 in
    let next () =
      if !idx >= len then raise_read_too_much ~module_name "bin_read_t";
      incr idx;
      bin_read_el buf ~pos_ref
    in
    let result = init ~len ~next in
    if !idx < len then raise_read_not_enough ~module_name "bin_read_t";
    result
  ;;

  let __bin_read_t__ _buf ~pos_ref _n =
    raise_variant_wrong_type "t" !pos_ref

  let bin_writer_t =
    {
      size = bin_size_t;
      write = bin_write_t;
    }

  let bin_reader_t =
    {
      read = bin_read_t;
      vtag_read = __bin_read_t__;
    }

  let bin_t =
    {
      shape = bin_shape_t;
      writer = bin_writer_t;
      reader = bin_reader_t;
    }
end

module type Make_iterable_binable1_spec = sig
  type 'a t
  type 'a el

  val caller_identity : Shape.Uuid.t
  val module_name : string option
  val length : 'a t -> int
  val iter : 'a t -> f : ('a el -> unit) -> unit
  val init : len:int -> next:(unit -> 'a el) -> 'a t
  val bin_size_el : ('a, 'a el) Size.sizer1
  val bin_write_el : ('a, 'a el) Write.writer1
  val bin_read_el : ('a, 'a el) Read.reader1
  val bin_shape_el : Shape.t -> Shape.t
end



module Make_iterable_binable1 (S : Make_iterable_binable1_spec) = struct
  open S

  let bin_shape_t t =
    Shape.(basetype caller_identity [
      basetype (Uuid.of_string "ac8a9ff4-4994-11e6-9a1b-9fb4e933bd9d") [S.bin_shape_el t]])

  let bin_size_t bin_size_a t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el bin_size_a el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then
      bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else
      raise_concurrent_modification ~module_name "bin_size_t"

  let bin_write_t bin_write_a buf ~pos t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let pos_ref = ref (Write.bin_write_nat0 buf ~pos plen) in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      pos_ref := bin_write_el bin_write_a buf ~pos:!pos_ref el;
      incr cnt_ref);
    if !cnt_ref = len then
      !pos_ref
    else
      raise_concurrent_modification ~module_name "bin_write_t"

  let bin_read_t bin_read_a buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    let idx = ref 0 in
    let next () =
      if !idx >= len then raise_read_too_much ~module_name "bin_read_t";
      incr idx;
      bin_read_el bin_read_a buf ~pos_ref
    in
    let result = init ~len ~next in
    if !idx < len then raise_read_not_enough ~module_name "bin_read_t";
    result

  let __bin_read_t__ _bin_read_a _buf ~pos_ref _n =
    raise_variant_wrong_type "t" !pos_ref

  let bin_writer_t bin_writer =
    {
      size = (fun v -> bin_size_t bin_writer.size v);
      write = (fun buf ~pos v ->
        bin_write_t bin_writer.write buf ~pos v);
    }

  let bin_reader_t bin_reader =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t bin_reader.read buf ~pos_ref);
      vtag_read = (fun buf ~pos_ref _n ->
        __bin_read_t__ bin_reader.read buf ~pos_ref _n);
    }

  let bin_t type_class =
    {
      shape = bin_shape_t type_class.shape;
      writer = bin_writer_t type_class.writer;
      reader = bin_reader_t type_class.reader;
    }
end


module type Make_iterable_binable2_spec = sig
  type ('a, 'b) t
  type ('a, 'b) el

  val caller_identity : Shape.Uuid.t
  val module_name : string option

  val length : ('a, 'b) t -> int
  val iter : ('a, 'b) t -> f : (('a, 'b) el -> unit) -> unit
  val init : len:int -> next:(unit -> ('a, 'b) el) -> ('a, 'b) t

  val bin_size_el : ('a, 'b, ('a, 'b) el) Size.sizer2
  val bin_write_el : ('a, 'b, ('a, 'b) el) Write.writer2
  val bin_read_el : ('a, 'b, ('a, 'b) el) Read.reader2
  val bin_shape_el : Shape.t -> Shape.t -> Shape.t
end

module Make_iterable_binable2 (S : Make_iterable_binable2_spec) = struct
  open S

  let bin_shape_t t1 t2 =
    Shape.(basetype caller_identity [
      basetype (Uuid.of_string "b4e54ad2-4994-11e6-b8df-87c2997f9f52") [S.bin_shape_el t1 t2]])

  let bin_size_t bin_size_a bin_size_b t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el bin_size_a bin_size_b el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification ~module_name "bin_size_t"

  let bin_write_t bin_write_a bin_write_b buf ~pos t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let pos_ref = ref (Write.bin_write_nat0 buf ~pos plen) in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      pos_ref := bin_write_el bin_write_a bin_write_b buf ~pos:!pos_ref el;
      incr cnt_ref);
    if !cnt_ref = len then
      !pos_ref
    else
      raise_concurrent_modification ~module_name "bin_write_t"

  let bin_read_t bin_read_a bin_read_b buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    let idx = ref 0 in
    let next () =
      if !idx >= len then raise_read_too_much ~module_name "bin_read_t";
      incr idx;
      bin_read_el bin_read_a bin_read_b buf ~pos_ref
    in
    let result = init ~len ~next in
    if !idx < len then raise_read_not_enough ~module_name "bin_read_t";
    result

  let __bin_read_t__ _bin_read_a _bin_read_b _buf ~pos_ref _n =
    raise_variant_wrong_type "t" !pos_ref

  let bin_writer_t bin_writer1 bin_writer2 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.write bin_writer2.write buf ~pos v);
    }

  let bin_reader_t bin_reader1 bin_reader2 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.read bin_reader2.read buf ~pos_ref);
      vtag_read = (fun buf ~pos_ref n ->
        __bin_read_t__ bin_reader1.read bin_reader2.read buf ~pos_ref n);
    }

  let bin_t type_class1 type_class2 =
    {
      shape = bin_shape_t type_class1.shape type_class2.shape;
      writer = bin_writer_t type_class1.writer type_class2.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader;
    }
end


module type Make_iterable_binable3_spec = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) el

  val caller_identity : Shape.Uuid.t
  val module_name : string option

  val length : ('a, 'b, 'c) t -> int
  val iter : ('a, 'b, 'c) t -> f : (('a, 'b, 'c) el -> unit) -> unit
  val init : len:int -> next:(unit -> ('a, 'b, 'c) el) -> ('a, 'b, 'c) t

  val bin_size_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Size.sizer3
  val bin_write_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Write.writer3
  val bin_read_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Read.reader3
  val bin_shape_el : Shape.t -> Shape.t -> Shape.t -> Shape.t
end

module Make_iterable_binable3 (S : Make_iterable_binable3_spec) = struct
  open S

  let bin_shape_t t1 t2 t3 =
    Shape.(basetype caller_identity [
      basetype
        (Uuid.of_string "f2112eda-e7d7-11e6-bb36-072e9ce159db")
        [S.bin_shape_el t1 t2 t3]])

  let bin_size_t bin_size_a bin_size_b bin_size_c t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el bin_size_a bin_size_b bin_size_c el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification ~module_name "bin_size_t"

  let bin_write_t bin_write_a bin_write_b bin_write_c buf ~pos t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let pos_ref = ref (Write.bin_write_nat0 buf ~pos plen) in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      pos_ref := bin_write_el bin_write_a bin_write_b bin_write_c buf ~pos:!pos_ref el;
      incr cnt_ref);
    if !cnt_ref = len then
      !pos_ref
    else
      raise_concurrent_modification ~module_name "bin_write_t"

  let bin_read_t bin_read_a bin_read_b bin_read_c buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    let idx = ref 0 in
    let next () =
      if !idx >= len then raise_read_too_much ~module_name "bin_read_t";
      incr idx;
      bin_read_el bin_read_a bin_read_b bin_read_c buf ~pos_ref
    in
    let result = init ~len ~next in
    if !idx < len then raise_read_not_enough ~module_name "bin_read_t";
    result

  let __bin_read_t__ _bin_read_a _bin_read_b _bin_read_c _buf ~pos_ref _n =
    raise_variant_wrong_type "t" !pos_ref

  let bin_writer_t bin_writer1 bin_writer2 bin_writer3 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size bin_writer3.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.write bin_writer2.write bin_writer3.write buf ~pos v);
    }

  let bin_reader_t bin_reader1 bin_reader2 bin_reader3 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.read bin_reader2.read bin_reader3.read buf ~pos_ref);
      vtag_read = (fun buf ~pos_ref n ->
        __bin_read_t__ bin_reader1.read bin_reader2.read bin_reader3.read buf ~pos_ref n);
    }

  let bin_t type_class1 type_class2 type_class3 =
    {
      shape  = bin_shape_t  type_class1.shape  type_class2.shape  type_class3.shape;
      writer = bin_writer_t type_class1.writer type_class2.writer type_class3.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader type_class3.reader;
    }
end
