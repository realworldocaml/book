open! Base
open! Stdio

module Code = struct
  type t =
    | NEG_INT8
    | INT16
    | INT32
    | INT64

  let to_int = function
    | NEG_INT8 -> 0xff
    | INT16 -> 0xfe
    | INT32 -> 0xfd
    | INT64 -> 0xfc
  ;;

  let char c = printf "'\\x%x'" (to_int c)
end

module Sig = struct
  let mk_base_tp (name : string) (typ : string) : unit =
    let name () = name
    and typ () = typ in
    printf
      !{|
val bin_writer_%{name} : %{typ} writer
val bin_reader_%{name} : %{typ} reader
val bin_shape_%{name} : Shape.t
val bin_%{name} : %{typ} t
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base name : unit = mk_base_tp name name

  let mk_base1_tp name typ =
    let name () = name
    and typ () = typ in
    printf
      !{|
val bin_writer_%{name} : ('a, 'a %{typ}) S1.writer
val bin_reader_%{name} : ('a, 'a %{typ}) S1.reader
val bin_shape_%{name} : Shape.t -> Shape.t
val bin_%{name} : ('a, 'a %{typ}) S1.t
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base1 name = mk_base1_tp name name

  let mk_base2_tp name typ =
    let name () = name
    and typ () = typ in
    printf
      !{|
val bin_writer_%{name} : ('a, 'b, ('a, 'b) %{typ}) S2.writer
val bin_reader_%{name} : ('a, 'b, ('a, 'b) %{typ}) S2.reader
val bin_shape_%{name} : Shape.t -> Shape.t -> Shape.t
val bin_%{name} : ('a, 'b, ('a, 'b) %{typ}) S2.t
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base2 name = mk_base2_tp name name
end

module Str = struct
  let mk_base (name : string) : unit =
    let name () = name
    and quoted_name () = Printf.sprintf "%S" name in
    printf
      !{|
let bin_writer_%{name} =
  { size = Size.bin_size_%{name}
  ; write = Write.bin_write_%{name}
  }
let bin_reader_%{name} =
  { read = Read.bin_read_%{name}
  ; vtag_read = variant_wrong_type %{quoted_name}
  }
let bin_shape_%{name} = Shape.bin_shape_%{name}
let bin_%{name} =
  { shape = bin_shape_%{name}
  ; writer = bin_writer_%{name}
  ; reader = bin_reader_%{name}
  }
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base1 name =
    let name () = name
    and quoted_name () = Printf.sprintf "%S" name in
    printf
      !{|
let bin_writer_%{name} bin_writer_el =
  { size = (fun v -> Size.bin_size_%{name} bin_writer_el.size v)
  ; write = (fun buf ~pos v ->
      Write.bin_write_%{name} bin_writer_el.write buf ~pos v)
  }
let bin_reader_%{name} bin_reader_el =
  { read = (fun buf ~pos_ref ->
      Read.bin_read_%{name} bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type %{quoted_name};
  }
let bin_shape_%{name} = fun x1 -> Shape.bin_shape_%{name} x1
let bin_%{name} bin_el =
  { shape = bin_shape_%{name} bin_el.shape
  ; writer = bin_writer_%{name} bin_el.writer
  ; reader = bin_reader_%{name} bin_el.reader
  }
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base2 name =
    let name () = name
    and quoted_name () = Printf.sprintf "%S" name in
    printf
      !{|
let bin_writer_%{name} bin_writer_el1 bin_writer_el2 =
  { size = (fun v ->
      Size.bin_size_%{name} bin_writer_el1.size bin_writer_el2.size v)
  ; write = (fun buf ~pos v ->
      Write.bin_write_%{name}
        bin_writer_el1.write bin_writer_el2.write buf ~pos v)
  }
let bin_reader_%{name} bin_reader_el1 bin_reader_el2 =
  { read = (fun buf ~pos_ref ->
    Read.bin_read_%{name}
      bin_reader_el1.read bin_reader_el2.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type %{quoted_name}
  }
let bin_shape_%{name} = fun x1 x2 -> Shape.bin_shape_%{name} x1 x2
let bin_%{name} bin_el1 bin_el2 =
  { shape = bin_shape_%{name} bin_el1.shape bin_el2.shape
  ; writer = bin_writer_%{name} bin_el1.writer bin_el2.writer
  ; reader = bin_reader_%{name} bin_el1.reader bin_el2.reader
  }
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;

  let mk_base3 name =
    let name () = name
    and quoted_name () = Printf.sprintf "%S" name in
    printf
      !{|
let bin_writer_%{name} bin_writer_el1 bin_writer_el2 bin_writer_el3 =
  { size = (fun v ->
      Size.bin_size_%{name}
        bin_writer_el1.size bin_writer_el2.size bin_writer_el3.size v)
  ; write = (fun buf ~pos v ->
      Write.bin_write_%{name}
        bin_writer_el1.write
        bin_writer_el2.write
        bin_writer_el3.write
        buf ~pos v);
  }
let bin_reader_%{name} bin_reader_el1 bin_reader_el2 bin_reader_el3 =
  { read = (fun buf ~pos_ref ->
      Read.bin_read_%{name}
        bin_reader_el1.read
        bin_reader_el2.read
        bin_reader_el3.read
        buf ~pos_ref)
  ; vtag_read = variant_wrong_type %{quoted_name}
  }
let bin_shape_%{name} = fun x1 x2 x3 -> Shape.bin_shape_%{name} x1 x2 x3
let bin_%{name} bin_el1 bin_el2 bin_el3 =
  { shape = bin_shape_%{name} bin_el1.shape bin_el2.shape bin_el3.shape
  ; writer = bin_writer_%{name} bin_el1.writer bin_el2.writer bin_el3.writer
  ; reader = bin_reader_%{name} bin_el1.reader bin_el2.reader bin_el3.reader
  }
|}
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
  ;;
end
