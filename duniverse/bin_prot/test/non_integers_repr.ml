(* WARNING: never accept the corrected output for this file, it must never change! *)

open Core_kernel
open Poly
open Bigarray
open Import

(* Generate reference serialized output for various functions of
   [Bin_prot.Write] for the purpose of ensuring [Bin_prot.Size.Maximum] and
   [Bin_prot.Size.Minimum] bound correctness.

   We can't generate reference output for every possible values as it would be huge,
   instead we choose a few interesting points and generate tests around and including
   those points.
*)

module Read = Bin_prot.Read
module Write = Bin_prot.Write
module Shape = Bin_prot.Shape

type 'a to_test =
  { writer : 'a Write.writer
  ; reader : 'a Read.reader
  ; values : 'a list
  ; equal : 'a -> 'a -> bool
  ; sexp_of : 'a -> Sexp.t (* Bounds on the bin_protted size *)
  ; hi_bound : int option
  ; lo_bound : int
  }

module Vec = struct
  let mk_gen_float tp n =
    let vec = Array1.create tp fortran_layout n in
    for i = 1 to n do
      vec.{i} <- float i
    done;
    vec
  ;;

  let mk_float32 = mk_gen_float float32
  let mk_float64 = mk_gen_float float64
  let to_array t = Array.init (Array1.dim t) ~f:(fun i -> t.{i + 1})
  let sexp_of_t t = [%sexp_of: float array] (to_array t)
  let equal t1 t2 = Array.equal Float.equal (to_array t1) (to_array t2)
end

module Mat = struct
  let mk_gen_float tp m n =
    let mat = Array2.create tp fortran_layout m n in
    let fn = float m in
    for c = 1 to n do
      let ofs = float (c - 1) *. fn in
      for r = 1 to m do
        mat.{r, c} <- ofs +. float r
      done
    done;
    mat
  ;;

  let mk_float32 = mk_gen_float float32
  let mk_float64 = mk_gen_float float64

  let to_array t =
    Array.init (Array2.dim1 t) ~f:(fun i ->
      Array.init (Array2.dim2 t) ~f:(fun j -> t.{i + 1, j + 1}))
  ;;

  let sexp_of_t t = [%sexp_of: float array array] (to_array t)
  let equal t1 t2 = Array.equal (Array.equal Float.equal) (to_array t1) (to_array t2)
end

module Tests = struct
  (* max and min *)
  let unit =
    { writer = Write.bin_write_unit
    ; reader = Read.bin_read_unit
    ; values = [ () ]
    ; equal = Unit.equal
    ; sexp_of = [%sexp_of: unit]
    ; hi_bound = Some Maximum.bin_size_unit
    ; lo_bound = Minimum.bin_size_unit
    }
  ;;

  let bool =
    { writer = Write.bin_write_bool
    ; reader = Read.bin_read_bool
    ; values = [ true; false ]
    ; equal = Bool.equal
    ; sexp_of = [%sexp_of: bool]
    ; hi_bound = Some Maximum.bin_size_bool
    ; lo_bound = Minimum.bin_size_bool
    }
  ;;

  let char =
    { writer = Write.bin_write_char
    ; reader = Read.bin_read_char
    ; values = [ '\x00'; 'A'; 'z'; ';'; '\xFF' ]
    ; equal = Char.equal
    ; sexp_of = [%sexp_of: char]
    ; hi_bound = Some Maximum.bin_size_char
    ; lo_bound = Minimum.bin_size_char
    }
  ;;

  let digest =
    { writer = Write.bin_write_md5
    ; reader = Read.bin_read_md5
    ; values = [ Md5_lib.of_hex_exn "0123456789abcdef0123456789ABCDEF" ]
    ; equal = (fun d1 d2 -> Md5_lib.compare d1 d2 = 0)
    ; sexp_of = (fun md5 -> Atom (Md5_lib.to_hex md5))
    ; hi_bound = Some Maximum.bin_size_md5
    ; lo_bound = Minimum.bin_size_md5
    }
  ;;

  let float =
    { writer = Write.bin_write_float
    ; reader = Read.bin_read_float
    ; values =
        [ Float.epsilon_float
        ; Float.infinity
        ; Float.max_finite_value
        ; Float.max_value
        ; Float.min_positive_normal_value
        ; Float.min_positive_subnormal_value
        ; Float.min_value
        ; Float.minus_one
        ; Float.neg_infinity
        ; Float.one
        ; Float.robust_comparison_tolerance
        ; Float.zero
        ]
    ; equal = (fun a b -> (Float.is_nan a && Float.is_nan b) || Float.equal a b)
    ; sexp_of = [%sexp_of: float]
    ; hi_bound = Some Maximum.bin_size_float
    ; lo_bound = Minimum.bin_size_float
    }
  ;;

  let float_nan = { float with values = [ Float.nan ] }

  (* min only *)
  let vec =
    { writer = Write.bin_write_vec
    ; reader = Read.bin_read_vec
    ; values = [ Vec.mk_float64 0; Vec.mk_float64 1 ]
    ; equal = Vec.equal
    ; sexp_of = [%sexp_of: Vec.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_vec
    }
  ;;

  let float32_vec =
    { writer = Write.bin_write_float32_vec
    ; reader = Read.bin_read_float32_vec
    ; values = [ Vec.mk_float32 0; Vec.mk_float32 1 ]
    ; equal = Vec.equal
    ; sexp_of = [%sexp_of: Vec.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_float32_vec
    }
  ;;

  let float64_vec =
    { writer = Write.bin_write_float64_vec
    ; reader = Read.bin_read_float64_vec
    ; values = [ Vec.mk_float64 0; Vec.mk_float64 1 ]
    ; equal = Vec.equal
    ; sexp_of = [%sexp_of: Vec.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_float64_vec
    }
  ;;

  let mat =
    { writer = Write.bin_write_mat
    ; reader = Read.bin_read_mat
    ; values = [ Mat.mk_float64 0 0; Mat.mk_float64 1 1 ]
    ; equal = Mat.equal
    ; sexp_of = [%sexp_of: Mat.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_mat
    }
  ;;

  let float32_mat =
    { writer = Write.bin_write_float32_mat
    ; reader = Read.bin_read_float32_mat
    ; values = [ Mat.mk_float32 0 0; Mat.mk_float32 1 1 ]
    ; equal = Mat.equal
    ; sexp_of = [%sexp_of: Mat.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_float32_mat
    }
  ;;

  let float64_mat =
    { writer = Write.bin_write_float64_mat
    ; reader = Read.bin_read_float64_mat
    ; values = [ Mat.mk_float64 0 0; Mat.mk_float64 1 1 ]
    ; equal = Mat.equal
    ; sexp_of = [%sexp_of: Mat.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_float64_mat
    }
  ;;

  let bigstring =
    { writer = Write.bin_write_bigstring
    ; reader = Read.bin_read_bigstring
    ; values = [ Bigstring.of_string ""; Bigstring.of_string "hello" ]
    ; equal =
        (fun s1 s2 -> String.equal (Bigstring.to_string s1) (Bigstring.to_string s2))
    ; sexp_of = [%sexp_of: Bigstring.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_bigstring
    }
  ;;

  let float_array =
    { writer = Write.bin_write_float_array
    ; reader = Read.bin_read_float_array
    ; values = [ [||]; [| 0. |] ]
    ; equal = Array.equal Float.equal
    ; sexp_of = [%sexp_of: float array]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_float_array
    }
  ;;

  let ref =
    { writer = Write.bin_write_ref Write.bin_write_int32
    ; reader = Read.bin_read_ref Read.bin_read_int32
    ; values = [ ref 0l; ref 1l; ref (-1l); ref Int32.max_value; ref Int32.min_value ]
    ; equal = (fun v1 v2 -> !v1 = !v2)
    ; sexp_of = [%sexp_of: int32 ref]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_ref
    }
  ;;

  let lazy_t =
    { writer = Write.bin_write_lazy Write.bin_write_int32
    ; reader = Read.bin_read_lazy Read.bin_read_int32
    ; values =
        [ lazy 0l; lazy 1l; lazy (-1l); lazy Int32.max_value; lazy Int32.min_value ]
    ; equal = (fun v1 v2 -> force v1 = force v2)
    ; sexp_of = [%sexp_of: int32 Lazy.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_lazy_t
    }
  ;;

  let option =
    { writer = Write.bin_write_option Write.bin_write_int32
    ; reader = Read.bin_read_option Read.bin_read_int32
    ; values =
        [ None; Some 0l; Some 1l; Some (-1l); Some Int32.max_value; Some Int32.min_value ]
    ; equal = Option.equal Int32.equal
    ; sexp_of = [%sexp_of: int32 option]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_option
    }
  ;;

  let pair =
    { writer = Write.bin_write_pair Write.bin_write_int32 Write.bin_write_int32
    ; reader = Read.bin_read_pair Read.bin_read_int32 Read.bin_read_int32
    ; values =
        [ 0l, 0l
        ; 1l, 1l
        ; -1l, -1l
        ; Int32.max_value, Int32.max_value
        ; Int32.min_value, Int32.min_value
        ]
    ; equal = Tuple2.equal ~eq1:Int32.equal ~eq2:Int32.equal
    ; sexp_of = [%sexp_of: int32 * int32]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_pair
    }
  ;;

  let triple =
    { writer =
        Write.bin_write_triple
          Write.bin_write_int32
          Write.bin_write_int32
          Write.bin_write_int32
    ; reader =
        Read.bin_read_triple Read.bin_read_int32 Read.bin_read_int32 Read.bin_read_int32
    ; values =
        [ 0l, 0l, 0l
        ; 1l, 1l, 1l
        ; -1l, -1l, -1l
        ; Int32.max_value, Int32.max_value, Int32.max_value
        ; Int32.min_value, Int32.min_value, Int32.min_value
        ]
    ; equal = Tuple3.equal ~eq1:Int32.equal ~eq2:Int32.equal ~eq3:Int32.equal
    ; sexp_of = [%sexp_of: int32 * int32 * int32]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_triple
    }
  ;;

  let list =
    { writer = Write.bin_write_list Write.bin_write_int32
    ; reader = Read.bin_read_list Read.bin_read_int32
    ; values =
        [ []
        ; [ 0l ]
        ; [ 0l; 1l ]
        ; [ 1l; -1l ]
        ; [ 0l; Int32.max_value ]
        ; [ Int32.max_value; Int32.min_value ]
        ]
    ; equal = List.equal Int32.equal
    ; sexp_of = [%sexp_of: int32 list]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_list
    }
  ;;

  let array =
    { writer = Write.bin_write_array Write.bin_write_int32
    ; reader = Read.bin_read_array Read.bin_read_int32
    ; values =
        [ [||]
        ; [| 0l |]
        ; [| 0l; 1l |]
        ; [| 1l; -1l |]
        ; [| 0l; Int32.max_value |]
        ; [| Int32.max_value; Int32.min_value |]
        ]
    ; equal = Array.equal Int32.equal
    ; sexp_of = [%sexp_of: int32 array]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_array
    }
  ;;

  let hashtbl =
    { writer = Write.bin_write_hashtbl Write.bin_write_int32 Write.bin_write_int32
    ; reader = Read.bin_read_hashtbl Read.bin_read_int32 Read.bin_read_int32
    ; values =
        List.map
          [ []
          ; [ 0l, 0l ]
          ; [ 0l, 0l; 1l, 1l ]
          ; [ 0l, 0l; Int32.max_value, Int32.max_value ]
          ; [ -1l, -1l; Int32.min_value, Int32.min_value ]
          ]
          ~f:(fun l ->
            let hashtbl = Caml.Hashtbl.create (List.length l) in
            List.iter l ~f:(fun (key, data) -> Caml.Hashtbl.add hashtbl key data);
            hashtbl)
    ; equal =
        (fun t1 t2 ->
           let to_list tbl =
             Caml.Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] |> List.sort ~compare
           in
           to_list t1 = to_list t2)
    ; sexp_of = [%sexp_of: (int32, int32) Sexplib.Std.Hashtbl.t]
    ; hi_bound = None
    ; lo_bound = Minimum.bin_size_hashtbl
    }
  ;;

  module R1 = struct
    type t =
      { x : Int32.t
      ; y : float
      }
    [@@deriving bin_io, fields, sexp_of]

    let maximum_bin_size =
      Fields.fold
        ~init:0
        ~x:(fun acc _ -> acc + Maximum.bin_size_int32)
        ~y:(fun acc _ -> acc + Maximum.bin_size_float)
    ;;

    let minimum_bin_size =
      Fields.fold
        ~init:0
        ~x:(fun acc _ -> acc + Minimum.bin_size_int32)
        ~y:(fun acc _ -> acc + Minimum.bin_size_float)
    ;;
  end

  let record1 =
    let open R1 in
    { writer = bin_write_t
    ; reader = bin_read_t
    ; values = [ { x = 0l; y = 0.0 }; { x = Int32.max_value; y = Float.max_value } ]
    ; equal = ( = )
    ; sexp_of = [%sexp_of: t]
    ; hi_bound = Some maximum_bin_size
    ; lo_bound = minimum_bin_size
    }
  ;;

  module R2 = struct
    type inner =
      { w : Int64.t
      ; x : Int32.t
      }
    [@@deriving bin_io, fields, sexp_of]

    let maximum_bin_size_of_inner =
      Fields_of_inner.fold
        ~init:0
        ~w:(fun acc _ -> acc + Maximum.bin_size_int64)
        ~x:(fun acc _ -> acc + Maximum.bin_size_int32)
    ;;

    let minimum_bin_size_of_inner =
      Fields_of_inner.fold
        ~init:0
        ~w:(fun acc _ -> acc + Minimum.bin_size_int64)
        ~x:(fun acc _ -> acc + Minimum.bin_size_int32)
    ;;

    type t =
      { y : inner
      ; z : unit
      }
    [@@deriving bin_io, fields, sexp_of]

    let maximum_bin_size =
      Fields.fold
        ~init:0
        ~y:(fun acc _ -> acc + maximum_bin_size_of_inner)
        ~z:(fun acc _ -> acc + Maximum.bin_size_unit)
    ;;

    let minimum_bin_size =
      Fields.fold
        ~init:0
        ~y:(fun acc _ -> acc + minimum_bin_size_of_inner)
        ~z:(fun acc _ -> acc + Minimum.bin_size_unit)
    ;;
  end

  let record2 =
    let open R2 in
    { writer = bin_write_t
    ; reader = bin_read_t
    ; values =
        [ { y = { w = 0L; x = 0l }; z = () }
        ; { y = { w = Int64.max_value; x = Int32.max_value }; z = () }
        ]
    ; equal = ( = )
    ; sexp_of = [%sexp_of: t]
    ; hi_bound = Some maximum_bin_size
    ; lo_bound = minimum_bin_size
    }
  ;;

  module Inline_record = struct
    type inner =
      | Inner of
          { w : Int64.t
          ; x : Int32.t
          }
      | Inner_other of unit
    [@@deriving bin_io, sexp_of, variants]

    let maximum_bin_size_of_inner =
      1
      + Variants_of_inner.fold
          ~init:0
          ~inner:(fun acc _ -> max acc (Maximum.bin_size_int64 + Maximum.bin_size_int32))
          ~inner_other:(fun acc _ -> max acc Maximum.bin_size_unit)
    ;;

    let minimum_bin_size_of_inner =
      1
      + Variants_of_inner.fold
          ~init:Int.max_value
          ~inner:(fun acc _ -> min acc (Minimum.bin_size_int64 + Minimum.bin_size_int32))
          ~inner_other:(fun acc _ -> min acc Minimum.bin_size_unit)
    ;;

    type t =
      | Outer_other of unit
      | Outer of
          { y : inner
          ; z : unit
          }
    [@@deriving bin_io, sexp_of, variants]

    let maximum_bin_size =
      1
      + Variants.fold
          ~init:0
          ~outer:(fun acc _ ->
            max acc (maximum_bin_size_of_inner + Maximum.bin_size_unit))
          ~outer_other:(fun acc _ -> max acc Maximum.bin_size_unit)
    ;;

    let minimum_bin_size =
      1
      + Variants.fold
          ~init:Int.max_value
          ~outer:(fun acc _ ->
            min acc (minimum_bin_size_of_inner + Minimum.bin_size_unit))
          ~outer_other:(fun acc _ -> min acc Minimum.bin_size_unit)
    ;;
  end

  let inline_record =
    let open Inline_record in
    { writer = bin_write_t
    ; reader = bin_read_t
    ; values =
        [ Outer { y = Inner { w = 0L; x = 0l }; z = () }
        ; Outer { y = Inner { w = Int64.max_value; x = Int32.max_value }; z = () }
        ; Outer { y = Inner_other (); z = () }
        ; Outer_other ()
        ]
    ; equal = ( = )
    ; sexp_of = [%sexp_of: t]
    ; hi_bound = Some maximum_bin_size
    ; lo_bound = minimum_bin_size
    }
  ;;
end

let buf = Bigstring.create 1024

let gen_tests t =
  let bin_protted_values =
    List.map t.values ~f:(fun v ->
      let len = t.writer buf ~pos:0 v in
      Bigstring.To_string.sub buf ~pos:0 ~len)
  in
  let hex_size =
    List.fold bin_protted_values ~init:0 ~f:(fun acc s -> Int.max acc (String.length s))
  in
  let min, max =
    List.fold2_exn
      t.values
      bin_protted_values
      ~init:(Int.max_value, 0)
      ~f:(fun (min, max) v s ->
        let len = String.length s in
        printf !"%s -> %{Sexp}" (to_hex s hex_size) (t.sexp_of v);
        Bigstring.From_string.blito ~src:s ~dst:buf ();
        let pos_ref = ref 0 in
        let v' = t.reader buf ~pos_ref in
        let len' = !pos_ref in
        let hi_bound = Option.value t.hi_bound ~default:Int.max_value in
        if len < t.lo_bound || len > hi_bound
        then printf ", bin_size outside of range %d..%d: %d" t.lo_bound hi_bound len;
        if (not (t.equal v v')) || len <> len'
        then
          printf
            !", read test failed: read %d byte%s as %{Sexp}"
            len'
            (if len' = 1 then "" else "s")
            (t.sexp_of v');
        Out_channel.output_char stdout '\n';
        Int.min min len, Int.max max len)
  in
  match t.hi_bound with
  | None ->
    if min <> t.lo_bound
    then printf "invalid lower bound: %d, expected: %d\n" min t.lo_bound
  | Some hi_bound ->
    if min <> t.lo_bound || max <> hi_bound
    then printf "invalid bounds: %d..%d, expected: %d..%d\n" min max t.lo_bound hi_bound
;;

let%expect_test "Non-integer bin_prot size tests" =
  gen_tests Tests.unit;
  [%expect {|
    00 -> ()
  |}];
  gen_tests Tests.bool;
  [%expect {|
    01 -> true
    00 -> false
  |}];
  gen_tests Tests.char;
  [%expect
    {|
    00 -> "\000"
    41 -> A
    7a -> z
    3b -> ";"
    ff -> "\255"
  |}];
  gen_tests Tests.digest;
  [%expect
    {|
    ef cd ab 89 67 45 23 01 ef cd ab 89 67 45 23 01 -> 0123456789abcdef0123456789abcdef
  |}];
  gen_tests Tests.float;
  [%expect
    {|
    3c b0 00 00 00 00 00 00 -> 2.2204460492503131E-16
    7f f0 00 00 00 00 00 00 -> INF
    7f ef ff ff ff ff ff ff -> 1.7976931348623157E+308
    7f f0 00 00 00 00 00 00 -> INF
    00 10 00 00 00 00 00 00 -> 2.2250738585072014E-308
    00 00 00 00 00 00 00 01 -> 4.94065645841247E-324
    ff f0 00 00 00 00 00 00 -> -INF
    bf f0 00 00 00 00 00 00 -> -1
    ff f0 00 00 00 00 00 00 -> -INF
    3f f0 00 00 00 00 00 00 -> 1
    3e 7a d7 f2 9a bc af 48 -> 1E-07
    00 00 00 00 00 00 00 00 -> 0
  |}];
  gen_tests Tests.float_nan;
  [%expect {|
    7f f{8,0} 00 00 00 00 00 01 -> NAN (glob)
  |}];
  gen_tests Tests.vec;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 -> ()
    3f f0 00 00 00 00 00 00 01 -> (1)
  |}];
  gen_tests Tests.float32_vec;
  [%expect {|
    .. .. .. .. 00 -> ()
    3f 80 00 00 01 -> (1)
  |}];
  gen_tests Tests.float64_vec;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 -> ()
    3f f0 00 00 00 00 00 00 01 -> (1)
  |}];
  gen_tests Tests.mat;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 00 -> ()
    3f f0 00 00 00 00 00 00 01 01 -> ((1))
  |}];
  gen_tests Tests.float32_mat;
  [%expect {|
    .. .. .. .. 00 00 -> ()
    3f 80 00 00 01 01 -> ((1))
  |}];
  gen_tests Tests.float64_mat;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 00 -> ()
    3f f0 00 00 00 00 00 00 01 01 -> ((1))
  |}];
  gen_tests Tests.bigstring;
  [%expect {|
    .. .. .. .. .. 00 -> ""
    6f 6c 6c 65 68 05 -> hello
  |}];
  gen_tests Tests.float_array;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 -> ()
    00 00 00 00 00 00 00 00 01 -> (0)
  |}];
  gen_tests Tests.ref;
  [%expect
    {|
    .. .. .. .. 00 -> 0
    .. .. .. .. 01 -> 1
    .. .. .. ff ff -> -1
    7f ff ff ff fd -> 2147483647
    80 00 00 00 fd -> -2147483648
  |}];
  gen_tests Tests.lazy_t;
  [%expect
    {|
    .. .. .. .. 00 -> 0
    .. .. .. .. 01 -> 1
    .. .. .. ff ff -> -1
    7f ff ff ff fd -> 2147483647
    80 00 00 00 fd -> -2147483648
  |}];
  gen_tests Tests.option;
  [%expect
    {|
    .. .. .. .. .. 00 -> ()
    .. .. .. .. 00 01 -> (0)
    .. .. .. .. 01 01 -> (1)
    .. .. .. ff ff 01 -> (-1)
    7f ff ff ff fd 01 -> (2147483647)
    80 00 00 00 fd 01 -> (-2147483648)
  |}];
  gen_tests Tests.pair;
  [%expect
    {|
    .. .. .. .. .. .. .. .. 00 00 -> (0 0)
    .. .. .. .. .. .. .. .. 01 01 -> (1 1)
    .. .. .. .. .. .. ff ff ff ff -> (-1 -1)
    7f ff ff ff fd 7f ff ff ff fd -> (2147483647 2147483647)
    80 00 00 00 fd 80 00 00 00 fd -> (-2147483648 -2147483648)
  |}];
  gen_tests Tests.triple;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. .. .. 00 00 00 -> (0 0 0)
    .. .. .. .. .. .. .. .. .. .. .. .. 01 01 01 -> (1 1 1)
    .. .. .. .. .. .. .. .. .. ff ff ff ff ff ff -> (-1 -1 -1)
    7f ff ff ff fd 7f ff ff ff fd 7f ff ff ff fd -> (2147483647 2147483647 2147483647)
    80 00 00 00 fd 80 00 00 00 fd 80 00 00 00 fd -> (-2147483648 -2147483648 -2147483648)
  |}];
  gen_tests Tests.list;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. 00 -> ()
    .. .. .. .. .. .. .. .. .. 00 01 -> (0)
    .. .. .. .. .. .. .. .. 01 00 02 -> (0 1)
    .. .. .. .. .. .. .. ff ff 01 02 -> (1 -1)
    .. .. .. .. 7f ff ff ff fd 00 02 -> (0 2147483647)
    80 00 00 00 fd 7f ff ff ff fd 02 -> (2147483647 -2147483648)
  |}];
  gen_tests Tests.array;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. 00 -> ()
    .. .. .. .. .. .. .. .. .. 00 01 -> (0)
    .. .. .. .. .. .. .. .. 01 00 02 -> (0 1)
    .. .. .. .. .. .. .. ff ff 01 02 -> (1 -1)
    .. .. .. .. 7f ff ff ff fd 00 02 -> (0 2147483647)
    80 00 00 00 fd 7f ff ff ff fd 02 -> (2147483647 -2147483648)
  |}];
  gen_tests Tests.record1;
  [%expect
    {|
    .. .. .. .. 00 00 00 00 00 00 00 00 00 -> ((x 0)(y 0))
    7f f0 00 00 00 00 00 00 7f ff ff ff fd -> ((x 2147483647)(y INF)) |}];
  gen_tests Tests.record2;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. .. .. 00 00 00 -> ((y((w 0)(x 0)))(z()))
    00 7f ff ff ff fd 7f ff ff ff ff ff ff ff fc -> ((y((w 9223372036854775807)(x 2147483647)))(z())) |}];
  gen_tests Tests.inline_record;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. .. .. 00 00 00 00 01 -> (Outer(y(Inner(w 0)(x 0)))(z()))
    00 7f ff ff ff fd 7f ff ff ff ff ff ff ff fc 00 01 -> (Outer(y(Inner(w 9223372036854775807)(x 2147483647)))(z()))
    .. .. .. .. .. .. .. .. .. .. .. .. .. 00 00 01 01 -> (Outer(y(Inner_other()))(z()))
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. 00 00 -> (Outer_other()) |}]
;;

(* Polymorphic hash is not the same when running in javascript.
   This makes the test fail because of different ordering. *)
let%expect_test ("Non-integer bin_prot size tests (no js)"[@tags "no-js"]) =
  gen_tests Tests.hashtbl;
  [%expect
    {|
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. 00 -> ()
    .. .. .. .. .. .. .. .. .. .. .. .. 00 00 01 -> ((0 0))
    .. .. .. .. .. .. .. .. .. .. 00 00 01 01 02 -> ((0 0)(1 1))
    .. .. 00 00 7f ff ff ff fd 7f ff ff ff fd 02 -> ((0 0)(2147483647 2147483647))
    80 00 00 00 fd 80 00 00 00 fd ff ff ff ff 02 -> ((-2147483648 -2147483648)(-1 -1))
  |}]
;;
