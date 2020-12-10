
open Printf

(*** Type definitions for json-static ***)

open Test

module C =
struct
  type t = char
  let to_json x = Json_type.Int (Char.code x)
  let of_json = function
      Json_type.Int i when i >= 0 && i < 256 -> (Char.chr i)
    | _ -> failwith "corrupted json char"
  let t = `Int
end

module I32 =
struct
  type t = int32
  let to_json x = Json_type.String (Int32.to_string x)
  let of_json = function
      Json_type.String s -> (Int32.of_string s)
    | _ -> failwith "corrupted json int32"
  let t = `String
end

module I64 =
struct
  type t = int64
  let to_json x = Json_type.String (Int64.to_string x)
  let of_json = function
      Json_type.String s -> (Int64.of_string s)
    | _ -> failwith "corrupted json int64"
  let t = `String
end

let json_of_unit () = Json_type.Null
let unit_of_json = function
    Json_type.Null -> ()
  | _ -> failwith "error: expected null"

type json test_variant = predefined
    [ `Case1 | `Case2 of int | `Case3 of string | `Case4 of test_variant list ]

and mixed_record = predefined {
  ?field0 : int option;
  ?field1 : float option;
  field2 : string option;
  field3 : I64.t;
  field4 : float array;
  ?field5 : bool option;
  ?field6 : string option;
  field7 : test_variant;
  field8 : string array;
  field9 : (
    int
    * int
    * C.t
    * int
    * I32.t
    * I64.t
  );
  field10 : bool;
  ?field11 : bool = false;
  field12 : unit list;
  field13 : string option list
}

and mixed = (mixed_record array * mixed_record array) list



(*** Creation of sample data for testing ***)

let make_mixed_record_array n =
  Array.init n (fun i ->
    {
      field0 = Some i;
      field1 = Some 0.555;
      field2 = Some (String.copy "abcdefghijklmnopqrstuvwxyz");
      field3 = 12345678L;
      field4 = [| 1.23; 3.45; 4.56 |];
      field5 = None;
      field6 = None;
      field7 = `Case4 [ `Case1; `Case2 999; `Case3 "abcdefghij"; `Case4 [] ];
      field8 = [| "a"; "bc"; "def"; "ghij"; "klmno";
                  "pqrstu"; "vwxyz01"; "23456789" |];
      field9 = (
        1_000_000,
        0xff,
        '\xff',
        0xffff,
        0xffffffffl,
        0xffffffffffffffffL
      );
      field10 = true;
      field11 = false;
      field12 = [ (); () ];
      field13 = [ Some "abcdefgh"; None; Some "qwerty" ]
    }
  )

let make_mixed ~top_len ~tab_len ~ar_len =
  List.init top_len (fun _ ->
    (make_mixed_record_array tab_len, make_mixed_record_array ar_len)
  )


(*** Benchmarking ***)

let time s f x =
  printf "%s: %!" s;
  let t1 = Unix.gettimeofday () in
  let y = f x in
  let t2 = Unix.gettimeofday () in
  printf "%.3f s\n%!" (t2 -. t1);
  y

let print_length label s =
  printf "%s len = %i\n" label (String.length s)

let marshal_mixed x =
  let s = Marshal.to_string x [Marshal.No_sharing] in
  print_length "marshal" s;
  s

let unmarshal_mixed s =
  (Marshal.from_string s 0 : mixed)

let jsonstatic_of_mixed x =
  let s = Json_io.string_of_json ~compact:true (json_of_mixed x) in
  print_length "json-static" s;
  s

let mixed_of_jsonstatic s =
  mixed_of_json (Json_io.json_of_string s)

let biniou_of_mixed x =
  let s = Test.string_of_mixed ~len:10_000_000 x in
  print_length "atdgen-biniou" s;
  s

let mixed_of_biniou s =
  Test.mixed_of_string s

let atdgenjson_of_mixed x =
  let s = Testj.string_of_mixed ~len:10_000_000 x in
  print_length "atdgen-json" s;
  s

let mixed_of_atdgenjson s =
  Testj.mixed_of_string s

let compact () =
  printf "[compaction]\n%!";
  Gc.compact ()

let single_perf_test () =
  let x = make_mixed ~top_len:100 ~tab_len:500 ~ar_len:500 in
  if true then (
    compact ();
    let marshal_s = time "marshal write" marshal_mixed x in
    compact ();
    ignore (time "marshal read" unmarshal_mixed marshal_s);
  );
  if true then (
    compact ();
    let biniou_s = time "atdgen-biniou write" biniou_of_mixed x in
    compact ();
    ignore (time "atdgen-biniou read" mixed_of_biniou biniou_s);
  );
  if true then (
    compact ();
    let json_s = time "atdgen-json write" atdgenjson_of_mixed x in
    compact ();
    ignore (time "atdgen-json read" mixed_of_atdgenjson json_s);
  );
  if true then (
    compact ();
    let json_s = time "json-static write" jsonstatic_of_mixed x in
    compact ();
    ignore (time "json-static read" mixed_of_jsonstatic json_s);
  )

let perf_test () =
  Gc.set { (Gc.get()) with Gc.verbose = 0x020 };
  let n = 2 in
  for i = 1 to n do
    printf "[run %i/%i]\n%!" i n;
    if i = 2 then
      Gc.set { (Gc.get()) with Gc.space_overhead = 500 };
    single_perf_test ()
  done


let () = perf_test ()
