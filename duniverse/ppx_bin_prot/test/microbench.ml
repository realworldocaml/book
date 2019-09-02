open Core
open Core_bench.Std

let write_bin_prot writer buf ~pos a =
  let len = writer.Bin_prot.Type_class.size a in
  assert(writer.Bin_prot.Type_class.write buf ~pos a = pos + len)

let read_bin_prot reader buf ~pos_ref =
  reader.Bin_prot.Type_class.read buf ~pos_ref

let pos_ref = ref 0

let one = if Random.bool () then 1.0      else 1.0
let pi  = if Random.bool () then 3.141597 else 3.141597

let read_float x =
  let buf = Bigstring.create 100 in
  write_bin_prot Float.bin_writer_t buf x ~pos:0;
  (fun () ->
     pos_ref := 0;
     ignore(read_bin_prot Float.bin_reader_t buf ~pos_ref : float))

let write_float x =
  let buf = Bigstring.create 100 in
  (fun () -> write_bin_prot Float.bin_writer_t buf x ~pos:0)

module R = struct
  type t = { x : float; y : float } [@@deriving bin_io]
end

let read_r x =
  let buf = Bigstring.create 100 in
  write_bin_prot R.bin_writer_t buf x ~pos:0;
  (fun () ->
     pos_ref := 0;
     ignore(read_bin_prot R.bin_reader_t buf ~pos_ref : R.t))

let write_r x =
  let buf = Bigstring.create 100 in
  (fun () -> write_bin_prot R.bin_writer_t buf x ~pos:0)


let r_one : R.t = if Random.bool () then { x = 1.0; y = 1.0 } else { x = 1.0; y = 1.0 }
let r_pi  : R.t = if Random.bool () then { x = pi ; y = pi  } else { x = pi ; y = pi  }

module IR = struct
  type t =
    | A of { x : float; y : float }
    | B of { u : int  ; v : int }
  [@@deriving bin_io]
end

let read_ir x =
  let buf = Bigstring.create 100 in
  write_bin_prot IR.bin_writer_t buf x ~pos:0;
  (fun () ->
     pos_ref := 0;
     ignore(read_bin_prot IR.bin_reader_t buf ~pos_ref : IR.t))

let write_ir x =
  let buf = Bigstring.create 100 in
  (fun () -> write_bin_prot IR.bin_writer_t buf x ~pos:0)


let ir_one : IR.t = if Random.bool () then B { u = 1; v = 1 }     else B { u = 1; v = 1 }
let ir_pi  : IR.t = if Random.bool () then A { x = pi ; y = pi  } else A { x = pi ; y = pi  }

let lengths = [0; 1; 10; 100; 1000; 10_000]

let write_float_array len =
  let arr = Array.create ~len 1.0 in
  let buf = Bigstring.create 1_000_000 in
  Staged.stage (fun () -> write_bin_prot bin_writer_float_array buf arr ~pos:0)

let read_float_array len =
  let arr = Array.create ~len 1.0 in
  let buf = Bigstring.create 1_000_000 in
  write_bin_prot bin_writer_float_array buf arr ~pos:0;
  Staged.stage (fun () ->
    pos_ref := 0;
    let arr = read_bin_prot bin_reader_float_array buf ~pos_ref in
    if not (Array.length arr = len)
    then failwithf "got len %d, expected %d"
           (Array.length arr) len ())

let benchs =
  [ Bench.Test.create ~name:"write float one"  (write_float one)
  ; Bench.Test.create ~name:"read float one"   (read_float one)
  ; Bench.Test.create ~name:"write float pi"   (write_float pi)
  ; Bench.Test.create ~name:"read float pi"    (read_float pi)
  ; Bench.Test.create ~name:"write record one" (write_r r_one)
  ; Bench.Test.create ~name:"read record one"  (read_r r_one)
  ; Bench.Test.create ~name:"write record pi"  (write_r r_pi)
  ; Bench.Test.create ~name:"read record pi"   (read_r r_pi)
  ; Bench.Test.create ~name:"write inline record one" (write_ir ir_one)
  ; Bench.Test.create ~name:"read inline record one"  (read_ir ir_one)
  ; Bench.Test.create ~name:"write inline record pi"  (write_ir ir_pi)
  ; Bench.Test.create ~name:"read inline record pi"   (read_ir ir_pi)
  ; Bench.Test.create_indexed ~name:"write float array" ~args:lengths (write_float_array)
  ; Bench.Test.create_indexed ~name:"read float array"  ~args:lengths (read_float_array)
  ]

let () = Command.run (Bench.make_command benchs)
