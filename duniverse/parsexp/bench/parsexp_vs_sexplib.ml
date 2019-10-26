open Core_kernel

let data =
  let data_fn = "data.sexp" in
  if Sys.file_exists data_fn then
    sprintf "(%s)" (In_channel.read_all data_fn)
  else begin
    (* Collect all the jbuilds in ${ROOT}/lib *)
    let ic = Unix.open_process_in "find ../../../lib -name jbuild -exec cat {} \\;" in
    let s = In_channel.input_all ic in
    assert (Unix.close_process_in ic = WEXITED 0);
    sprintf "(%s)" s
  end

(* To make sure the input is valid before starting the bench *)
let _ : Sexp.t = Sexplib.Sexp.of_string data
let (_, positions) = Parsexp.Single_and_positions.parse_string_exn data

let () =
  let pos_mem_kb = Parsexp.Positions.memory_footprint_in_bytes positions / 1024 in
  printf "input size: %d KB\n\
          position set size: %d KB (%d KW)\n%!"
    (String.length data / 1024)
    pos_mem_kb
    (pos_mem_kb /
     match Word_size.word_size with
     | W32 -> 4
     | W64 -> 8)

(* Obj.tag is a C call so the compiler can't consider the value as dead-code *)
let don't_optimize_out x = ignore (Obj.tag (Obj.repr x) : int)

let%bench "sexplib" =
  don't_optimize_out (Sexplib.Sexp.of_string data : Sexp.t)

let%bench "sexplib.annotated" =
  don't_optimize_out (Sexplib.Sexp.Annotated.of_string data : Sexp.Annotated.t)

let%bench "parsexp" =
  don't_optimize_out (Parsexp.Single.parse_string_exn data : Sexp.t)

let%bench "parsexp+positions" =
  don't_optimize_out (Parsexp.Single_and_positions.parse_string_exn data : _ * _)
