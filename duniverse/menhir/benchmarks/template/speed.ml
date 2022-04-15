open Scanf
open Printf
let out = printf

(* This program reads and processes the information gathered by speed.sh. *)

(* This is the information we gather about one run. *)

type measurement = {
  (* number of tokens parsed *)
  tokens : float;
  (* time spent *)
  time : float;
  (* number of words allocated in minor heap *)
  minor : float;
  (* number of words allocated in major heap *)
  major : float;
  (* number of promoted words *)
  promoted : float;
  (* size of semantic value *)
  size : float;
}

(* Operations on measurements. *)

let zero =
  { tokens = 0.; time = 0.; minor = 0.; major = 0.; promoted = 0.; size = 0. }

let ( + ) m m' =
  { tokens = m.tokens +. m'.tokens
  ; time = m.time +. m'.time
  ; minor = m.minor +. m'.minor
  ; major = m.major +. m'.major
  ; promoted = m.promoted +. m'.promoted
  ; size = m.size +. m'.size
  }

let ( / ) m n =
  { tokens = m.tokens /. n
  ; time = m.time /. n
  ; minor = m.minor /. n
  ; major = m.major /. n
  ; promoted = m.promoted /. n
  ; size = m.size /. n
  }

let average ms =
  List.fold_left ( + ) zero ms / (float_of_int @@ List.length ms)

(* Reading a measurement. *)

let read f =
  let c = Scanning.from_file f in
  try
    Scanf.bscanf c
      "tokens: %f\ntime: %f\nminor: %f\nmajor: %f\npromoted: %f\nsize: %d\n"
    @@ fun tokens time minor major promoted size ->
      let size = float_of_int size in
      Scanning.close_in c;
      { tokens; time; minor; major; promoted; size }
  with
  | Scanf.Scan_failure _ ->
      eprintf "Error: file %s/%s does not conform to the expected format.\n"
        (Sys.getcwd()) f;
      exit 1

(* Printing a measurement. *)

(* In my experience, [major] and [promoted] are equal; nothing is allocated
   directly in the major heap. Thus, we print [minor + major - promoted] only. *)

let print m =
  out
    "  Time : %.1f seconds per billion tokens.\n"
    (m.time *. 1000000000.0 /. m.tokens)
  ;
  out
    "  Space: %.2f words per token.\n"
    ((m.minor +. m.major -. m.promoted) /. m.tokens)
  ;
  out
    "    among which %.2f words per token used by the semantic value.\n"
    (m.size /. m.tokens)
  ;
  out "\n"

let has_suffix suffix filename =
  Filename.check_suffix filename suffix

let remove_suffix suffix filename =
  if Filename.check_suffix filename suffix then
    Some (Filename.chop_suffix filename suffix)
  else
    None

let backends =
  Sys.readdir "backends"
  |> Array.to_list
  |> List.filter_map (remove_suffix ".backend")
  |> List.sort compare

let ( / ) =
  Filename.concat

let m backend =
  let Unix.{ st_size; _ } =
    Unix.stat
      ( "backends"
      / (backend ^ ".backend")
      / ".main.eobjs"
      / "native"
      / "dune__exe__Parser.o" )
  in
  let parser_size = Int.div st_size 1024 in (* in kilobytes *)
  let measurements =
    Array.to_list (Sys.readdir "times")
    |> List.filter (has_suffix (backend ^ ".time"))
    |> List.map (fun f -> "times" / f)
    |> List.map read
  in
  parser_size, average measurements

(* Display a comparison. *)

let () =
  let name = Sys.argv.(1) in
  out "Benchmark: %s\n\n" name;
  backends |> List.iter begin fun backend ->
    let parser_size, m = m backend in
    out "Back-end: %s\n" backend;
    out "  Parser size: %d kilobytes.\n" parser_size;
    print m
  end;
  out "End of benchmark: %s\n" name;
  flush stdout
