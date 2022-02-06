(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(* Basic types and file [bisect*.coverage] file identifier. Shared with the
   reporter. *)

type instrumented_file = {
  filename : string;
  points : int array;
  counts : int array;
}

type coverage = (string, instrumented_file) Hashtbl.t

let coverage_file_identifier = "BISECT-COVERAGE-4"



(* Output functions for the [bisect*.coverage] file format. *)

let write_int formatter i =
  Format.fprintf formatter " %i" i

let write_string formatter s =
  Format.fprintf formatter " %i %s" (String.length s) s

let write_array write_element formatter a =
  Format.fprintf formatter " %i" (Array.length a);
  Array.iter (write_element formatter) a

let write_list write_element formatter l =
  Format.fprintf formatter " %i" (List.length l);
  List.iter (write_element formatter) l

let write_instrumented_file formatter {filename; points; counts} =
  write_string formatter filename;
  write_array write_int formatter points;
  write_array write_int formatter counts

let write_coverage formatter coverage =
  Format.fprintf formatter "%s" coverage_file_identifier;
  write_list write_instrumented_file formatter coverage;
  Format.pp_print_flush formatter ()



(* Accumulated visit counts. This is used only by the native and ReScript
   runtimes. It is idly linked as part of this module into the PPX and reporter,
   as well, but not used by them. *)

let coverage : coverage Lazy.t =
  lazy (Hashtbl.create 17)

let register_file ~filename ~points =
  let counts = Array.make (Array.length points) 0 in
  let coverage = Lazy.force coverage in
  if not (Hashtbl.mem coverage filename) then
    Hashtbl.add coverage filename {filename; points; counts};
  `Visit (fun index ->
    let current_count = counts.(index) in
    if current_count < max_int then
      counts.(index) <- current_count + 1)

let flatten_coverage coverage =
  Hashtbl.fold (fun _ file acc -> file::acc) coverage []

let flatten_data () =
  flatten_coverage (Lazy.force coverage)

let reset_counters () =
  Lazy.force coverage
  |> Hashtbl.iter begin fun _ {counts; _} ->
    match Array.length counts with
    | 0 -> ()
    | n -> Array.fill counts 0 (n - 1) 0
  end



(** Helpers for serializing the coverage data in {!coverage}. *)

let runtime_data_to_string () =
  match flatten_data () with
  | [] ->
    None
  | data ->
    let buffer = Buffer.create 4096 in
    write_coverage (Format.formatter_of_buffer buffer) data;
    Some (Buffer.contents buffer)

let write_runtime_coverage coverage channel =
  write_coverage (Format.formatter_of_out_channel channel) (flatten_coverage coverage)

let write_runtime_data channel =
  write_coverage (Format.formatter_of_out_channel channel) (flatten_data ())

let prng =
  Random.State.make_self_init () [@coverage off]

let random_filename ~prefix =
  Printf.sprintf "%s%09d.coverage"
    prefix (abs (Random.State.int prng 1000000000))
