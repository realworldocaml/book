(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Functions for test stub generation. *)

let filenames _argv =
  let usage = "arguments: [--ml-file $filename] [--c-file $filename]" in
  let ml_filename = ref ""
  and c_filename = ref ""
  and c_struct_filename = ref "" in
  let spec = Arg.([("--ml-file",
                    Set_string ml_filename, "ML filename");
                   ("--c-file",
                    Set_string c_filename, "C filename");
                   ("--c-struct-file",
                    Set_string c_struct_filename, "C struct filename");]) in
  let no_positional_args _ =
    prerr_endline "No positional arguments" in
  begin
    Arg.parse spec no_positional_args usage;
    (!ml_filename, !c_filename, !c_struct_filename)
  end

module Foreign_binder : Cstubs.FOREIGN
  with type 'a result = 'a
   and type 'a return = 'a =
struct
  type 'a fn = 'a Ctypes.fn
  type 'a return = 'a
  let (@->) = Ctypes.(@->)
  let returning = Ctypes.returning

  type 'a result = 'a
  let foreign name fn = Foreign.foreign name fn
  let foreign_value name fn = Foreign.foreign_value name fn
end

module type STUBS = functor  (F : Cstubs.FOREIGN) -> sig end

let with_open_formatter filename f =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  let close_channel () = close_out out in
  try
    let rv = f fmt in
    close_channel ();
    rv
  with e ->
    close_channel ();
    raise e

let header = "#include \"test_functions.h\""

let run ?concurrency ?errno ?(cheader="") argv ?structs specs =
  let ml_filename, c_filename, c_struct_filename = filenames argv
  in
  if ml_filename <> "" then
    with_open_formatter ml_filename
      (fun fmt -> Cstubs.write_ml ?concurrency ?errno
          fmt ~prefix:"cstubs_tests" specs);
  if c_filename <> "" then
    with_open_formatter c_filename
      (fun fmt -> 
        Format.fprintf fmt "%s@\n%s@\n" header cheader;
        Cstubs.write_c ?concurrency ?errno
          fmt ~prefix:"cstubs_tests" specs);
  begin match structs, c_struct_filename with
   | None, _ -> ()
   | Some _, "" -> ()
   | Some specs, c_filename ->
     with_open_formatter c_filename
      (fun fmt ->
        Format.fprintf fmt "%s@\n%s@\n" header cheader;
        Cstubs_structs.write_c fmt specs)
  end

