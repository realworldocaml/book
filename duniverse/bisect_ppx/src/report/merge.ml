(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)


let output ~to_file ~coverage_files ~coverage_paths =
  let coverage =
    Input.load_coverage
      ~coverage_files ~coverage_paths ~expect:[] ~do_not_expect:[] in
  let () = Util.mkdirs (Filename.dirname to_file) in
  let oc = open_out to_file in
  try
    let () = Bisect_common.write_runtime_coverage coverage oc in
    close_out oc
  with exn ->
    close_out_noerr oc;
    raise exn
