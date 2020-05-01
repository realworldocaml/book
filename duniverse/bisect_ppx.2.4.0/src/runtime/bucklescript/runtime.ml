(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let get_coverage_data =
  Bisect_common.runtime_data_to_string

let write_coverage_data () =
  match get_coverage_data () with
  | None ->
    ()
  | Some data ->
    let rec create_file attempts =
      let filename = Bisect_common.random_filename "bisect" in
      match Node.Fs.openSync filename `Write_fail_if_exists with
      | exception exn ->
        if attempts = 0 then
          raise exn
        else
          create_file (attempts - 1)
      | _ ->
        Node.Fs.writeFileSync filename data `binary
    in
    create_file 100

let reset_coverage_data =
  Bisect_common.reset_counters

let node_at_exit = [%bs.raw {|
  function (callback) {
    if (typeof process !== 'undefined' && typeof process.on !== 'undefined')
      process.on("exit", callback);
  }
|}]

let exit_hook_added = ref false

let write_coverage_data_on_exit () =
  if not !exit_hook_added then begin
    node_at_exit (fun () -> write_coverage_data (); reset_coverage_data ());
    exit_hook_added := true
  end

let register_file
    ~bisect_file:_ ~bisect_silent:_ file ~point_count ~point_definitions =
  write_coverage_data_on_exit ();
  Bisect_common.register_file file ~point_count ~point_definitions
