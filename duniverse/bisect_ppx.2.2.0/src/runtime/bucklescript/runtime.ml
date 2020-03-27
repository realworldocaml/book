(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let register_file =
  Bisect_common.register_file

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

let node_at_exit = [%bs.raw {|
  function (callback) {
    process.on("exit", callback);
  }
|}]

let write_coverage_data_on_exit () =
  node_at_exit write_coverage_data
