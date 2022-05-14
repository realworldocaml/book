open Core

let () =
  let tmp_file = Filename_unix.temp_file "version_util" "" in
  Exn.protect
    ~finally:(fun () -> Sys_unix.remove tmp_file)
    ~f:(fun () ->
      let initial_contents_of_exe = In_channel.read_all "main.exe" in
      let new_exe_contents =
        Option.value
          ~default:initial_contents_of_exe
          (Version_util.Expert.insert_version_util
             ~contents_of_exe:initial_contents_of_exe
             [ { repo = "ssh://machine//path/to/repo"
               ; version = "0123456789abcdef0123456789abcdef01234567"
               }
             ])
      in
      Out_channel.write_all tmp_file ~data:new_exe_contents;
      let perm = (Core_unix.lstat tmp_file).st_perm in
      Core_unix.chmod tmp_file ~perm:(perm lor 0o100);
      Sys_unix.command_exn (Sys.concat_quoted [ tmp_file; "version-util-must-be-set" ]);
      let check_rewrite contents_of_exe =
        (* If the exe had version util already, both rewrites should have have failed. If
           not, both should have succeeded. Either way, we should end up with the initial
           exe. *)
        let exe_contents_after_removal =
          Option.value
            ~default:contents_of_exe
            (Version_util.Expert.remove_version_util ~contents_of_exe)
        in
        assert (String.( = ) initial_contents_of_exe exe_contents_after_removal)
      in
      check_rewrite initial_contents_of_exe;
      check_rewrite new_exe_contents)
;;

let () =
  let contents_of_exe = In_channel.read_all "main.exe" in
  let occurrences =
    Version_util.Expert.For_tests.count_pattern_occurrences ~contents_of_exe
  in
  let expect =
    match Version_util.version_list with
    | [ "NO_VERSION_UTIL" ] -> 1
    | _ -> 0
  in
  [%test_result: int] ~expect occurrences
;;
