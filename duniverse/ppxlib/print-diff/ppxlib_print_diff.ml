open StdLabels

let patdiff_cmd ~use_color ~extra_patdiff_args =
  let args =
    List.concat
      [
        [ "-keep-whitespace" ];
        [ "-location-style omake" ];
        (if use_color then [] else [ "-ascii" ]);
        extra_patdiff_args;
      ]
  in
  String.concat ~sep:" " ("patdiff" :: args)

let print ?diff_command ?(extra_patdiff_args = []) ?(use_color = false) ~file1
    ~file2 () =
  let exec cmd =
    let cmd =
      Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1)
        (Filename.quote file2)
    in
    match Sys.command cmd with
    | 0 -> `Same
    | 1 -> `Different
    | n -> `Error (n, cmd)
  in
  match diff_command with
  | Some s -> ignore (exec s : [> `Same | `Different | `Error of int * string ])
  | None -> (
      match exec (patdiff_cmd ~use_color ~extra_patdiff_args) with
      | `Same ->
          (* patdiff produced no output, fallback to diff -u *)
          Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
          ignore
            (exec "diff -u" : [> `Same | `Different | `Error of int * string ])
      | `Different ->
          (* patdiff successfully found a difference *)
          ()
      | `Error (err_code, cmd) ->
          (* patdiff threw an error... perhaps it wasn't installed? fallback to diff -u *)
          Printf.eprintf
            "Error:\n\
             > %S exited with code %d\n\
             > Perhaps patdiff is not installed? Hint, try: opam install patdiff\n\
             > Falling back to diff -u\n\n"
            cmd err_code;
          Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
          ignore
            (exec "diff -u" : [> `Same | `Different | `Error of int * string ]))
