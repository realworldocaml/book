let read_lines file =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !r

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c      -> Buffer.add_char buf c; loop (i + 1)
  and skip i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | 'm' -> loop (i + 1)
      | _   -> skip (i + 1)
  in
  loop 0

let run_test temp_file t =
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid =
    Unix.create_process
      "sh" [|"sh"; "-c"; t.Cram.command|] Unix.stdin fd fd
  in
  Unix.close fd;
  match snd (Unix.waitpid [] pid) with
  | WEXITED n -> n
  | _ -> 255

let () =
  let expect_test = ref None in
  let usage =
    Printf.sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name)
  in
  let anon s =
    match !expect_test with
    | None -> expect_test := Some s
    | Some _ -> raise (Arg.Bad "test must only be given once")
  in
  let non_deterministic = ref false in
  Arg.parse [
    "--non-deterministic",
    Arg.Set non_deterministic,
    "Run non-deterministic tests"
  ] anon usage;
  let expect_test =
    match !expect_test with
    | None -> raise (Arg.Bad "expect test file must be passed")
    | Some p -> p
  in
  Cram.run expect_test ~f:(fun file_contents items ->
      let temp_file = Filename.temp_file "jbuilder-test" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Cram.Line l -> Cram.pp_line ppf l
          | Cram.Test t ->
            match !non_deterministic, t.Cram.non_deterministic with
            | false, `Command ->
              (* the command is non-deterministic so skip everything *)
              List.iter (Cram.pp_line ppf) t.Cram.lines
            | false, `Output ->
              (* the command's output is non-deterministic; run it but
                 keep the old output. *)
              let _ = run_test temp_file t in
              List.iter (Cram.pp_line ppf) t.Cram.lines
            | _ ->
              let n = run_test temp_file t in
              let lines = read_lines temp_file in
              let output =
                let output = List.map (fun x -> `Output x) lines in
                if Cram.equal_output output t.output then t.output else output
              in
              Fmt.pf ppf "  $ %s\n" t.Cram.command;
              List.iter (function
                  | `Ellipsis    -> Fmt.pf ppf "  ...\n"
                  | `Output line -> Fmt.pf ppf "  %s\n" (ansi_color_strip line)
                ) output;
              Cram.pp_exit_code ppf n
        ) items;
      Format.pp_print_flush ppf ();
      Buffer.contents buf)
