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
  Arg.parse [ ] anon usage;
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
          | `Output _ -> ()
          | `Part _ | `Comment _ as i -> Cram.pp_item ppf i
          | `Command s ->
            Fmt.pf ppf "  $ %s\n" s;
            let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
            let pid =
              Unix.create_process "sh" [|"sh"; "-c"; s|] Unix.stdin fd fd
            in
            Unix.close fd;
            let n =
              match snd (Unix.waitpid [] pid) with
              | WEXITED n -> n
              | _ -> 255
            in
            List.iter (fun line ->
                Fmt.pf ppf "  %s\n" (ansi_color_strip line)
              ) (read_lines temp_file);
            if n <> 0 then Printf.bprintf buf "  [%d]\n" n
        ) items;
      Buffer.contents buf)
