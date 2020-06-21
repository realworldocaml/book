open Pcre
open Printf

let show_array arr =
  Array.map string_of_int arr
  |> Array.to_list
  |> String.concat ";"
  |> sprintf "[|%s|]"

let new_workspace () = Array.make 50 0

let () =
  let pat =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else begin
      eprintf "%s: expected pattern argument\n" Sys.argv.(0);
      exit 1
    end
  in
  let rex = regexp pat in
  let rec find_match flags workspace = 
    print_string "> ";
    let line, eof = try read_line (), false with End_of_file -> "", true in
    match pcre_dfa_exec ~rex ~flags ~workspace line with
    | res ->
        printf "match completed: %S\n" (show_array res);
        if not eof then begin
          printf "\n *input & workspace reset*\n";
          find_match [`PARTIAL] (new_workspace ()) 
        end
    | exception (Error Partial) ->
        printf "partial match, provide more input:\n";
        find_match [`DFA_RESTART; `PARTIAL] workspace
    | exception exn ->
        begin match exn with
        | Not_found -> eprintf "pattern match failed\n"
        | Error WorkspaceSize -> eprintf "need larger workspace vector\n"
        | Error InternalError s -> eprintf "internal error: %s\n" s
        | exn -> raise exn
        end;
        exit 1
  in
  find_match [`PARTIAL] (new_workspace ())
