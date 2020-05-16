open Printf

(*
let split s = Str.split (Str.regexp ",") s
*)

let split s =
  let acc = ref [] in
  let stop = ref (String.length s) in
  for i = !stop - 1 downto 0 do
    if s.[i] = ',' then (
      let start = i + 1 in
      acc := String.sub s start (!stop - start) :: !acc;
      stop := i
    )
  done;
  String.sub s 0 !stop :: !acc


let load_lines accu s =
  let ic = open_in s in
  let l = ref accu in
  (try
     while true do
       l := input_line ic :: List.rev !l
     done
   with End_of_file ->
     close_in ic
  );
  !l

let load ic =
  let buf = Buffer.create 1000 in
  try
    while true do
      Buffer.add_char buf (input_char ic);
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

let ( // ) = Filename.concat

let default_dict_path () =
  try
    match Sys.os_type with
        "Unix" -> Some (Sys.getenv "HOME" // ".bdump-dict")
      | "Win32" -> Some (Sys.getenv "HOMEPATH" // "_bdump-dict")
      | "Cygwin" -> Some (Sys.getenv "HOME" // ".bdump-dict")
      | _ -> None
  with Not_found ->
    None

let load_dictionary dic_file accu =
  match dic_file with
      None -> accu
    | Some fn ->
        if Sys.file_exists fn then
          try
            load_lines accu fn
          with e ->
            failwith (sprintf "Cannot load dictionary from %S: %s\n%!"
                        fn (Printexc.to_string e))
        else
          accu

let write_uniq oc a =
  if Array.length a > 0 then (
    fprintf oc "%s\n" a.(0);
    ignore (
      Array.fold_left (
        fun last x ->
          if last <> x then
            fprintf oc "%s\n" x;
          x
      ) a.(0) a
    )
  )

let save_dictionary dic_file l =
  match dic_file with
      None -> ()
    | Some fn ->
        let a = Array.of_list l in
        Array.sort String.compare a;
        let oc = open_out fn in
        let finally () = close_out_noerr oc in
        try
          write_uniq oc a;
          finally ()
        with e ->
          finally ();
          raise e
