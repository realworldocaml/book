(* Preload directory structure into memory *)
open Lwt

type t =
  | File of string
  | Directory

type cache = (string, t) Hashtbl.t

let is_dir fname =
  try Sys.is_directory fname
  with Sys_error err ->
   Printf.printf "WARNING: skipping %s\n%!" err;
   false

let string_of_file file =
  Lwt_unix.((stat file) >|= fun st -> st.st_size)
  >>= fun size ->
  let buf = String.create size in
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ic -> Lwt_io.read_into_exactly ic buf 0 size)
  >|= fun () -> buf
      
let cache root =
  let h = Hashtbl.create 53 in
  let rec aux ~rel =
    let fulldir = Filename.concat root rel in
    Printf.printf "Caching directory: %s\n%!" fulldir;
    Lwt_stream.iter_s
      (fun file ->
         let fullname = Filename.concat fulldir file in
         if file = "." || file = ".." || file.[0] = '.' then return ()
         else (match is_dir fullname with
         |true ->
            Hashtbl.add h (Filename.concat rel file) Directory;
            aux ~rel:fullname;
         |false ->
            string_of_file fullname;
            >>= fun buf ->
            Hashtbl.add h (Filename.concat rel file) (File buf);
            return ())
      ) (Lwt_unix.files_of_directory (if fulldir = "" then "." else fulldir))
  in
  aux ~rel:""
  >|= fun () -> h

let lookup h path =
  match Hashtbl.mem h path with
  |true -> Some (Hashtbl.find h path)
  |false -> None

let test () =
  Lwt_unix.run (
    cache ""
    >>= fun h ->
    Hashtbl.iter (fun k v ->
     Printf.printf "%s -> %s\n" k
       (match v with
        |File f -> Printf.sprintf "FILE %d" (String.length f)
        |Directory -> "DIR"
       )
    ) h;
    return ()
  )
