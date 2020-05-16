open Core
open Async

module Data = struct
  type t = (int * string) list [@@deriving sexp]
end

type t = Data.t Sexp.With_text.t String.Map.t [@@deriving sexp]

let edit_file (type a) (module A : Sexpable with type t = a) filename =
  let editor =
    match Sys.getenv "EDITOR" with
    | Some x -> x
    | None -> "emacs"
  in
  let rec loop () =
    Unix.system_exn (String.concat [ editor; " "; filename ])
    >>= fun () ->
    Reader.file_contents filename
    >>= fun text ->
    match Sexp.With_text.of_text A.t_of_sexp ~filename text with
    | Ok result -> return (Some result)
    | Error e ->
      printf "Unable to read data:\n%s\n" (Error.sexp_of_t e |> Sexp.to_string_hum);
      printf "Try again? (Y/n): ";
      Reader.read_line (Lazy.force Reader.stdin)
      >>= fun response ->
      let reread =
        match response with
        | `Eof -> true
        | `Ok s ->
          (match s |> String.lowercase |> String.strip with
           | "n" | "no" -> false
           | _ -> true)
      in
      if not reread
      then (
        printf "Abandoning edit\n";
        return None)
      else loop ()
  in
  loop ()
;;

let rec edit_loop t =
  printf "key to edit: ";
  Reader.read_line (force Reader.stdin)
  >>= function
  | `Eof -> Deferred.unit
  | `Ok key ->
    let current =
      match Map.find t key with
      | Some x -> x
      | None -> Sexp.With_text.of_value [%sexp_of: (int * string) list] []
    in
    let filename = Filename.temp_file "test" ".scm" in
    Writer.save filename ~contents:(Sexp.With_text.text current)
    >>= fun () ->
    edit_file (module Data) filename
    >>= (function
      | None -> edit_loop t
      | Some updated ->
        let t = Map.set t ~key ~data:updated in
        printf "full sexp of map:\n";
        printf "%s\n" (sexp_of_t t |> Sexp.to_string_hum);
        printf "\njust data:\n";
        printf
          "%s\n"
          (Map.map ~f:Sexp.With_text.value t
           |> [%sexp_of: Data.t String.Map.t]
           |> Sexp.to_string_hum);
        edit_loop t)
;;

let () =
  don't_wait_for (edit_loop String.Map.empty);
  never_returns (Scheduler.go ())
;;
