open Core
open Import
module Fd = Raw_fd

type t = Fd.t Option_array.t

let capacity t = Option_array.length t

let create ~num_file_descrs =
  if num_file_descrs < 0
  then
    raise_s
      [%message
        "[Fd_by_descr.create] got negative [num_file_descrs]" (num_file_descrs : int)];
  Option_array.create ~len:num_file_descrs
;;

let bounds_check t file_descr =
  let i = file_descr |> File_descr.to_int in
  0 <= i && i < capacity t
;;

let bounds_check_error t file_descr =
  [%message
    "The file descriptor is not in the range that Async allows, which probably means \
     that the program has created too many file descriptors without closing them.  You \
     can cause Async to allow more file descriptors via the [ASYNC_CONFIG] environment \
     variable, like this: ASYNC_CONFIG='((max_num_open_file_descrs <NUMBER>))' foo.exe \
     arg1 arg2 ..."
      (file_descr : File_descr.t)
      ~min_file_descr:0
      ~max_file_descr:(capacity t - 1 : int)]
;;

let bounds_check_exn t file_descr =
  if not (bounds_check t file_descr) then raise_s (bounds_check_error t file_descr)
;;

let mem t file_descr =
  bounds_check t file_descr && Option_array.is_some t (file_descr |> File_descr.to_int)
;;

let find t file_descr =
  if not (bounds_check t file_descr)
  then None
  else Option_array.get t (file_descr |> File_descr.to_int)
;;

let find_exn t file_descr =
  bounds_check_exn t file_descr;
  if Option_array.is_none t (file_descr |> File_descr.to_int)
  then
    raise_s
      [%message
        "[Fd_by_descr.find_exn] got unknown file_descr" (file_descr : File_descr.t)];
  Option_array.get_some_exn t (file_descr |> File_descr.to_int)
;;

let remove t (fd : Fd.t) =
  bounds_check_exn t fd.file_descr;
  Option_array.set_none t (fd.file_descr |> File_descr.to_int)
;;

let add t (fd : Fd.t) =
  let file_descr = fd.file_descr in
  if not (bounds_check t file_descr)
  then error_s (bounds_check_error t file_descr)
  else if Option_array.is_some t (file_descr |> File_descr.to_int)
  then
    error_s
      [%message
        "Attempt to register a file descriptor with Async that Async believes it is \
         already managing."]
  else (
    Option_array.set_some t (file_descr |> File_descr.to_int) fd;
    Ok ())
;;

let fold t ~init ~f =
  let r = ref init in
  for i = 0 to capacity t - 1 do
    if Option_array.is_some t i then r := f !r (Option_array.get_some_exn t i)
  done;
  !r
;;

let foldi t ~init ~f =
  let r = ref init in
  for i = 0 to capacity t - 1 do
    if Option_array.is_some t i then r := f i !r (Option_array.get_some_exn t i)
  done;
  !r
;;

let iter t ~f =
  for i = 0 to capacity t - 1 do
    if Option_array.is_some t i then f (Option_array.get_some_exn t i)
  done
;;

(* The default sexp representation of this is huge and pollutes debug output *)
let sexp_of_t t =
  let fd_alist = foldi ~init:[] t ~f:(fun i acc fd -> (i, fd) :: acc) in
  [%sexp_of: (int * Fd.t) list] (List.rev fd_alist)
;;

let invariant t =
  try
    for i = 0 to capacity t - 1 do
      match Option_array.get t i with
      | None -> ()
      | Some fd ->
        Fd.invariant fd;
        assert (File_descr.equal (i |> File_descr.of_int) (Fd.file_descr fd))
    done
  with
  | exn -> raise_s [%message "Fd_by_descr.invariant failure" (exn : exn) ~fd:(t : t)]
;;
