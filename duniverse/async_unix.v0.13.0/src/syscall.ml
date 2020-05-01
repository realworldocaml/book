open Core
open Poly
module Syscall_result = Unix.Syscall_result

let max_tries = 1000

let too_many_tries =
  Error.to_exn
    (Error.create "syscall interrupted too many times" max_tries [%sexp_of: int])
;;

let too_many_tries_error = Error too_many_tries

let syscall =
  let rec loop f n =
    if n >= max_tries
    then too_many_tries_error
    else (
      match f () with
      | x -> Ok x
      | exception Unix.Unix_error (EINTR, _, _) -> loop f (n + 1)
      | exception exn -> Error exn)
  in
  fun f -> loop f 0
;;

let is_eintr r = Syscall_result.is_error r && Syscall_result.error_exn r = EINTR

let syscall_result =
  let rec loop a f n =
    if n >= max_tries
    then raise too_many_tries
    else (
      let r = f a in
      if not (is_eintr r) then r else loop a f (n + 1))
  in
  fun a f -> loop a f 0
;;

let syscall_result2 =
  let rec loop a b f n =
    if n >= max_tries
    then raise too_many_tries
    else (
      let r = f a b in
      if not (is_eintr r) then r else loop a b f (n + 1))
  in
  fun a b f -> loop a b f 0
;;

