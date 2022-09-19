(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** Module [Lwt_result]: explicit error handling *)

open Result

type (+'a, +'b) t = ('a, 'b) Result.t Lwt.t

let return x = Lwt.return (Ok x)
let fail e = Lwt.return (Error e)

let lift = Lwt.return
let ok x = Lwt.map (fun y -> Ok y) x
let error x = Lwt.map (fun y -> Error y) x

let map f e =
  Lwt.map
    (function
      | Error e -> Error e
      | Ok x -> Ok (f x))
    e

let map_error f e =
  Lwt.map
    (function
      | Error e -> Error (f e)
      | Ok x -> Ok x)
    e
let map_err f e = map_error f e

let catch e =
  Lwt.catch
    (fun () -> ok e)
    fail

let get_exn e =
  Lwt.bind e
    (function
      | Ok x -> Lwt.return x
      | Error e -> Lwt.fail e)

let bind e f =
  Lwt.bind e
    (function
      | Error e -> Lwt.return (Error e)
      | Ok x -> f x)
      
let bind_error e f =
  Lwt.bind e
    (function
      | Error e -> f e
      | Ok x -> Lwt.return (Ok x))
      
let bind_lwt e f =
  Lwt.bind e
    (function
      | Ok x -> ok (f x)
      | Error e -> fail e)

let bind_result e f =
  Lwt.map
    (function
      | Error e -> Error e
      | Ok x -> f x)
    e

let bind_lwt_error e f =
  Lwt.bind e
    (function
      | Error e -> Lwt.bind (f e) fail
      | Ok x -> return x)
let bind_lwt_err e f = bind_lwt_error e f

let both a b =
  let s = ref None in
  let set_once e =
    match !s with
    | None -> s:= Some e
    | Some _ -> ()
  in
  let (a,b) = map_error set_once a,map_error set_once b in
  let some_assert = function
    | None -> assert false
    | Some e -> Error e
  in
  Lwt.map
    (function
      | Ok x, Ok y -> Ok (x,y)
      | Error _, Ok _
      | Ok _,Error _
      | Error _, Error _ -> some_assert !s)
    (Lwt.both a b)

let iter f r =
  Lwt.bind r
    (function
      | Ok x -> f x
      | Error _ -> Lwt.return_unit)

let iter_error f r =
  Lwt.bind r
    (function
      | Error e -> f e
      | Ok _ -> Lwt.return_unit)

module Infix = struct
  let (>>=) = bind
  let (>|=) e f = map f e
end

module Let_syntax = struct
  module Let_syntax = struct
    let return = return
    let map t ~f = map f t
    let bind t ~f = bind t f
    let both = both
    module Open_on_rhs = struct
    end
  end
end

module Syntax = struct
  let (let*) = bind
  let (and*) = both

  let (let+) x f = map f x
  let (and+) = both
end

include Infix
