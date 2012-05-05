open Core.Std

module Rotation_schedule = struct
  type t = | Bytes of int
           | No_rotation
end

module type Instance = sig
  module Log : Log_intf.S
  val this : Log_intf.t
  val rotation_schedule : Rotation_schedule.t
  val bytes_since_rotation : int ref
end

let enabled = ref true
let loggers = Stack.create ()

let add_logger (type config) logger config rotation_schedule now =
  Result.Monad_infix.(
    let module Log =
          (val logger : Log_intf.S with type config = config) in
    Log_intf.create config now
    >>| fun dest ->
    let module Instance = struct
      module Log = Log
      let this = dest
      let rotation_schedule = rotation_schedule
      let bytes_since_rotation = ref 0
    end in
    let instance = (module Instance : Instance) in
    Stack.push loggers instance
  )

let set_enabled v =
  enabled := v

let log now msg =
  if !enabled then (
    Stack.iter loggers ~f:(fun logger ->
      let module Instance = (val logger : Instance) in
      Instance.Log_destination.log Instance.this now (Lazy.force msg);
      let bytes = String.length (Lazy.force msg) in
      let bump r x = r := !r + x in
      bump Instance.bytes_since_rotation bytes;
      let module R = Rotation_schedule in
      match Instance.rotation_schedule with
      | R.No_rotation -> ()
      | R.Bytes byte_limit ->
        if !Instance.bytes_since_rotation >= byte_limit then
          Instance.Log_destination.rotate_log Instance.this now
    ))
