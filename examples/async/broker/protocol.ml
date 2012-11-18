open Core.Std
open Async.Std

module Username : Identifiable = String
module Topic    : Identifiable = String

module Message = struct
  type t = { text: string;
             topic: Topic.t;
             from: Username.t;
             time: Time.t;
           }
  with sexp, bin_io
end

let publish_rpc = Rpc.Rpc.create
  ~name:"publish"
  ~version:0
  ~bin_query:Message.bin_t
  ~bin_response:Unit.bin_t

let subscribe_rpc = Rpc.Pipe_rpc.create
  ~name:"subscribe"
  ~version:0
  ~bin_query:Topic.bin_t
  ~bin_response:Message.bin_t
  ~bin_error:String.bin_t

module Dump = struct
  type single = { topic : Topic.t;
                  message : Message.t;
                  num_subscribers: int; }
  with sexp,bin_io
  type t = single list with sexp,bin_io
end

let dump_rpc = Rpc.Rpc.create
  ~name:"dump"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Dump.bin_t


let shutdown_rpc = Rpc.Rpc.create
  ~name:"shutdown"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Unit.bin_t

