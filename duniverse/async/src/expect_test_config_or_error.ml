open Core
open! Async_kernel
open! Async_unix
module IO_run = Deferred.Or_error
module Expect_test_config = Expect_test_config

module IO_flush = struct
  include Expect_test_config.IO_flush

  let to_run t = Deferred.Or_error.return t
end

let run f = Thread_safe.block_on_async_exn f |> Or_error.ok_exn
let sanitize s = s
let flushed () = true
let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
