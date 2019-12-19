open! Async_kernel
open! Async_unix
module IO_run = Deferred

module IO_flush = struct
  include Core_kernel.Expect_test_config.IO_flush

  let to_run t = Deferred.return t
end

let flush () = ()
let run f = Thread_safe.block_on_async_exn f
let flushed () = true
let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
