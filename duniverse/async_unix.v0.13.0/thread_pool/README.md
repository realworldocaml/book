# Thread_pool

A thread pool is a set of OCaml threads used to do work, where each
piece of work is simply a thunk.

The `Async_unix` library uses `Thread_pool` to make blocking system
calls.
