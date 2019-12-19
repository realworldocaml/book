"Async_unix"
============

Async_unix includes Unix-related dependencies for things like system calls and
threads. Using these, it hooks the Async_kernel scheduler up to either `epoll`
or `select`, depending on availability, and manages a thread pool that blocking
system calls run in.

API documentation for the latest release can be found
[here][https://ocaml.janestreet.com/ocaml-core/latest/doc/async/index.html].
