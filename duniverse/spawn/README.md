# SPAWN - spawning system process

[![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[travis]:         https://travis-ci.org/janestreet/spawn
[travis-img]:     https://travis-ci.org/janestreet/spawn.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/diml/ppxlib/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/bogbsm33uvh083jx?svg=true

Spawn is a small library exposing only one function:
`Spawn.spawn`. Its purpose is to start command in the
background. Spawn aims to provide a few missing features of
`Unix.create_process` such as providing a working directory as well as
improving error reporting and performance.

Errors such as directory or program not found are properly reported as
`Unix.Unix_error` exceptions, on both Unix and Windows.

On Unix, Spawn uses `vfork` by default as it is often a lot faster
than fork. There is a benchmark comparing `Spawn.spawn` to
`Unix.create_process` in `spawn-lib/bench`. If you don't trust
`vfork`, you can set the environment variable `SPAWN_USE_FORK` to make
Spawn use `fork` instead.

## Portability

Spawn is expected to be fully portable. However, so far it has only
been tested on Linux, OSX and Windows.

## Implementation

On Windows, `Spawn.spawn` simply uses the
[CreateProcess][CreateProcess] Windows function.

Under Linux, it uses a custom implementation that relies on
`fork`/`vfork` followed by `execve`. Compared to other implementations
such as `Unix.create_process` or the [posix_spawn][posixspawn] C
library call, our implementations supports the following:

- setting the current working directory of the child process
- reporting errors from `execve` to the caller

To report `execve` errors, our implementation proceeds as follow: just
before calling `fork` or `vfork` we create a pipe with the `O_CLOEXEC`
flag. After forking, the parent reads from this pipe. If the `execve`
succeeds, the pipe get closed, the `read` in the parent returns 0
bytes and the parent concludes that the `execve` succceeded. If the
`execve` fails, the child process sends the error code to the parent
via the pipe and exits.

[CreateProcess]: https://docs.microsoft.com/en-gb/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessa?redirectedfrom=MSDN
[posixspawn]: http://man7.org/linux/man-pages/man3/posix_spawn.3.html
