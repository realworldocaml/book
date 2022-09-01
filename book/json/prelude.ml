#require "core,core.top,yojson,core_unix.sys_unix";;

open Core;;

let () = Printexc.record_backtrace false

let () = print_endline (Sys_unix.getcwd ())

let json = Yojson.Basic.from_file "examples/book.json"
