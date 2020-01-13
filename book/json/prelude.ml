#require "core,core.top,ppx_jane,yojson";;

open Core_kernel;;

let () = Printexc.record_backtrace false

let () = print_endline (Sys.getcwd ())

let json = Yojson.Basic.from_file "examples/json/book.json"
