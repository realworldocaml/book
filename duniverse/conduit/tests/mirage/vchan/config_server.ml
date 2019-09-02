open Mirage

let main = foreign "Unikernel.Server" (time @-> job)

let () =
  register
    ~libraries:["conduit.mirage"; "vchan.xen"]
    ~packages:["conduit"; "vchan"]
    "vchan_server" [ main $ default_time ]
