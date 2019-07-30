open Mirage

let main = foreign "Unikernel.Client" (time @-> job)

let () =
  register
    ~libraries:["conduit.mirage"; "vchan.xen"]
    ~packages:["conduit"; "vchan"]
    "vchan_client" [ main $ default_time ]
