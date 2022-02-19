open Ppxlib

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun _ _ -> failwith "An exception in a whole file transform")
      "raise_exc")

let () = Ppxlib.Driver.standalone ()
