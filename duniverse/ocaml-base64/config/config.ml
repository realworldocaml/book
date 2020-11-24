module Config = Configurator.V1

let pre407 = {ocaml|external unsafe_set_uint16 : bytes -> int -> int -> unit = "%caml_string_set16u" [@@noalloc]|ocaml}
let standard = {ocaml|external unsafe_set_uint16 : bytes -> int -> int -> unit = "%caml_bytes_set16u" [@@noalloc]|ocaml}

type t =
  { major : int
  ; minor : int
  ; patch : int option
  ; extra : string option }

let v ?patch ?extra major minor = { major; minor; patch; extra; }

let parse s =
  try Scanf.sscanf s "%d.%d.%d+%s" (fun major minor patch extra -> v ~patch ~extra major minor)
  with End_of_file | Scanf.Scan_failure _ ->
    ( try Scanf.sscanf s "%d.%d+%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ ->
        ( try Scanf.sscanf s "%d.%d.%d" (fun major minor patch -> v ~patch major minor)
          with End_of_file | Scanf.Scan_failure _ ->
            Scanf.sscanf s "%d.%d" (fun major minor -> v major minor) ) )

let ( >|= ) x f = match x with
  | Some x -> Some (f x )
  | None -> None

let ocaml_cp ~src ~dst =
  let ic = open_in src in
  let oc = open_out dst in
  let bf = Bytes.create 0x1000 in
  let rec go () = match input ic bf 0 (Bytes.length bf) with
    | 0 -> ()
    | len -> output oc bf 0 len ; go ()
    | exception End_of_file -> () in
  go () ; close_in ic ; close_out oc
;;

let () =
  Config.main ~name:"config-base64" @@ fun t ->
  match Config.ocaml_config_var t "version" >|= parse with
  | Some version ->
    let dst = "unsafe.ml" in

    if (version.major, version.minor) >= (4, 7)
    then ocaml_cp ~src:"unsafe_stable.ml" ~dst
    else ocaml_cp ~src:"unsafe_pre407.ml" ~dst
  | None -> Config.die "OCaml version is not available"
  | exception exn -> Config.die "Got an exception: %s" (Printexc.to_string exn)
