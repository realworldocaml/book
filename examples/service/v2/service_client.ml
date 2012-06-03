open Core.Std

(** Handles a single request coming from stdin *)
let handle_one bundle =
  printf ">>> %!"; (* prompt *)
  match In_channel.input_line stdin with
  | None -> `Stop (* terminate on end-of-stream, so Ctrl-D will exit *)
  | Some line ->
    let line = String.strip line in (* drop leading and trailing whitespace *)
    if line = "" then `Continue
    else match Or_error.try_with (fun () -> Sexp.of_string line) with
    | Error err ->
      eprintf "Couldn't parse query: %s\n%!" (Error.to_string_hum err);
      `Continue
    | Ok query_sexp ->
      let resp = Service.Bundle.handle_request bundle query_sexp in
      Sexp.output_hum stdout (<:sexp_of<Sexp.t Or_error.t>> resp);
      Out_channel.newline stdout;
      `Continue
;;

let handle_loop bundle =
  let rec loop () =
    match handle_one bundle with
    | `Stop -> ()
    | `Continue -> loop ()
  in
  loop ()
;;

module Counter = struct
  type t = int ref
  type config = int with sexp

  let name = "update-counter"
  let create i = ref i

  let handle_request t sexp =
    match Or_error.try_with (fun () -> int_of_sexp sexp) with
    | Error _ as err -> err
    | Ok x ->
      t := !t + x;
      Ok (sexp_of_int !t)
end

module List_dir = struct
  type t = unit
  type config = unit with sexp

  let name = "ls"
  let create () = ()

  let handle_request () sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir -> Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end

module Loader = struct
  type t = Service.Bundle.t * (module Service.S) list
  type config = t sexp_opaque with sexp

  let name = "loader"
  let create x = x

  type request =
  | Load of string * Sexp.t  (* create and load the named service with
                                the provided config *)
  | Unload of string         (* unload the named service *)
  | Known_services
  with sexp

  let register_service bundle service config_sexp =
    let module New_service = (val service : Service.S) in
    match
      Or_error.try_with
        (fun () -> New_service.config_of_sexp config_sexp)
    with
    | Error _ as err -> err
    | Ok config ->
      Service.Bundle.register bundle
        (module New_service : Service.S with type config = New_service.config)
        config
  ;;

  let handle_request (bundle,services) sexp =
    match Or_error.try_with (fun () -> request_of_sexp sexp) with
    | Error _ as err -> err
    | Ok request ->
      match request with
      | Known_services ->
        let service_names = List.map services ~f:(fun s ->
          let module Service = (val s : Service.S) in
          Service.name)
        in
        Ok (<:sexp_of<string list>> service_names)
      | Unload service_name ->
        Service.Bundle.unregister bundle service_name;
        Ok Sexp.unit
      | Load (service_name,config_sexp) ->
        let matching_service =
          List.find services ~f:(fun service ->
            let module Service = (val service : Service.S) in
            Service.name = service_name)
        in
        match matching_service with
        | None ->
          Or_error.error "no matching service name"
            service_name <:sexp_of<String.t>>
        | Some service ->
          Result.map ~f:(fun () -> Sexp.unit)
            (register_service bundle service config_sexp)
end

let () =
  let bundle = Service.Bundle.create () in
  let loadable_services =
    [ (module List_dir : Service.S);
      (module Counter : Service.S);
    ]
  in
  match
    Service.Bundle.register bundle
      (module Loader : Service.S with type config = Loader.config)
      (bundle, loadable_services)
  with
  | Ok () -> handle_loop bundle
  | Error err ->
    eprintf "Failed to load service loader\n%s\n"
      (Error.to_string_hum err);
    exit (-1)
