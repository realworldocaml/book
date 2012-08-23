open Core.Std

module type S = sig
  type t
  type config with sexp

  val create         : config -> t
  val name           : string
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

type 'config t = (module S with type config = 'config)

module Bundle = struct
  type 'a service = 'a t
  type t = { handlers: (Sexp.t -> Sexp.t Or_error.t) String.Table.t; }

  let create services =
    { handlers = String.Table.create () }

  let register (type config) t service config  =
    let module Service = (val service : S with type config = config) in
    if Hashtbl.mem t.handlers Service.name then
      Or_error.error_string
        ("Attempt to register duplicate handler for "^Service.name)
    else
      let service = Service.create config in
      Hashtbl.replace t.handlers ~key:Service.name
        ~data:(fun sexp -> Service.handle_request service sexp);
      Ok ()
  ;;

  let unregister t service_name =
    Hashtbl.remove t.handlers service_name

  let handle_request t sexp =
    match sexp with
    | Sexp.List [Sexp.Atom name;query] ->
      begin match Hashtbl.find t.handlers name with
      | None -> Or_error.error_string ("Unknown service: "^name)
      | Some handler ->
        try handler query
        with exn -> Error (Error.of_exn exn)
      end
    | _ -> Or_error.error_string "Malformed query"
end


