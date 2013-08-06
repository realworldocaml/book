open Core.Std

module type S = sig
  type t
  type config with sexp

  val create         : config -> t
  val name           : string
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

module type Instance = sig
  module Service : S
  val this : Service.t
end

type 'a t = (module S with type config = 'a)

module Bundle = struct
  type 'a service = 'a t
  type t = { instances: (module Instance) String.Table.t; }

  let create () =
    { instances = String.Table.create () }

  let register (type config) t service config  =
    let module Instance = struct
      module Service = (val service : S with type config = config)
      let this = Service.create config
    end
    in
    let key = Instance.Service.name in
    if Hashtbl.mem t.instances key then
      Or_error.error_string
        ("Attempt to register duplicate handler for " ^ key)
    else (
      Hashtbl.replace t.instances ~key ~data:(module Instance : Instance);
      Ok ()
    )
  ;;

  let unregister t service_name =
    Hashtbl.remove t.instances service_name

  let handle_request t sexp =
    match sexp with
    | Sexp.List [Sexp.Atom name;query] ->
      begin match Hashtbl.find t.instances name with
      | None -> Or_error.error_string ("Unknown service: "^name)
      | Some (module Instance) ->
        try Instance.Service.handle_request Instance.this query
        with exn -> Error (Error.of_exn exn)
      end
    | _ -> Or_error.error_string "Malformed query"
end


