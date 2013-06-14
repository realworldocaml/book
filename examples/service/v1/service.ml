open Core.Std

module type S = sig
  type t
  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

module type Instance = sig
  module Service : S
  val this : Service.t
end

module Bundle = struct
  type t = { instances: (module Instance) String.Table.t; }

  let create services =
    let instances = String.Table.create () in
    List.iter services ~f:(fun service ->
      let module Instance = struct
        module Service = (val service : S)
        let this = Service.create ()
      end
      in
      let key = Instance.Service.name in
      if Hashtbl.mem instances key then
        failwith ("Attempt to register duplicate handler for " ^ key);
      Hashtbl.replace instances ~key ~data:(module Instance : Instance)
    );
    {instances}

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

  let service_names t = Hashtbl.keys t.instances

end


