

[@@@part "1"];;
module Loader = struct
  type config = (module Query_handler) list sexp_opaque
  [@@deriving sexp]

  type t = { known  : (module Query_handler)          String.Table.t
           ; active : (module Query_handler_instance) String.Table.t
           }

  let name = "loader"


[@@@part "2"];;
let create known_list =
    let active = String.Table.create () in
    let known  = String.Table.create () in
    List.iter known_list
      ~f:(fun ((module Q : Query_handler) as q) ->
        Hashtbl.replace known ~key:Q.name ~data:q);
    { known; active }


[@@@part "3"];;
let load t handler_name config =
    if Hashtbl.mem t.active handler_name then
      Or_error.error "Can't re-register an active handler"
        handler_name String.sexp_of_t
    else
      match Hashtbl.find t.known handler_name with
      | None ->
        Or_error.error "Unknown handler" handler_name String.sexp_of_t
      | Some (module Q : Query_handler) ->
        let instance =
          (module struct
             module Query_handler = Q
             let this = Q.create (Q.config_of_sexp config)
           end : Query_handler_instance)
        in
        Hashtbl.replace t.active ~key:handler_name ~data:instance;
        Ok Sexp.unit


[@@@part "4"];;
let unload t handler_name =
    if not (Hashtbl.mem t.active handler_name) then
      Or_error.error "Handler not active" handler_name String.sexp_of_t
    else if handler_name = name then
      Or_error.error_string "It's unwise to unload yourself"
    else (
      Hashtbl.remove t.active handler_name;
      Ok Sexp.unit
    )


[@@@part "5"];;
type request =
    | Load of string * Sexp.t
    | Unload of string
    | Known_services
    | Active_services
  [@@deriving sexp]


[@@@part "6"];;
let eval t sexp =
    match Or_error.try_with (fun () -> request_of_sexp sexp) with
    | Error _ as err -> err
    | Ok resp ->
      match resp with
      | Load (name,config) -> load   t name config
      | Unload name        -> unload t name
      | Known_services ->
        Ok (<:sexp_of<string list>> (Hashtbl.keys t.known))
      | Active_services ->
        Ok (<:sexp_of<string list>> (Hashtbl.keys t.active))
end
