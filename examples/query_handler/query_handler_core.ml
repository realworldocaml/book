open Core.Std

module type Query_handler = sig

  (** Configuration for a query handler.  Note that this can be
      converted to and from an s-expression *)
  type config with sexp

  (** The name of the query-handling service *)
  val name : string

  (** The state o fthe query handler *)
  type t

  (** Create a new query handler from a config *)
  val create : config -> t

  (** Evaluate a given query, where both input and output are
      s-expressions *)
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

module Unique = struct
  type config = int with sexp
  type t = { mutable next_id: int }

  let name = "unique"
  let create start_at = { next_id = start_at }

  let eval t sexp =
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as err -> err
    | Ok () ->
      let response = Ok (Int.sexp_of_t t.next_id) in
      t.next_id <- t.next_id + 1;
      response
end

module List_dir = struct
  type config = string with sexp
  type t = { cwd: string }

  (** [is_abs p] Returns true if [p] is an absolute path  *)
  let is_abs p =
    String.length p > 0 && p.[0] = '/'

  let name = "ls"
  let create cwd = { cwd }

  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir ->
      let dir =
        if is_abs dir then dir else Filename.concat t.cwd dir
      in
      Result.map (Or_error.try_with (fun () -> Sys.readdir dir))
        ~f:(Array.sexp_of_t String.sexp_of_t)
end

let (l : (module Query_handler) list) =
  [ (module Unique)
  ; (module List_dir)
  ]

module type Query_handler_instance = sig
  module Query_handler : Query_handler
  val this : Query_handler.t
end


let unique_instance =
  (module struct
     module Query_handler = Unique
     let this = Unique.create 0
   end : Query_handler_instance)

let list_dir_instance =
  (module struct
     module Query_handler = List_dir
     let this = List_dir.create "/"
   end : Query_handler_instance)

let () = ignore [unique_instance; list_dir_instance]

let build_instance
      (type a)
      (module Q : Query_handler with type config = a)
      config
  =
  (module struct
     module Query_handler = Q
     let this = Q.create config
   end : Query_handler_instance)

let unique_instance = build_instance (module Unique) 0
let list_dir_instance = build_instance (module List_dir)  "/var"

let dispatch_to_list handlers name_and_query =
  match name_and_query with
  | Sexp.List [Sexp.Atom name; query] ->
    let response =
      List.find_map handlers
        ~f:(fun (module I : Query_handler_instance) ->
          if I.Query_handler.name <> name then None
          else Some (I.Query_handler.eval I.this query)
        )
    in
    begin match response with
    | Some x -> x
    | None -> Or_error.error "Could not find matching handler"
                name String.sexp_of_t
    end
  | _ ->
    Or_error.error_string "malformed query"
;;

let rec cli handlers =
  printf ">>> %!";
  let result =
    match In_channel.input_line stdin with
    | None -> `Stop
    | Some line ->
      match Or_error.try_with (fun () -> Sexp.of_string line) with
      | Error e -> `Continue (Error.to_string_hum e)
      | Ok (Sexp.Atom "quit") -> `Stop
      | Ok query ->
        begin match dispatch_to_list handlers query with
        | Error e -> `Continue (Error.to_string_hum e)
        | Ok s    -> `Continue (Sexp.to_string_hum s)
        end;
  in
  match result with
  | `Stop -> ()
  | `Continue msg ->
    printf "%s\n%!" msg;
    cli handlers
