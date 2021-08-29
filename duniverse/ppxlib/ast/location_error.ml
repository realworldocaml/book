open Import

type old_t (*IF_NOT_AT_LEAST 408 = Location.error *) = {
    loc: Location.t;
    msg: string;
    sub: old_t list;
    if_highlight: string;
  }

type location_msg = (Format.formatter -> unit) Location.loc

include struct
  [@@@warning "-37"]

  type location_report_kind (*IF_AT_LEAST 408 = Location.report_kind *) =
    | Report_error
    | Report_warning of string
    | Report_warning_as_error of string
    | Report_alert of string
    | Report_alert_as_error of string
end

type location_report (*IF_AT_LEAST 408 = Location.report *) = {
  kind : location_report_kind;
  main : location_msg;
  sub : location_msg list;
}

type t (*IF_AT_LEAST 408 = Location.error *) (*IF_NOT_AT_LEAST 408 = old_t *)

type error_type = [`Report of location_report | `Old_error of old_t]

let error_type_of_t : t -> error_type = fun x ->
  (*IF_AT_LEAST 408 `Report x *)
  (*IF_NOT_AT_LEAST 408 `Old_error x *)

let of_exn : exn -> t = fun exn ->
  (*IF_AT_LEAST 406 match Location.error_of_exn exn with None | Some `Already_displayed -> raise exn | Some (`Ok e) -> e *)
  (*IF_NOT_AT_LEAST 406 match Location.error_of_exn exn with None -> raise exn | Some e -> e*)

let of_exn exn =
  match of_exn exn with
  | t -> Some t
  | exception _ -> None

let to_extension (error : t) =
  let open Parsetree in
  let open Ast_helper in
  let mk_string_constant x = Str.eval (Exp.constant (Const.string x)) in
  match error_type_of_t error with
  | `Old_error old_error ->
    let rec extension_of_old_error ({loc; msg; if_highlight = _; sub} : old_t) =
      { Location.loc; txt = "ocaml.error" },
      PStr ((mk_string_constant msg) ::
            (List.map (fun ext -> Str.extension (extension_of_old_error ext)) sub)) in
    extension_of_old_error old_error
  | `Report report ->
    let extension_of_report ({kind; main; sub} : location_report) =
      if kind <> Report_error then
        raise (Invalid_argument "extension_of_error: expected kind Report_error");
      let str_of_pp pp_msg = Format.asprintf "%t" pp_msg in
      let extension_of_sub (sub : location_msg) =
        { Location.loc = sub.loc; txt = "ocaml.error" },
        PStr ([mk_string_constant (str_of_pp sub.txt)])
      in
      { Location.loc = main.loc; txt = "ocaml.error" },
      PStr (mk_string_constant (str_of_pp main.txt) ::
            List.map (fun msg -> Str.extension (extension_of_sub msg)) sub) in
    extension_of_report report

let register_error_of_exn f = Location.register_error_of_exn f

let _get_message_old t =
  t.msg

let _get_message_new t =
  let buff = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buff in
  t.main.txt ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buff

let message t =
  (*IF_NOT_AT_LEAST 408 _get_message_old t*)
  (*IF_AT_LEAST 408 _get_message_new t*)

let _set_message_old t msg =
  { t with msg; }

let _set_message_new t msg =
  let txt ppf = Format.pp_print_string ppf msg in
  let main = { t.main with txt; } in
  { t with main }

let set_message t msg =
  (*IF_NOT_AT_LEAST 408 _set_message_old t msg*)
  (*IF_AT_LEAST 408 _set_message_new t msg*)

let make_error_of_message_old ~loc msg ~sub =
  let sub = List.map (fun (loc, msg) -> { loc; msg; sub = []; if_highlight = msg; }) sub in
  { loc; msg; sub; if_highlight = msg; }

let make_error_of_message_new ~loc msg ~sub =
  let mk_txt x ppf = Format.pp_print_string ppf x in
  let mk loc x = { Location.loc; txt = mk_txt x; } in
  { kind = Report_error;
    main = mk loc msg;
    sub = List.map (fun (loc, msg) -> mk loc msg) sub; }

let make ~loc msg ~sub =
  (*IF_NOT_AT_LEAST 408 make_error_of_message_old ~loc msg ~sub*)
  (*IF_AT_LEAST 408 make_error_of_message_new ~loc msg ~sub*)

let raise error = raise (Location.Error error)

let update_loc_old error loc =
  { error with loc }

let update_loc_new error loc =
  let main = { error.main with loc } in
  { error with main }

let update_loc error loc =
  (*IF_NOT_AT_LEAST 408 update_loc_old error loc*)
  (*IF_AT_LEAST 408 update_loc_new error loc*)
