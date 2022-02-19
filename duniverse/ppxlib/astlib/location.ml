include Ocaml_common.Location

let set_input_name name = input_name := name

module Error = struct
  [@@@warning "-37"]

  type old_t (*IF_NOT_AT_LEAST 408 = Ocaml_common.Location.error *) = {
    loc: t;
    msg: string;
    sub: old_t list;
    if_highlight: string;
  }

  type location_report_kind (*IF_AT_LEAST 408 = Ocaml_common.Location.report_kind *) =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

  type location_msg = (Format.formatter -> unit) loc

  type location_report (*IF_AT_LEAST 408 = Ocaml_common.Location.report *) = {
    kind : location_report_kind;
    main : location_msg;
    sub : location_msg list;
  }

  type t (*IF_AT_LEAST 408 = Ocaml_common.Location.error *) (*IF_NOT_AT_LEAST 408 = old_t *)
  (** On ocaml >= 4.08: [t] is a [location_report] for which [location_report_kind] must be [Report_error]. *)

  type version_specific_t = [`New_error of location_report | `Old_error of old_t]

  let version_specific_t_of_t : t -> version_specific_t = fun x ->
    (*IF_AT_LEAST 408 `New_error x *)
    (*IF_NOT_AT_LEAST 408 `Old_error x *)

  let is_well_formed error =
    match version_specific_t_of_t error with
    | `New_error { kind = Report_error; _ } -> true
    | `New_error _ -> false
    | `Old_error _ -> true

  let string_of_location_msg (msg : location_msg) = Format.asprintf "%t" msg.txt

  let main_msg error =
    match version_specific_t_of_t error with
    | `New_error { main; _ } ->
        { txt = string_of_location_msg main; loc = main.loc }
    | `Old_error { msg; loc; _ } -> { txt = msg; loc }

  let sub_msgs error =
    match version_specific_t_of_t error with
    | `New_error { sub; _ } ->
        List.map
          (fun err -> { txt = string_of_location_msg err; loc = err.loc })
          sub
    | `Old_error { sub; _ } ->
        let rec deeply_flattened_sub_msgs acc = function
          | [] -> acc
          | { loc; msg; sub; _ } :: tail ->
              deeply_flattened_sub_msgs ({ txt = msg; loc } :: acc) (sub @ tail)
        in
        deeply_flattened_sub_msgs [] sub

  let of_exn exn =
    (*IF_AT_LEAST 406 match error_of_exn exn with | Some (`Ok e) -> Some e | None | Some `Already_displayed -> None *)
    (*IF_NOT_AT_LEAST 406 error_of_exn exn*)

  let _set_main_msg_old error msg = { error with msg }

  let _set_main_msg_new error msg =
    let txt ppf = Format.pp_print_string ppf msg in
    let main = { error.main with txt } in
    { error with main }

  let set_main_msg error msg =
    (*IF_NOT_AT_LEAST 408 _set_main_msg_old error msg*)
    (*IF_AT_LEAST 408 _set_main_msg_new error msg*)

  let _make_error_of_message_old ~sub { loc; txt } =
    let sub =
      List.map
        (fun { loc; txt } -> { loc; msg = txt; sub = []; if_highlight = txt })
        sub
    in
    { loc; msg = txt; sub; if_highlight = txt }

  let _make_error_of_message_new ~sub { loc; txt } =
    let mk_txt x ppf = Format.pp_print_string ppf x in
    let mk loc x = { loc; txt = mk_txt x } in
    {
      kind = Report_error;
      main = mk loc txt;
      sub = List.map (fun { loc; txt } -> mk loc txt) sub;
    }

  let make ~sub msg =
    (*IF_NOT_AT_LEAST 408 _make_error_of_message_old ~sub msg*)
    (*IF_AT_LEAST 408 _make_error_of_message_new ~sub msg*)

  let _set_main_loc_old error loc = { error with loc }

  let _set_main_loc_new error loc =
    let main = { error.main with loc } in
    { error with main }

  let set_main_loc error loc =
    (*IF_NOT_AT_LEAST 408 _set_main_loc_old error loc*)
    (*IF_AT_LEAST 408 _set_main_loc_new error loc*)
end

let raise_errorf ?loc msg = raise_errorf ?loc msg
