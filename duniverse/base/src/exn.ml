open! Import

type t = exn [@@deriving_inline sexp_of]

let sexp_of_t = (sexp_of_exn : t -> Sexplib0.Sexp.t)

[@@@end]

let exit = Caml.exit

exception Finally of t * t [@@deriving_inline sexp]

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Finally] (function
    | Finally (arg0__001_, arg1__002_) ->
      let res0__003_ = sexp_of_t arg0__001_
      and res1__004_ = sexp_of_t arg1__002_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "exn.ml.Finally"; res0__003_; res1__004_ ]
    | _ -> assert false)
;;

[@@@end]

exception Reraised of string * t [@@deriving_inline sexp]

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Reraised] (function
    | Reraised (arg0__005_, arg1__006_) ->
      let res0__007_ = sexp_of_string arg0__005_
      and res1__008_ = sexp_of_t arg1__006_ in
      Sexplib0.Sexp.List
        [ Sexplib0.Sexp.Atom "exn.ml.Reraised"; res0__007_; res1__008_ ]
    | _ -> assert false)
;;

[@@@end]

exception Sexp of Sexp.t

(* We install a custom exn-converter rather than use:

   {[
     exception Sexp of Sexp.t [@@deriving_inline sexp]
     (* ... *)
     [@@@end]
   ]}

   to eliminate the extra wrapping of [(Sexp ...)]. *)
let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Sexp] (function
    | Sexp t -> t
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false)
;;

let create_s sexp = Sexp sexp

let raise_with_original_backtrace t backtrace =
  Caml.Printexc.raise_with_backtrace t backtrace
;;

external is_phys_equal_most_recent : t -> bool = "Base_caml_exn_is_most_recent_exn"

let reraise exn str =
  let exn' = Reraised (str, exn) in
  if is_phys_equal_most_recent exn
  then (
    let bt = Caml.Printexc.get_raw_backtrace () in
    raise_with_original_backtrace exn' bt)
  else raise exn'
;;

let reraisef exc format = Printf.ksprintf (fun str () -> reraise exc str) format
let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)
let to_string_mach exc = Sexp.to_string_mach (sexp_of_exn exc)
let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : _ -> unit) =
  match f x with
  | res ->
    finally x;
    res
  | exception exn ->
    let bt = Caml.Printexc.get_raw_backtrace () in
    (match finally x with
     | () -> raise_with_original_backtrace exn bt
     | exception final_exn ->
       (* Unfortunately, the backtrace of the [final_exn] is discarded here. *)
       raise_with_original_backtrace (Finally (exn, final_exn)) bt)
;;

let protect ~f ~finally = protectx ~f () ~finally

let does_raise (type a) (f : unit -> a) =
  try
    ignore (f () : a);
    false
  with
  | _ -> true
;;

include Pretty_printer.Register_pp (struct
    type t = exn

    let pp ppf t =
      match sexp_of_exn_opt t with
      | Some sexp -> Sexp.pp_hum ppf sexp
      | None -> Caml.Format.pp_print_string ppf (Caml.Printexc.to_string t)
    ;;

    let module_name = "Base.Exn"
  end)

let print_with_backtrace exc raw_backtrace =
  Caml.Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@\n@." pp exc;
  if Caml.Printexc.backtrace_status ()
  then Caml.Printexc.print_raw_backtrace Caml.stderr raw_backtrace;
  Caml.flush Caml.stderr
;;

let set_uncaught_exception_handler () =
  Caml.Printexc.set_uncaught_exception_handler print_with_backtrace
;;

let handle_uncaught_aux ~do_at_exit ~exit f =
  try f () with
  | exc ->
    let raw_backtrace = Caml.Printexc.get_raw_backtrace () in
    (* One reason to run [do_at_exit] handlers before printing out the error message is
       that it helps curses applications bring the terminal in a good state, otherwise the
       error message might get corrupted.  Also, the OCaml top-level uncaught exception
       handler does the same. *)
    if do_at_exit
    then (
      try Caml.do_at_exit () with
      | _ -> ());
    (try print_with_backtrace exc raw_backtrace with
     | _ ->
       (try
          Caml.Printf.eprintf "Exn.handle_uncaught could not print; exiting anyway\n%!"
        with
        | _ -> ()));
    exit 1
;;

let handle_uncaught_and_exit f = handle_uncaught_aux f ~exit ~do_at_exit:true

let handle_uncaught ~exit:must_exit f =
  handle_uncaught_aux f ~exit:(if must_exit then exit else ignore) ~do_at_exit:must_exit
;;

let reraise_uncaught str func =
  try func () with
  | exn ->
    let bt = Caml.Printexc.get_raw_backtrace () in
    raise_with_original_backtrace (Reraised (str, exn)) bt
;;

external clear_backtrace : unit -> unit = "Base_clear_caml_backtrace_pos" [@@noalloc]

let raise_without_backtrace e =
  (* We clear the backtrace to reduce confusion, so that people don't think whatever
     is stored corresponds to this raise. *)
  clear_backtrace ();
  Caml.raise_notrace e
;;

let initialize_module () = set_uncaught_exception_handler ()

module Private = struct
  let clear_backtrace = clear_backtrace
end
