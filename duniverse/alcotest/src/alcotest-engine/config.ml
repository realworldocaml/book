open! Utils

type bound = [ `Unlimited | `Limit of int ]

type 'a with_options =
  ?and_exit:bool ->
  ?verbose:bool ->
  ?compact:bool ->
  ?tail_errors:[ `Unlimited | `Limit of int ] ->
  ?quick_only:bool ->
  ?show_errors:bool ->
  ?json:bool ->
  ?filter:Re.re option * int list option ->
  ?log_dir:string ->
  ?bail:bool ->
  'a

(* User configs before defaults have been applied. *)
module User = struct
  type t = {
    and_exit : bool option;
    verbose : bool option;
    compact : bool option;
    tail_errors : bound option;
    quick_only : bool option;
    show_errors : bool option;
    json : bool option;
    filter : (Re.re option * int list option) option;
    log_dir : string option;
    bail : bool option;
  }

  (* Lift a config-sensitive function to one that consumes optional arguments that
     override config defaults. *)
  let kcreate : 'a. (t -> 'a) -> 'a with_options =
   fun f ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors ?json
       ?filter ?log_dir ?bail ->
    f
      {
        and_exit;
        verbose;
        compact;
        tail_errors;
        quick_only;
        show_errors;
        json;
        filter;
        log_dir;
        bail;
      }

  let create : (unit -> t) with_options = kcreate (fun t () -> t)
end

type t =
  < and_exit : bool
  ; verbose : bool
  ; compact : bool
  ; tail_errors : bound
  ; quick_only : bool
  ; show_errors : bool
  ; json : bool
  ; filter : Re.re option * int list option
  ; log_dir : string
  ; bail : bool >

let apply_defaults ~default_log_dir : User.t -> t =
 fun {
       and_exit;
       verbose;
       compact;
       tail_errors;
       quick_only;
       show_errors;
       json;
       filter;
       log_dir;
       bail;
     } ->
  object
    method and_exit = Option.value ~default:true and_exit
    method verbose = Option.value ~default:false verbose
    method compact = Option.value ~default:false compact
    method tail_errors = Option.value ~default:`Unlimited tail_errors
    method quick_only = Option.value ~default:false quick_only
    method show_errors = Option.value ~default:false show_errors
    method json = Option.value ~default:false json
    method filter = Option.value ~default:(None, None) filter
    method log_dir = Option.value ~default:default_log_dir log_dir
    method bail = Option.value ~default:false bail
  end
