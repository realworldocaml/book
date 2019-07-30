(* top-level lwt feature configuration *)

let use_libev = ref None
let use_pthread = ref None
let android_target = ref None
let libev_default = ref None
let default_configuration = ref None

let arg_bool r =
  Arg.Symbol (["true"; "false"],
              function
              | "true" -> r := Some true
              | "false" -> r := Some false
              | _ -> assert false)

let usage =
  "enable lwt.unix features\noptions are:"

let args = [
  "-use-libev", arg_bool use_libev,
    " whether to check for libev";
  "-use-pthread", arg_bool use_pthread,
    " whether to use pthread";
  "-android-target", arg_bool android_target,
    " compile for Android";
  "-libev-default", arg_bool libev_default,
    " whether to use the libev backend by default";
  "-default-configuration", arg_bool default_configuration,
    " generate a default configuration for a typical opam install"
]

let main () =
  Arg.parse args ignore usage;

  let config_file = "lwt_config" in

  begin match !default_configuration with
  | Some true -> begin
    (* Check if we have the opam command and conf-libev is installed. If so,
       behave as if -use-libev true was passed.
       opam 2.0.0 returns exit code 0 for `opam list` even if the package is not available,
       so we instead use opam config vars to detect the presence of conf-libev *)
    try
      let ch = Unix.open_process_in "opam config var conf-libev:installed 2>/dev/null" in
      match input_line ch with
      |"true" -> use_libev := Some true
      |_ -> use_libev := Some false
    with _ ->
      (* Esy users won't have opam installed in the esy sandbox. *)
      match Sys.getenv "LIBEV_CFLAGS", Sys.getenv "LIBEV_LIBS" with
      | "", "" ->
        use_libev := Some false
      | exception Not_found ->
        use_libev := Some false
      | _flags, _libs -> ()
  end
  | _ ->
    ()
  end;

  let f = open_out config_file in
  let print name var =
    match var with
    | None -> ()
    | Some var -> Printf.fprintf f "%s: %b\n" name var
  in
  print "use_libev" !use_libev;
  print "use_pthread" !use_pthread;
  print "android_target" !android_target;
  print "libev_default" !libev_default;
  close_out f

let () =
  main ()
