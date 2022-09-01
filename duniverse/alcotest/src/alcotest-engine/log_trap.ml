open Astring
open Model
open! Import
include Log_trap_intf

module Make
    (Promise : Monad.EXTENDED)
    (Platform : Platform.S with type 'a promise := 'a Promise.t) =
struct
  open Promise.Syntax

  type state = {
    root : string;
    uuid : string;
    suite_name : string;
    has_alias : bool;
  }

  type t = Inactive | Active of state

  (** Take a string path and collapse a leading [$HOME] path segment to [~]. *)
  let maybe_collapse_home path =
    match Platform.home_directory () with
    | Error _ -> path
    | Ok home -> (
        (* Astring doesn't have [cut_prefix]. *)
        match String.is_prefix ~affix:home path with
        | false -> path
        | true ->
            let tail =
              String.Sub.to_string (String.sub ~start:(String.length home) path)
            in
            "~" ^ tail)

  let inactive = Inactive

  let active ~root ~uuid ~suite_name =
    Platform.prepare_log_trap ~root ~uuid ~name:suite_name;
    let has_alias = Platform.file_exists (Filename.concat root suite_name) in
    Active { root; uuid; suite_name; has_alias }

  let pp_path = Fmt.using maybe_collapse_home Fmt.string

  let iter_lines fpath ~f =
    let ic = open_in fpath in
    try
      while true do
        f (input_line ic)
      done;
      assert false
    with End_of_file -> close_in ic

  (** Show the last lines of a log file. *)
  let pp_tail max_lines fpath ppf =
    match max_lines with
    | `Unlimited -> iter_lines fpath ~f:(Fmt.pf ppf "%s@\n")
    | `Limit limit ->
        let rev_lines = ref [] in
        iter_lines fpath ~f:(fun l -> rev_lines := l :: !rev_lines);
        let selected_lines = List.rev_head limit !rev_lines in
        let omitted_count =
          List.length !rev_lines - List.length selected_lines
        in
        let display_lines =
          if omitted_count = 0 then selected_lines
          else
            Fmt.str "... (omitting %i line%a)" omitted_count Pp.pp_plural
              omitted_count
            :: selected_lines
        in
        ListLabels.iter display_lines ~f:(Fmt.pf ppf "%s@\n")

  let log_dir ~via_symlink { suite_name; uuid; root; has_alias } =
    let via_symlink = via_symlink && has_alias in
    Filename.concat root (if via_symlink then suite_name else uuid)

  let output_fpath ~via_symlink t tname =
    Filename.concat (log_dir ~via_symlink t) (Test_name.file tname)

  let active_or_exn = function
    | Active t -> t
    | Inactive -> failwith "internal error: no log location"

  let pp_current_run_dir t ppf =
    let t = active_or_exn t in
    pp_path ppf (log_dir ~via_symlink:true t)

  let pp_log_location t tname ppf =
    let t = active_or_exn t in
    let path = output_fpath ~via_symlink:true t tname in
    pp_path ppf path

  let recover_logs t ~tail tname =
    match t with
    | Inactive -> None
    | Active t -> (
        let fpath = output_fpath ~via_symlink:false t tname in
        match Platform.file_exists fpath with
        | false -> None
        | true -> Some (fun ppf -> pp_tail tail fpath ppf))

  let with_captured_logs t tname f x =
    match t with
    | Inactive -> f x
    | Active t ->
        let fd =
          Platform.open_write_only (output_fpath ~via_symlink:false t tname)
        in
        let* () = Promise.return () in
        let+ a = Platform.with_redirect fd (fun () -> f x) in
        Platform.close fd;
        a
end
