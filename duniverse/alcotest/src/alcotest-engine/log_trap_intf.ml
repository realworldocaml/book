open! Import
open Model

(** Running tests have their output hidden by default to avoid cluttering the
    Alcotest display with irrelevant output. However, we (usually) want to keep
    the logs on disk so that we can re-display them if a test fails. Logs are
    stored with the following structure:

    {[
      <log_capture_root_dir>
      ├── E0965BF9/...
      ├── 6DDB68D5/                 ;; ID for each test run
      │   │
      │   ├── alpha.000.output      ;; ... containing files for individual tests
      │   ├── alpha.001.output      ;;     with format <test_name>.<index>.output.
      │   └── beta.000.output
      │
      ├── latest/                   ;; Symlink to most recent UUID
      └── <suite_name>/             ;; Symlink to most recent <suite_name> UUID
    ]} *)
module type S = sig
  type 'a promise
  type t

  val inactive : t
  val active : root:string -> uuid:string -> suite_name:string -> t

  val with_captured_logs :
    t -> Test_name.t -> ('a -> 'b promise) -> 'a -> 'b promise
  (** Capture all logs for a given test run. *)

  val recover_logs :
    t ->
    tail:[ `Unlimited | `Limit of int ] ->
    Test_name.t ->
    (Format.formatter -> unit) option
  (** Print the logs for a given test to the given formatter, if they exist.
      [tail] determines whether to show all lines in the captured log or just a
      suffix of them. *)

  val pp_current_run_dir : t -> Format.formatter -> unit
  (** Print the folder containing all captured traces for the current test run.
      Raises an exception if traces are not being recorded. *)

  val pp_log_location : t -> Test_name.t -> Format.formatter -> unit
  (** Print the file containing the trace of a particular test. Raises an
      exception if traces are not being recorded. *)
end

module type Log_trap = sig
  module type S = S

  module Make
      (Promise : Monad.EXTENDED)
      (Platform : Platform.S with type 'a promise := 'a Promise.t) :
    S with type 'a promise := 'a Promise.t
end
