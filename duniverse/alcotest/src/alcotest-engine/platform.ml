open! Import

module type S = sig
  val time : unit -> float
  (** [time ()] returns the current timestamp, used to measure the duration of a
      testrun. *)

  val getcwd : unit -> string
  (** [getcwd ()] returns the current working directory. *)

  type 'a promise
  (** The type of monadic actions handled by {!with_redirect}. *)

  val stdout_isatty : unit -> bool
  (** Return [true] if standard output refers to a terminal or console window,
      [false] otherwise. *)

  val stdout_columns : unit -> int option
  (** [stdout_columns ()] is the current width of [stdout] in columns, or [None]
      if no width can be determined (e.g. [stdout] is not a TTY). *)

  val setup_std_outputs :
    ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
  (** [setup_std_outputs ~style_renderer ~utf_8 ()] is called at startup of
      alcotest and sets up the standard streams for colored output. *)

  val log_trap_supported : bool
  (** Whether or not the test runner should trap test logs. The following
      functions are used iff this is set to [true]. *)

  val prepare_log_trap : root:string -> uuid:string -> name:string -> unit
  (** [prepare ~root ~uuid ~name] is called before test suite execution.

      - [root] is the directory used for log capturing;
      - [uuid] is the unique test execution ID;
      - [name] is the suite name. *)

  type file_descriptor

  val file_exists : string -> bool
  val open_write_only : string -> file_descriptor
  val close : file_descriptor -> unit

  val with_redirect : file_descriptor -> (unit -> 'a promise) -> 'a promise
  (** [with_redirect fd f] is called for each test. On Unix, it deals with
      redirection of standard streams to the [fd]. *)

  val home_directory : unit -> (string, [ `Msg of string ]) result
  (** [home_directory ()] is the current user's HOME directory, if it exists. *)
end

module type MAKER = functor (M : Monad.S) -> S with type 'a promise := 'a M.t
