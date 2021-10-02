module type S = sig
  val time : unit -> float
  (** [time ()] returns the current timestamp, used to measure the duration of a
      testrun. *)

  val getcwd : unit -> string
  (** [getcwd ()] returns the current working directory. *)

  val prepare : base:string -> dir:string -> name:string -> unit
  (** [prepare ~base ~dir ~name] is called before test suite execution. [base]
      is the parent of the log directory, [dir] the log directory (including
      unique testrun ID), and [name] is the test name. On Unix, this function
      creates the log directory [dir] for the test output, and sets up the
      symlink [latest] to the latest result. *)

  type 'a promise
  (** The type of monadic actions handled by {!with_redirect}. *)

  val stdout_isatty : unit -> bool
  (** Return [true] if standard output refers to a terminal or console window,
      [false] otherwise. *)

  val stdout_columns : unit -> int option
  (** [stdout_columns ()] is the current width of [stdout] in columns, or [None]
      if no width can be determined (e.g. [stdout] is not a TTY). *)

  val with_redirect : string -> (unit -> 'a promise) -> 'a promise
  (** [with_redirect output_file f] is called for each test. On Unix, it it
      deals with redirection of standard streams to the [output_file]. The
      implementation of [with_redirect] has to make sure to call [f] in order to
      run the test case. *)

  val setup_std_outputs :
    ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
  (** [setup_std_outputs ~style_renderer ~utf_8 ()] is called at startup of
      alcotest and sets up the standard streams for colored output. *)

  val home_directory : unit -> (string, [ `Msg of string ]) result
  (** [home_directory ()] is the current user's HOME directory, if it exists. *)
end

module type MAKER = functor (M : Monad.S) -> S with type 'a promise := 'a M.t
