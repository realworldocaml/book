(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** Helpers for the testing framework. *)

val test : string -> (unit -> unit) -> OUnit2.test
(** [test name f] creates two tests that run [f], one that uses [ocamlc] and one
    that uses [ocamlopt]. Each time [f] is run, some global state is updated so
    that {!compile} calls the appropriate compiler, and {!with_bisect} and
    similar functions expand to the right flags for that compiler. [f] is run
    with the process switched to a temporary directory [test/_scratch].

    The OUnit path of both generated tests includes [name]. *)

val test_directory : unit -> string
(** Path to the working directory for the current test. *)



val ocamlc_403_or_more : unit -> bool
(** [ocamlc] version is 4.03.0 or greater. *)

val ocamlc_404_or_more : unit -> bool
(** [ocamlc] version is 4.04.0 or greater. *)

val ocamlc_408_or_more : unit -> bool
(** [ocamlc] version is 4.08.0 or greater. *)

val have_package : string -> bool
(** Checks that the given Findlib package is installed using
    [ocamlfind query]. *)

val if_package : string -> unit
(** If the given package is not installed (see [have_package]), skips the
    current test case. *)



val run : string -> unit
(** Runs the given command using [Unix.system]. The command is passed to the
    shell, so redirections are supported and string escaping is necessary.
    Raises [Failure] if the exit code is not zero. *)



val compile : ?r:string -> string -> string -> unit
(** [compile flags ml_file] uses [ocamlfind] to compile [file] with the current
    compiler and the given compiler [flags]. [file] is given relative to the
    [test/] directory, for example ["report/source.ml"]. The result of
    compilation is a number of output files in the current directory
    [_scratch/], depending on the [flags]. [flags] may include options for
    [ocamlfind], such as [-package].

    If [~r] is supplied, that string is appended to the end of the command
    invocation. This is intended for redirections, e.g. [~r:"2> output"].

    Raises [Failure] if the exit code is not zero. *)

val compile_compare : (unit -> string) -> string -> OUnit2.test
(** [compile_compare flags directory] lists [.ml] files in [directory], and for
    each one [x.ml] whose name does not begin with [test_], creates test cases
    using {!test} that compile [x.ml] with flags [flags ()] and [-dsource], then
    compare the dumped output to [x.ml.reference] in [directory]. *)

val with_bisect : unit -> string
(** Flags for compiling with Bisect_ppx built by [make all] in the root
    directory of the project working tree. If concatenating these with other
    flags, be sure to separate them with spaces. *)

val with_bisect_args : string -> string
(** The same as [with_bisect], but passes the given flags to the ppx
    extension. *)

val dune_build_directory : string
(** Path to the Dune _build directory in which the Bisect_ppx under test is
    installed. *)



val report :
  ?env:(string * string) list -> ?f:string -> ?r:string -> string -> unit
(** [report flags] runs [bisect-ppx-report] built by [make all] in the root
    directory of the project working tree with the given flags.

    If [~env] is supplied, the environment is extended with the given
    environment variables.

    If [~r] is supplied, that string is appended to the end of the command
    invocation. This is intended for redirections.

    If [~f] is supplied, [report] uses the pattern to find [*.coverage] files.
    The default value is [bisect*.coverage]. *)

val diff : ?preserve_as:string -> string -> unit
(** [diff f] runs the command [diff] between the file [f] and [_scratch/output].
    [f] is given relative to [test/], e.g. ["report/reference.html"]. If there
    is a difference, [diff] fails the current test case, and includes the
    difference in the error message.

    [_scratch/output] is then copied to the [_preserve] subdirectory, under a
    matching filename - e.g. ["_preserve/report/reference.html"]. This preserved
    output can be used to replace [f] if, in fact, the output is correct:

      make save-test-output

    Sometimes, [f] is the name of an intermediate generated file, rather than an
    original reference file under source control. In this case, [~preserve_as]
    can be used to override the name under which [_scratch/output] is preserved.
    If the argument is provided, output is copied to
    ["_preserve/" ^ preserve_as]. *)

val normalize_source : string -> string -> unit
(** [normalize_source source normalized] uses [compiler-libs] to parse the file
    [source], then prints it back to [normalized] as if by [-dsource] (i.e.,
    using [Pprintast].) *)

val diff_ast : string -> unit
(** Same as [diff], but first applies [normalize_source] to the given reference
    file. The [diff] command is then run between [_scratch/output] and the
    normalized source. Use [diff_ast] for comparing [.ml] files. *)
