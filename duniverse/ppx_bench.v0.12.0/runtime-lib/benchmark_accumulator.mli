(** The point of [Benchmark_accumulator] is to provide a global place where inline
    benchmarking macros can register themselves. Once registered here, the benchmarks are
    retrieved and analyzed using [Core_bench].

    This module holds the registered benchmarks in a global hashtable indexed by library
    name.  We care about the registered benchmarks if and only if the library is being
    used in a [inline_benchmarks_runner.exe]. To avoid building this hashtable in cases
    where we will not use it, this module peeks into the commandline args of the running
    program to decide if the benchmarks should be registered or not.
*)

module Current_libname : sig
  val set   : string -> unit
  val unset : unit   -> unit
end

module Entry : sig
  type 'a indexed_spec = {
    arg_name   : string;
    arg_values : int list;
    thunk      : int -> unit -> 'a;
  }

  type test_spec =
    | Regular_thunk : ([`init] -> unit -> 'a) -> test_spec
    | Indexed_thunk : 'a indexed_spec -> test_spec

  type t = private {
    unique_id         : int;
    code              : string;
    type_conv_path    : string;
    name              : string;
    filename          : string;
    line              : int;
    startpos          : int;
    endpos            : int;
    test_spec         : test_spec;
    bench_module_name : string option;
  }

  val compare : t -> t -> int
  val get_indexed_arg_name : t -> string option
  val get_module_name_opt : t -> string option
end

(** [add_environment_var] returns true if the benchmarks should be added to the
    hashtable *)
val add_environment_var : bool

(** [lookup_lib] returns all the benchmarks from the specified library *)
val lookup_lib : libname:string -> Entry.t list

(** [add_bench] registers benchmarks with the global hashtable maintained in
    [ppx_bench_lib]. This is meant to be called by the code generated for the BENCH and
    BENCH_INDEXED macros *)
val add_bench
  :  name:string
  -> code:string
  -> filename:string
  -> type_conv_path:string
  -> line:int
  -> startpos:int
  -> endpos:int
  -> Entry.test_spec
  -> unit

(** [add_bench_module] adds a bench module name to the benchmarks. This is called by
    BENCH_MODULE macro *)
val add_bench_module
  :  name:string
  -> code:string
  -> type_conv_path:string
  -> filename:string
  -> line:int
  -> startpos:int
  -> endpos:int
  -> (unit -> unit)
  -> unit
