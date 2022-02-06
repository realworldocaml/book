(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** This module provides the necessary function ({!detach}) to schedule some
    computations to be ran in parallel in a separate domain. The result of such
    a computation is exposed to the caller of {!detach} as a promise. Thus, this
    module allows to mix multicore parallelism with the concurrent-only
    scheduling of the rest of Lwt. *)

    type pool
    (** Domainslib Task pool *)

    val detach : pool-> ('a -> 'b) -> 'a -> 'b Lwt.t
    (** [detach pool f x] runs the computation [f x] in a separate domain in
        parallel.

        [detach pool f x] evaluates to an Lwt promise which is pending until the
        domain completes the execution of [f x] at which point it becomes
        resolved. If [f x] raises an exception, then the promise is rejected.

        It is recommended you initialise the task pool using
        {!setup_pool} with a number of domains equal to the number of
        physical cores.

        Note that the function [f] passed to [detach] cannot safely use {!Lwt}.
        This is true even for implicit callback arguments (i.e.,
        {!Lwt.with_value}). If you need to use {!Lwt} or interact with promises,
        you must use {!run_in_main}.

        In the special case where the task pool has size one (i.e., when there
        is no additional domain to detach the computation to), the computation
        runs immediately on the main domain. In other words, when the number of
        domains is one (1), then [detach f x] is identical to
        [Lwt.return (f x)].

        @raise [Invalid_argument] if pool is already torn down. *)

  val run_in_main : (unit -> 'a Lwt.t) -> 'a
    (** [run_in_main f] can be called from a detached computation to execute [f
        ()] in the parent domain, i.e. the one executing {!Lwt_main.run}.

        [run_in_main f] blocks until [f ()] completes, then it returns its
        result. If [f ()] raises an exception, [run_in_main f] raises the same
        exception. The whole of {!Lwt} can be safely used from within [f].
        However, note that implicit callback arguments are local to [f]. I.e.,
        {!Lwt.get} can only retrieve values set inside of [f], and not those set
        inside the promise that called [detach] that called [run_in_main].

        Note that the calling domain will be idle until [f ()] completes
        execution and returns the result. Thus, heavy use of [run_in_main] may
        lead to most or all domains being frozen. It's also possible to create a
        dead-lock when [run_in_main] is called (thus freezing a domain) with a
        function that calls [detach] (thus needing a domain). Consequently, it
        is recommended to use this function sparingly. *)

  val setup_pool : ?name:string -> int -> pool
  (** [setup_pool name num_additional_domains] returns a task pool with
      [num_additional_domains] domains including the current domain.

      It is recommended to use this function to create a pool once before
      calling [Lwt_main.run] and to not call it again afterwards. To resize the
      pool, call [teardown_pool ()] first before creating a new pool again.
      Multiple calls to resize the domain pool are safe but costly.

      If [name] is provided, the pool is mapped to name. It can be obtained
      later with [lookup_pool name].

      For more details about task pool, please refer:
      https://github.com/ocaml-multicore/domainslib/blob/master/lib/task.mli

      @raise [Invalid_argument] if given number of domains [n] is smaller than
      [1].

      @raise [Failure] if the pool is already initialised when the function is
      called.
      *)

  val teardown_pool : pool -> unit
  (** [teardown_pool ()] shuts down the task pool. It is safe to call
      [setup_pool] again after [teardown_pool] returns.

      This function is useful if different portions of your program have benefit
      from different degree of parallelism.

      @raise [TasksActive] if any tasks in the pool are currently active.

      @raise [Invalid_argument] if pool is already torn down. *)

  val lookup_pool : string -> pool option
  (** [lookup_pool name] returns [Some pool] if [pool] is associated to [name]
      or returns [None] if no value is associated to it. *)

  val get_num_domains : pool -> int
    (** [get_num_domains pool] returns the number of domains in [pool]. *)

  (**/**)
