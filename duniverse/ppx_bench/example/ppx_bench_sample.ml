open Core
(* This shows some sample uses of BENCH. Build and look at ppx_bench_sample.ml.pp in this
   directory to see how the preprocessor works. *)

(* One can specify a benchmark using the following syntax:

     let%bench "name" = expr

   In the above, the value of [expr] is ignored.  This creates a benchmark for [expr],
   that is run using the [inline_benchmarks_runner] script from the command-line. This
   workflow is similar to that of inline unit tests.
*)

let%bench "add mutable" =
  let i = ref 0 in
  for j = 1 to 10_000 do
    i := !i + j
  done;
  !i

let%bench "add functional" =
  let rec f acc j =
    if j > 10_000
    then acc
    else f (acc + j) (j + 1)
  in
  f 0 1

(* One can specify benchmarks that require some initialization using [bench_fun]. For
   example:

      let%bench_fun "name" =
        let t = create () in
        (fun () -> test_something t)

   The function returned on the RHS of [bench_fun] should have type [unit
   -> unit].

   The reason that the RHS of [bench] can be any non-arrow type, while [bench_fun] and
   [bench_indexed] have constrained types is that [bench] is a special case for writing
   terse macros, while in the other cases the macros cannot be as terse and consequently
   it is less useful to insert an ignore there.
*)

let%bench_fun "fold list" =
  let l = List.init 10_000 ~f:(fun i -> i) in
  (fun () -> List.fold l ~init:0 ~f:( + ) |> ignore)

(* One can specify benchmarks that have a variable parameter using an optional [@indexed
   <var> = <expr>] argument to [bench_fun]. Here <expr> has to be of type [int list]. In
   the example below, the parameter [len] is bound in the RHS of [bench_indexed].

   Indexed tests can be useful in highlighting non-linearities in the execution time of
   functions.
*)

let%bench_fun "fold list indexed" [@indexed len = [1;10;100;1000]] =
  let l = List.init len ~f:(fun i -> i) in
  (fun () -> List.fold l ~init:0 ~f:( + ) |> ignore)

(* We can group benchmarks together into modules and the output of
   [inline_benchmarks_runner] will reflect this grouping.

     let%bench_module "Blit tests" = (module struct

       ..some benchmarks..

     end)

   For examples of all of the above see [bench_gc.ml] and [bench_array.ml] in
   [lib/core_kernel/bench].

   Only the generated [inline_benchmarks_runner.exe] depends on [Core_bench] and other
   libraries. The library that includes the benchmarks itself does not have a dependency
   on [Core_bench]. Doing this is important so that we can add benchmarks to [Core] and
   still avoid cyclic dependencies. Finally, it is important to note that adding inline
   benchmarks should have little effect on the execution or module initialization time.
*)

let%bench_module "trivial module" = (module struct
  let%bench "trivial" = 3
end)

(* You can also use bench inside a functor. Since bench cannot figure out the module name
   (since this is not a well-defined concept), you can use the [@name_suffix] attribute
   to append an arbitrary expression (of type string) to the benchmark name.
   The following modules' benchmark names will look like:
   [pa_bench_sample.ml:Make:MakeQ_1] blah
   [pa_bench_sample.ml:Make:MakeQ_1000] blah
*)
module type Q = sig
  val j : int
end

module Make(Q : Q) = struct
  let j = Q.j

  let%bench_module "MakeQ" [@name_suffix sprintf "_%i" j] =
    (module struct
      let%bench "blah" =
        for _ = 0 to j do
          ()
        done
    end)
end

module J1 = Make(struct let j = 1 end)
module J1000 = Make(struct let j = 1_000 end)
