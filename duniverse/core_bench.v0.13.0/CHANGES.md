## git version

- Changed `quota` parameter of `Run_config.t` to be either time quota or
  a number of calls. Use `Run_config.create ~quota:(Span old_quota)` to
  preserve the previous functionality.

- Made "stdless" so that one should now `open Core_bench` rather than
  `open Core_bench.Std`.

## 113.24.00

- Switched to ppx.

## 112.35.00

- Exposed the equality of `Core_bench.Std.Bench.Test.t` with
  `Core_bench.Test.t`, so that one can get the name of a test.

    This is useful for filtering based on test name.

## 112.17.00

- Updated code to follow some core changes

## 112.06.00

- Solved a problem in which OCaml 4.02 was optimizing away benchmarks,
  making them meaningless.

## 112.01.00

- fixed legacy format string

## 109.58.00

- Added support for saving inline benchmark measurements to tabular
  files for easy loading into Octave.

## 109.55.00

- Improved `bench.mli`'s generated docs and added some usage examples.

    This also partly satisfies issue #3.
- Added the ability to create groups of benchmarks with a common prefix.

    For example, the prefix "Perf" below is created in created using
    `create_group`:

    ```ocaml
    let command = Bench.make_command [
      Bench.Test.create ~name:"Time.now" (fun () ->
        ignore (Time.now ()));

      ...

      Bench.Test.create_group ~name:"Perf" [
        Bench.Test.create ~name:"TSC.now" ...
    ```

    and the output shows:

    ```
    Estimated testing time 7s (7 benchmarks x 1s). Change using -quota SECS.
    ┌───────────────────────────────────────────┬──────────┬─────────┬────────────┐
    │ Name                                      │ Time/Run │ mWd/Run │ Percentage │
    ├───────────────────────────────────────────┼──────────┼─────────┼────────────┤
    │ Time.now                                  │  41.38ns │   2.00w │     16.72% │
    │ Calibrator.calibrate                      │ 247.42ns │  32.00w │    100.00% │
    │ Perf/TSC.now                              │   7.84ns │         │      3.17% │
    │ Perf/TSC.to_time                          │   9.35ns │   2.00w │      3.78% │
    │ Perf/TSC.to_time (TSC.now ())             │  13.22ns │   2.00w │      5.34% │
    │ Perf/TSC.to_nanos_since_epoch             │  10.83ns │         │      4.38% │
    │ Perf/TSC.to_nanos_since_epoch(TSC.now ()) │  14.86ns │         │      6.00% │
    └───────────────────────────────────────────┴──────────┴─────────┴────────────┘
    ```

## 109.53.00

- Fixed a bug in `Core_bench` where the linear regression was
  sometimes supplied with spurious data.

    This showed up when doing custom regressions that allow for a non-zero
    y-intercept.

## 109.52.00

- Exposed an extensible form of `make_command` so that
  inline-benchmarking and the other tools can add more commandline
  flags.
- A significant rewrite of `Core_bench`.

    The rewrite provides largely the same functionality as the older
    version.  The most visible external change is that the API makes it
    clear that `Core_bench` performs linear regressions to come up with
    its numbers.  Further, it allows running user-specified multivariate
    regressions in addition to the built in ones.

    The underlying code has been cleaned up in many ways, some of which
    are aimed at improving the implementation of inline benchmarking
    (the `BENCH` syntax, which has not yet been released).

## 109.41.00

- Columns that have a `+` prefix are now always displayed, whereas
  columns that don't are displayed only if they have meaningful data.

- Added the ability to reload saved metrics (benchmark test data) so
  that bench can re-analyze them.

## 109.39.00

- Added support for additional predictors like minor/major GCs and
  compactions, using multi-variable linear regression.

    Replaced linear regression with multi-variable linear regression.
    The original algorithm estimated the cost of a function `f` by using
    a linear regression of the time taken to run `f` vs the number of
    runs.  The new version adds the ability to include additional
    predictors such as minor GCs, compactions etc.

    This allows a more fine-grained split-up of the running costs of a
    function, distinguishing between the time spent actually running `f`
    and the time spent doing minor GCs, major GCs or compactions.
- Added a forking option that allows benchmarks to be run in separate
  processes.

    This avoids any influence (e.g. polluting the cache, size of live
    heap words) they might otherwise have on each other.

## 109.32.00

- Changed `-save` to output compaction information.

- Added indexed tests.

    These are benchmarks of the form `int -> unit -> unit`, which can be
    profiled for a list of user specified `int`s.

## 109.30.00

- Report compaction stats

## 109.27.00

- Added R^2 error estimation.

    Adding this metric should give us a sense of how closely the given
    values fit a line.  Even dots that are fairly scattered can give
    tight confidence intervals.  We would like to have to number to have
    a sense of how much noise we have.

