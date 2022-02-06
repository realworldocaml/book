### 1.5.0 (2021-10-09)

- Make Alcotest compatible with `js_of_ocaml.3.11.0`. Users can depend on the
  new virtual `alcotest-js` Opam library to pick up the right `js_of_ocaml`
  version automatically. (#326 #328, @hhugo @smorimoto)

- Record exception backtraces during test suite runs by default. This behaviour
  can be disabled by passing `~record_backtrace:false` to `Alcotest.run`. (#317,
  @CraigFe)

- Generate shorter unique identifiers for test runs (8-character alphanumeric,
  rather than a full 128-bit UUID). (#304, @CraigFe)

- Change `Alcotest.{char,string}` pretty-printers to use OCaml syntax on
  assertion failures (i.e. wrap with quotes and escape control characters).
  (#318, @CraigFe)

- Fix process for getting the width of attached terminals on MacOS.
  Previously, a terminal width of 80 columns was assumed. (#325, @CraigFe)

- Fix parsing of test filter ranges to allow '-' separators (e.g. `test alpha
  1-4`), as advertised in the manpage. The previously-used '..' separator is
  also supported. (#312, @CraigFe)

- Introduce an `Alcotest.V1` module that aliases the existing `Alcotest` API and
  provides a stability guarantee over major version changes. Similar versioned
  aliases also exist for the backends: `Alcotest_{async,lwt}.V1`. (#306,
  @CraigFe)

- Change the `~filter` argument to `Alcotest.run` to be a predicate over tests.
  (#305, @CraigFe)

- Renamed / removed some less frequently used modules used by the test backends:
  - `Alcotest.Unix` -> `Alcotest.Unix_platform`
  - `Alcotest_engine.{Cli,Core,Test}` -> `Alcotest_engine.V1.{Cli,Core,Test}`
  - `Alcotest.{Cli,Core}` are now gone. Use `Alcotest_engine.V1.{Cli,Core}.Make
    (Alcotest.Unix_platform)` instead.
  (#306 #309, @CraigFe)

- Avoid exporting `list_tests` in the main test APIs (`Alcotest{,_lwt,_async}`).
  Use `Alcotest_engine` directly if you want this function. (#310, @CraigFe)

### 1.4.0 (2021-04-15)

- Add `?here` and `?pos` arguments to the test assertion functions. These can be
  used to pass information about the location of the call-site, which is
  displayed in failing test output. (#291, @CraigFe)

- Add a pretty-printer for the exception raised by `Alcotest.check` and related
  functions. This allows them to be used outside of an Alcotest test runner for
  making general assertions.  (#296, @CraigFe)

- Add `--bail` option (and corresponding `ALCOTEST_BAIL` environment variable),
  which causes Alcotest to terminate after the first test failure. (#298,
  @CraigFe)

### 1.3.0 (2021-02-16)

- Add `Alcotest.triple` for testing 3-tuples. (#288, @sheepduke)

- Correctly report test suite duration with millisecond precision. (#286,
  @CraigFe)

- Improve pretty-printing of results to consider the terminal width, fixing
  several display issues due to line wrapping in small terminals. (#282,
  @CraigFe)

### 1.2.3 (2020-09-07)

- Require Dune 2.2. (#274, @CraigFe)

- Fix a bug in the handling of the `~and_exit:false` option when the test suite
  fails. (#271, @CraigFe)

### 1.2.2 (2020-08-26)

- Fail gracefully when the user supplies an empty suite name. (#265, @CraigFe)

- Fix compatibility with `fmt.0.8.8+dune` by adding a missing `fmt` dependency
  in `alcotest`'s dune file (#266, @NathanReb)

- Only show "in progress" lines when writing to a TTY. (#267, @CraigFe)

### 1.2.1 (2020-07-15)

- Surround pretty-printed diffs with quotes to make trailing whitespace more
  obvious. (#261, @CraigFe)
  
- Allow `.` characters to appear unescaped in symlinks and test directories.
  (#259, @CraigFe)

### 1.2.0 (2020-07-13)

- Add an `alcotest-mirage` package, allowing the construction of MirageOS
  unikernels that run Alcotest test suites. (#238, @hannesm @linse)

- Add `Alcotest.check'`, a variant of `Alcotest.check` with labeled arguments.
  (#239, @hartmut27)

- Add a testable for the `bytes` type. (#253, @mefyl)

- Many assorted improvements to Alcotest output formatting. (#246, @CraigFe)

- Default to `--color=always` when running inside Dune (#242, @CraigFe). The
  value can be overridden by setting the `ALCOTEST_COLOR` variable in a `dune`
  file, for example:

```dune
(env
 (_
  (env-vars
   (ALCOTEST_COLOR auto))))
```

- Support all UTF-8 characters in test names and suite names, by normalising
  them for file-system interactions. (#249, @gs0510; #246, @CraigFe)

- Fix various crashes when using non-filesystem-safe characters in test suite
  names (these break Alcotest when attempting to generate a corresponding log
  file). (#241, @mefyl; #246 @CraigFe)

### 1.1.0 (2020-04-03)

- Fix handling of CLI options for `Alcotest_{async,lwt}.run`. (#222, @CraigFe)
- Fix interleaving of ASSERT outputs with the other test code, and ensure that
  it is correctly captured in log files. (#215 #228, @icristescu @CraigFe)
- Don't raise Test_exception on Cmdliner parse failure (#234, @CraigFe)

### 1.0.1 (2020-02-12)

- Add support for an `ALCOTEST_COLOR={auto,always,never}` environment variable
  to control the colorization of terminal output. (#209, @mjambon)
- Fix handling of exceptions in `Alcotest_{async,lwt}`. (#212, @CraigFe
  @talex5)

### 1.0.0 (2020-01-14)

- Require OCaml 4.03. (#159, @hannesm)
- Change `Alcotest_{async,lwt}.test_case` to return monadic values. These must
  be run with the new `Alcotest_{async,lwt}.run` functions. See
  [examples/lwt/test.ml](https://github.com/mirage/alcotest/blob/878c500f3b25f6ebbdafc7236ebc2b7756dafe9d/examples/lwt/test.ml#L85)
  for an example of the new API. (#167, @CraigFe)
- Add generation of `latest` symlinks in the `_test` directory which point to
  the most recent test output directory. (#155, @cfcs)
- Allow all CLI options to be passed directly to `Alcotest.{run,run_with_args}`
  without use of the `argv` parameter. (#182, @CraigFe)
- Add `--compact` option for more concise result reporting. (#149,
  @andersfugmann)
- Add `--tail-errors` option for limiting the size of error logs printed to
  standard output. (#200, @mjambon)
- Add support for executing subsets of tests via the `test` subcommand. (#158,
  @CraigFe)
- Change the `float` check to include equality of `isNaN` and infinities. See
  [examples/floats.ml](https://github.com/mirage/alcotest/blob/47908b1f576d65f4418bcf778d2a1db213f5a7ff/examples/floats.ml)
  for demonstrations of the new semantics. (#152, @psafont)
- Reject test suites with colliding output directories. (#176, @CraigFe)
- Restrict the set of characters allowed in test names to alphanumerics,
  hyphens, underscores and spaces. (#161, @CraigFe)
- Fix a race condition on creating output directories. (#150, @edwintorok)
- Report test suite durations in real time rather than system time. (#162,
  @CraigFe)
- Improve documentation of the `?argv` parameter. (#164, @ian-barnes)
- Remove version number from test binary help pages. (#186, @CraigFe)
- Remove dependency on `tput`. (#189, @samoht)
- Remove result dependency. (#159, @hannesm)
- Remove uses of deprecated `Pervasives.compare`. (#173, @CraigFe)

### 0.8.5 (2018-12-11)
- Port build to Dune from jbuilder (#139 @samoht)
- Fix output path on Windows/Cygwin (#141 @kkirstein)
- Switch opam metadata to 2.0 format (#144 @samoht)
- Add nice screenshots to the README (#143 @rizo)
- Fix ocamldoc headings to work with odoc (#145 @avsm)
- Do not test on Debian-unstable, add Fedora (#145 @avsm)

### 0.8.4 (2018-10-17)

- Improve documentation for speed and tests (#129, @raphael-proust)
- Improve documentation on test case filtering and flush error formatter on exit
  (#133, @edwintorok)
- Create a fresh log sub-dir for every run (#125, @m-harrison)
- Fix wrong location hint for test files when using dune (#135, @m-harrison)

### 0.8.3 (2018-03-25)

- Show one failure when multiple tests fail (#117, @aantron)

### 0.8.2 (2017-08-21)

- add `Async` support: there is a new `alcotest-async` package containing
  an `Alcotest_async` module (#104, @rgrinberg)

### 0.8.1 (2017-08-03)

- Add `failf` (#105, @hcarty)
- Relax the `float` combinator to compare its epsilon using `<=` instead
  of `<`. This allows to use `float 0.` for "exact" float comparison
  (#107, @samoht, @talex5)
- Fix outdated displayed information when using `--verbose`.
  Be clearer that no new output logs are actually created and
  do not try to display outdated information (#108, @samoht)


### 0.8.0 (2017-06-22)

- Format "got" and "expected" values in the same way (#86, @talex5)
- Change the `float` combinator to take a mandatory 'epsilon' parameter
  (#89, @superbobry)
- Switch to jbuilder (#92, @rgrinberg)
- Add a `test_case` function (#94, @samoht)
- Add an `alcotest-lwt` package, containing an `Alcotet_lwt` module with
  an `Alcotest_lwt.test_case` function to better deal with lwt tests
  (#94, @talex5, @samoht)
- Add `Alcotest.neg` to negate test results (#95, @samoht)
- Change the `test_case` type from `unit -> unit` to `'a -> unit`. The `'a`
  parameter can be built using as a `Cmdliner` term using the new
  `run_with_args` function. This is useful to configure the tests using the CLI
  (#96, @samoht)

### 0.7.2 (2016-11-10)

- Clean up handling of env variables (#83, @samoht)

### 0.7.1 (2016-11-03)

- Store tests output to `_build/_tests` by default (#77, @pqwy)

### 0.7.0 (2016-10-25)

- Add a `unit` testable (useful for functions with side-effects) (#79, @avsm)
- Add a `testable` combinator to easily build `'a testable` values (#75, @pqwy)
- Add `pp` and `equal` to extract the pretty-printer and equality functions
  from an `'a testable` (#75, @pqwy)
- Add an `array` testable (#75, @pqwy)

### 0.6.0 (2016-06-28)

- Add int32,int64,float testables (#71, @hcarty)

### 0.5.0 (2016-06-27)

* Use `topkg` (#68, @samoht)
* Add `Alcotest.reject` to always fail tests (#64, @talex5)
* Fix pretty-printing of `Alcotest.list` (#53, #65, @talex5)
* Add an `argv` optional argument to `run` to use custom command-line arguments
  (#63, @dinosaure)
* Fix typo in JSON output (#67, @fxfactorial)
* Use `Astring` for the unit tests (#62, @hannesm)

### 0.4.11 (2016-05-11)

* Fix regression introduced in 0.4.8 about alignment of [ERROR] (#60, @samoht)

### 0.4.10 (2016-05-03)

* Fix support for 4.03 (#58, by @hannesm)

### 0.4.9 (2016-02-25)

* Add `Alcotest.pass` a testable combinator which always pass (#50, @seliopou)
* Fix `index out of bounds` for empty test doc string (#51, @dariusf)
* Display the log directory (@samoht)
* Add missing newline in display header (#53, #54, @samoht)
* Add a `--color` flag to tweak color usage on the command-line and use `Fmt`
  (#52, #55, @samoht)

### 0.4.8 (2016-03-12)

* Fix `check_raises` (#48, by @yallop)
* Use Astring (this drops support for 4.00) (@samoht)
* Simplify the build system (@samoht)

### 0.4.7 (2016-02-22)

* Minimal fix to ensure windows support (#46, by @samoht)

### 0.4.6 (2015-12-29)

* Add missing newline to verbose output (#36, by @seliopou)
* Result: add Result.result combinator (#37, by @seliopou)
* When redirecting stdout/stderr, use a single fd to share the seek offset
  (#39, by @dsheets)
* If redirecting output, print error results as well (#39, by @dsheets)

### 0.4.5 (2015-09.16)

* Add boolean assert: `Alcotest.bool` (#33, by @zjhmale)
* Add sorted list assert: `Alcotest.slist` (#34, by @samoht)
* Add pair assert: `Alcotest.pair` (#34, by @samoht)
* Add simple assert, built using `Pervasive.compare` and a pretty-printing
  function: `Alcotest.of_pp` (#34, by @samoht)

### 0.4.4 (2015-07-31)

* Fix of the format of log filenames
* Fix a regression in 0.4.* which were hiding error messages when using wrong
  command-line arguments

### 0.4.3 (2015-07-22)

* Flush formatter for `Alcotest.check` (#27, by @edwintorok)
* Handle UTF8 for test documentation strings (#5)

### 0.4.2 (2015-07-03)

* Improve the result outputs

### 0.4.1 (2015-07-03)

* Fix regression introduced in 0.4.0: display the error if there is only
  one error
* Add a testable combinator for options.

### 0.4.0 (2015-06-29)

* Simplify the use of the library by removing global states -- now calling
  the `run` function multiple times is much more consistent.
* Remove the direct dependency to `OUnit`. Programs using `OUnit` and `Alcotest`
  should continue to work.
* Add a `TESTABLE` signature and a `check` function to check invariants in
  the tested libraries.

### 0.3.3 (2015-06-19)

* Control `--show-errors` using the ALCOTEST_SHOW_ERRORS env variable (#9)
* Add an `and_exit` optional argument to `Alcotest.run` to control
  the exit behavior of the main test function (#4)
* Fix the output of `--version`
* Add a `--json` argument to show the test results as a JSON object
  (#14, by @eowzukw)
* Expose `Alcotest.result` to turn a test into a result

### 0.3.2: (2015-06-08)

* Do not fail if the output file does not exist
* Add a simple example (#10, by @leowzukw)
* Add a logo (#12, by @leowzukw)

### 0.3.1 (2015-04-18)

* Fix OCaml 4.01.0 and earlier support (regressed in 0.3.0).
* Add Travis CI tests.

### 0.3.0 (2015-04-13)

* Fix backtrace handling (#2 by @dsheets)
* Use `Bytes` module instead of `String`

### 0.2.0 (2012-12-19)

* Fix issues with redirections
* Display the full errors when only one test is selected

### 0.1.0 (2012-12-12)

* Initial release
