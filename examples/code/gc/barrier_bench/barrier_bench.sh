### run
  $ jbuilder build barrier_bench.exe
%% --non-deterministic [skip]
  $ ./_build/default/barrier_bench.exe -ascii alloc -quota 1
  Estimated testing time 2s (2 benchmarks x 1s). Change using -quota SECS.

    Name        Time/Run   mWd/Run   mjWd/Run   Prom/Run   Percentage
   ----------- ---------- --------- ---------- ---------- ------------
    mutable       8.98ms    2.00Mw     20.36w     20.36w      100.00%
    immutable     5.66ms    5.00Mw                             63.08%

### help
  $ ./_build/default/barrier_bench.exe -help
  Benchmark for mutable, immutable
  
    barrier_bench.exe [COLUMN ...]
  
  Columns that can be specified are:
  	time       - Number of nano secs taken.
  	cycles     - Number of CPU cycles (RDTSC) taken.
  	alloc      - Allocation of major, minor and promoted words.
  	gc         - Show major and minor collections per 1000 runs.
  	percentage - Relative execution time as a percentage.
  	speedup    - Relative execution cost as a speedup.
  	samples    - Number of samples collected for profiling.
  
  Columns with no significant values will not be displayed. The
  following columns will be displayed by default:
  	time alloc percentage
  
  Error Estimates
  ===============
  To display error estimates, prefix the column name (or
  regression) with a '+'. Example +time.
  
  (1) R^2 is the fraction of the variance of the responder (such as
  runtime) that is accounted for by the predictors (such as number of
  runs).  More informally, it describes how good a fit we're getting,
  with R^2 = 1 indicating a perfect fit and R^2 = 0 indicating a
  horrible fit. Also see:
  http://en.wikipedia.org/wiki/Coefficient_of_determination
  
  (2) Bootstrapping is used to compute 95% confidence intervals
  for each estimate.
  
  Because we expect runtime to be very highly correlated with number of
  runs, values very close to 1 are typical; an R^2 value for 'time' that
  is less than 0.99 should cause some suspicion, and a value less than
  0.9 probably indicates either a shortage of data or that the data is
  erroneous or peculiar in some way.
  
  Specifying additional regressions
  =================================
  The builtin in columns encode common analysis that apply to most
  functions. Bench allows the user to specify custom analysis to help
  understand relationships specific to a particular function using the
  flag "-regression" . It is worth noting that this feature requires
  some understanding of both linear regression and how various quatities
  relate to each other in the OCaml runtime.  To specify a regression
  one must specify the responder variable and a command separated list
  of predictor variables.
  
  For example: +Time:Run,mjGC,Comp
  
  which asks bench to estimate execution time using three predictors
  namely the number of runs, major GCs and compaction stats and display
  error estimates. Drop the prefix '+' to suppress error estimation. The
  variables available for regression include:
  	Time  - Time
  	Cycls - Cycles
  	Run   - Runs per sampled batch
  	mGC   - Minor Collections
  	mjGC  - Major Collections
  	Comp  - Compactions
  	mWd   - Minor Words
  	mjWd  - Major Words
  	Prom  - Promoted Words
  	One   - Constant predictor for estimating measurement overhead
  
  
  === flags ===
  
    [-all-values]         Show all column values, including very small ones.
    [-ascii]              Display data in simple ascii based tables.
    [-ci-absolute]        Display 95% confidence interval in absolute numbers
    [-clear-columns]      Don't display default columns. Only show user specified
                          ones.
    [-display STYLE]      Table style (short, tall, line, blank or column).
                          Default short.
    [-fork]               Fork and run each benchmark in separate child-process
    [-geometric SCALE]    Use geometric sampling. (default 1.01)
    [-linear INCREMENT]   Use linear sampling to explore number of runs, example
                          1.
    [-load FILE]          Analyze previously saved data files and don't run tests.
                          [-load] can be specified multiple times.
    [-no-compactions]     Disable GC compactions.
    [-overheads]          Show measurement overheads, when applicable.
    [-quota SECS]         Time quota allowed per test (default 10s).
    [-reduced-bootstrap]  Reduce the number of bootstrapping iterations
    [-regression REGR]    Specify additional regressions (See -? help).
    [-save]               Save benchmark data to <test name>.txt files.
    [-sexp]               Output as sexp.
    [-stabilize-gc]       Stabilize GC between each sample capture.
    [-v]                  High verbosity level.
    [-width WIDTH]        width limit on column display (default 200).
    [-build-info]         print info about this build and exit
    [-version]            print the version of this build and exit
    [-help]               print this help text and exit
                          (alias: -?)
  
