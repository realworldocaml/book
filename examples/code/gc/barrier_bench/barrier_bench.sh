### run
  $ jbuilder build barrier_bench.exe
% --non-deterministic [skip]
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
  
  ...
  
