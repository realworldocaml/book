  $ jbuilder build barrier_bench.exe
  $ ./_build/default/barrier_bench.exe -ascii alloc -quota 1
  Estimated testing time 2s (2 benchmarks x 1s). Change using -quota SECS.
                                                                       
    Name        Time/Run   mWd/Run   mjWd/Run   Prom/Run   Percentage  
   ----------- ---------- --------- ---------- ---------- ------------ 
    mutable       8.98ms    2.00Mw     20.36w     20.36w      100.00%  
    immutable     5.66ms    5.00Mw                             63.08%  
                                                                       
