  $ dune build bench_patterns.exe
%% --non-deterministic [skip]
  $ ./_build/default/bench_patterns.exe -ascii -quota 0.25
  Estimated testing time 750ms (3 benchmarks x 250ms). Change using -quota SECS.
                                                        
    Name                         Time/Run   Percentage  
   ---------------------------- ---------- ------------ 
    Polymorphic pattern           24.93ns       89.45%  
    Monomorphic larger pattern    27.88ns      100.00%  
    Monomorphic small pattern     10.84ns       38.90%  
                                                        
