  $ dune build bench_poly_and_mono.exe
%% --non-deterministic [skip]
  $ ./_build/default/bench_poly_and_mono.exe -ascii -quota 1
  Estimated testing time 2s (2 benchmarks x 1s). Change using -quota SECS.
                                                       
    Name                        Time/Run   Percentage  
   ------------------------ ------------- ------------ 
    Polymorphic comparison   18_402.43ns      100.00%  
    Monomorphic comparison      734.22ns        3.99%  
                                                       
