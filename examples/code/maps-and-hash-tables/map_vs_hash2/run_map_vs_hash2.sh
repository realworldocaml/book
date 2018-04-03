  $ jbuilder build map_vs_hash2.exe
  Done: 3/5 (jobs: 1)                   Done: 74/77 (jobs: 1)                     Done: 75/77 (jobs: 1)                     Done: 76/77 (jobs: 1)                     
%% --non-deterministic [skip]
  $ ./_build/default/map_vs_hash2.exe -ascii -clear-columns time speedup
  Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
                                  
    Name      Time/Run   Speedup  
   ------- ------------ --------- 
    table   4_453.95us     25.80  
    map       172.61us      1.00  
                                  
