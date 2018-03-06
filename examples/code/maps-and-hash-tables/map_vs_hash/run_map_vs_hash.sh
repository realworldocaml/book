  $ jbuilder build map_vs_hash.exe
  $ ./_build/default/map_vs_hash.exe -ascii -quota 1 -clear-columns time speedup
  Estimated testing time 2s (2 benchmarks x 1s). Change using -quota SECS.
                                
    Name    Time/Run   Speedup  
   ------- ---------- --------- 
    table    13.34ms      1.00  
    map      44.54ms      3.34  
                                
