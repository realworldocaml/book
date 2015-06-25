$ corebuild -pkg core_bench map_vs_hash2.native
$ ./map_vs_hash2.native -ascii -clear-columns name time speedup
Estimated testing time 20s (change using -quota SECS).
                               
  Name    Time (ns)   Speedup  
 ------- ----------- --------- 
  map       218_581     12.03  
  table   2_628_423      1.00  
                               
