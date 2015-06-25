$ corebuild -pkg core_bench barrier_bench.native
$ ./barrier_bench.native -ascii name alloc
Estimated testing time 20s (change using -quota SECS).
                                                                   
  Name        Time (ns)       Minor   Major   Promoted   % of max  
 ----------- ----------- ----------- ------- ---------- ---------- 
  mutable     6_304_612   2_000_004    9.05       9.05     100.00  
  immutable   4_775_718   5_000_005    0.03       0.03      75.75  
                                                                   
