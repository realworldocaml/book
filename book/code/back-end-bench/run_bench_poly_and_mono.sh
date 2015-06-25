$ corebuild -pkg core_bench bench_poly_and_mono.native
$ ./bench_poly_and_mono.native -ascii
Estimated testing time 20s (change using -quota SECS).
                                                 
  Name                     Time (ns)   % of max  
 ------------------------ ----------- ---------- 
  Polymorphic comparison      13_919     100.00  
  Monomorphic comparison         815       5.86  
                                                 
