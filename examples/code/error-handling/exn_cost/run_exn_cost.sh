  $ jbuilder build exn_cost.exe
%% --non-deterministic [skip]
  $ ./_build/default/exn_cost.exe -ascii cycles -quota 1
  Estimated testing time 4s (4 benchmarks x 1s). Change using -quota SECS.

    Name                           Time/Run   Cycls/Run   mWd/Run   Percentage
   ------------------------------ ---------- ----------- --------- ------------
    simple computation             177.25ns     353.04c    84.00w       90.04%
    simple computation w/handler   171.60ns     341.81c    84.00w       87.17%
    end with exn                   196.85ns     392.11c    84.00w      100.00%
    end with exn notrace           169.20ns     337.00c    84.00w       85.95%
