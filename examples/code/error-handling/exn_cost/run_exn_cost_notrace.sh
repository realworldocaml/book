%% --non-deterministic
  $ OCAMLRUNPARAM= ./_build/default/exn_cost.exe -ascii cycles -quota 1
  Estimated testing time 4s (4 benchmarks x 1s). Change using -quota SECS.
  |
  |    Name                           Time/Run   Cycls/Run   mWd/Run   Percentage
  |   ------------------------------ ---------- ----------- --------- ------------
  |    simple computation             428.29ns      1.33kc    84.00w       92.36%
  |    simple computation w/handler   463.72ns      1.44kc    84.00w      100.00%
  |    end with exn                   428.89ns      1.33kc    84.00w       92.49%
  |    end with exn notrace           413.29ns      1.28kc    84.00w       89.13%
