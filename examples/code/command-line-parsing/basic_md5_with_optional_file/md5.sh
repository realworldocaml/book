  $ jbuilder build md5.exe
  Done: 3/7 (jobs: 1)                   Done: 4/7 (jobs: 1)                   Done: 5/7 (jobs: 1)                   Done: 74/77 (jobs: 1)                     Done: 75/77 (jobs: 1)                     Done: 76/77 (jobs: 1)                     
%% --non-deterministic
  $ cat /etc/services | ./_build/default/md5.exe
  27bf1f2dbadd4cae84f1da4dfe8b5cb3
