  $ dune build cal.exe
  Done: 3/7 (jobs: 1)                   Done: 4/7 (jobs: 1)                   Done: 5/7 (jobs: 1)                   Done: 70/73 (jobs: 1)                     Done: 71/73 (jobs: 1)                     Done: 72/73 (jobs: 1)                     
  $ ./_build/default/cal.exe -help
  Add [days] to the [base] date and print day
  
    cal.exe BASE DAYS
  
  === flags ===
  
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  $ ./_build/default/cal.exe 2012-12-25 40
  2013-02-03
