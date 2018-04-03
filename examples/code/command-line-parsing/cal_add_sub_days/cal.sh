### build
  $ jbuilder build cal.exe
  Done: 3/7 (jobs: 1)                   Done: 4/7 (jobs: 1)                   Done: 5/7 (jobs: 1)                   Done: 70/73 (jobs: 1)                     Done: 71/73 (jobs: 1)                     Done: 72/73 (jobs: 1)                     
  $ ./_build/default/cal.exe -help
  Manipulate dates
  
    cal.exe SUBCOMMAND
  
  === subcommands ===
  
    add      Add [days] to the [base] date
    diff     Show days between [date1] and [date2]
    version  print version information
    help     explain a given subcommand (perhaps recursively)
  
### run
  $ ./_build/default/cal.exe add 2012-12-25 40
  2013-02-03
  $ ./_build/default/cal.exe diff 2012-12-25 2012-11-01
  54 days
