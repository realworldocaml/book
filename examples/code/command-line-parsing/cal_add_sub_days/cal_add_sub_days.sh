### build
  $ jbuilder build cal_add_sub_days.exe
  $ ./_build/default/cal_add_sub_days.exe -help
  Manipulate dates
  
    cal_add_sub_days.exe SUBCOMMAND
  
  === subcommands ===
  
    add      Add [days] to the [base] date
    diff     Show days between [date1] and [date2]
    version  print version information
    help     explain a given subcommand (perhaps recursively)
  
### run
  $ ./_build/default/cal_add_sub_days.exe add 2012-12-25 40
  2013-02-03
  $ ./_build/default/cal_add_sub_days.exe diff 2012-12-25 2012-11-01
  54 days
