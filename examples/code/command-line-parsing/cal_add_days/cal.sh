  $ jbuilder build cal.exe
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
