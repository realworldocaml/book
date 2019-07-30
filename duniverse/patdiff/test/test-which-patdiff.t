  $ start_test

Test that we are testing the patdiff in the tree, not /bin/patdiff.

  $ type patdiff
  patdiff is a function
  patdiff () 
  { 
      patdiff.exe "$@"
  }
  $ which patdiff.exe | grep -q /usr/local/home
