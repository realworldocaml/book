1.
1. The program returns the string `"A"`.
1. The program returns the empty string.
1. The program returns the string `"B"`.
1. The program raises the `Scan_failure` exception, removing
  the `C` from the input channel.
1.
  
  The program returns the string `"C"`.  The
  first `scanf` fails, but removes
  the `A` from the input stream.  The
  second `scanf` matches the `B`, and
  returns the string `"C"`.

