1.
  If the function raises an exception, the exception can be caught with a wildcard exception handler,
  the channel closed, and the exception re-raised.
```ocaml
  let with_in_file filename f =
     let in_chan = open_in filename in
     try
        let x = f in_chan in
        close_in in_chan;
        x
     with exn ->
        close_in in_chan;
        raise exn
```

