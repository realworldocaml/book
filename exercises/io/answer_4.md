1.
  A sensible approach is to read the characters, swap them, and write the results back.
  If an error occurs before the values are written, the operation can simply be aborted.
  
  However, once the first file is modified, the second file must be modified as well.  In the
  following code, if an error occurs while writing the second file (line 7), the error is caught and
  an attempt is made to write character `c1` back to the first file.  An error on line 10 is
  fatal.  In general is it not possible to make the `exchange` operation atomic.
  
```ocaml
  let exchange file1 file2 =
     let c1 = with_in_file file1 input_char in
     let c2 = with_in_file file2 input_char in
     with_out_file file1 (fun chan -> output_char chan c2);
     (* Errors after this line must be handled *)
     try
        with_out_file file2 (fun chan -> output_char chan c1)
     with exn ->
        (* Try to write back c1 to file1 *)
        with_out_file file1 (fun chan -> output_char chan c1);
        raise exn
```

