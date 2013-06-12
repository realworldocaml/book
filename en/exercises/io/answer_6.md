1.
  The lines that begin with the digit `1` can be
  printed immediately, so we need only buffer the lines beginning with
  the digit `2`.
  
```ocaml
  let deinterleave () =
     let inc = open_in "data.txt" in
     let buf = Buffer.create 256 in
     try
        while true do
           let line = input_line inc in
           let len = String.length line - 1 in
           if line.[0] = '1' then begin
              output stdout line 1 len;
              output_char stdout '\n'
           end
           else begin
              Buffer.add_substring buf line 1 len;
              Buffer.add_char buf '\n'
           end
     with End_of_file ->
        output_string stdout (Buffer.contents buf)
```
  This code is a bit sloppy.  It doesn't deal gracefully with blank
  lines, and it assumes that any line not beginning with the
  digit `1` must begin with a `2`.

