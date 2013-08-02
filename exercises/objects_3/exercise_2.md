  
## Exercise
  Consider the following class, which implements a persistent
  reference-counted value stored in a file.  When there are no more
  references, the file is removed.
  
```ocaml
  class persistent_refcounted_value filename =
  object (self)
      (* persistent_value *)
      val mutable x : int list =
         let fin = open_in_bin filename in
         let x = input_value fin in
         close_in fin;
         x
      method get = x
      method set y = x <- y; self#save
      method private save =
         let fout = open_out_bin filename in
         output_value fout x;
         close_out fout
  
      (* refcounted_value *)
      val mutable ref_count = 1
      method add_ref = ref_count <- ref_count + 1
      method rm_ref =
         ref_count <- ref_count - 1;
         if ref_count = 0 then
            Sys.remove filename
  end
```
1. Partition the class into three classes: `persistent_value` implements
  persistent values stored in files, `refcounted_value` implements generic reference
  counted objects, and `persistent_refcounted_value` inherits from both.
  
1. What is the advantage in partitioning the class?
  
