1.
1.
  The class is simply split down the middle.  The details of deletion must be implemented by
  the value, not the reference counting class, so the virtual method `delete` is
  used to connect the two objects.
  
```ocaml
  class persistent_value filename =
  object (self)
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
      method private delete = Sys.remove filename
  end
  
  class virtual ref_value =
  object (self)
      val mutable ref_count = 1
      method add_ref = ref_count <- ref_count + 1
      method rm_ref =
         ref_count <- ref_count - 1;
         if ref_count = 0 then self#delete
      method private virtual delete : unit
  end
  
  class persistent_ref_value2 filename =
  object
     inherit persistent_value filename
     inherit ref_value
  end
```
  
1. The advantage of splitting the class is that we now have two more generic classes.
  For example, reference counting is general concept that can be re-used elsewhere in the program.

