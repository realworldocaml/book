let string = 
  view 
    ~read:string_of_char_ptr 
    ~write:char_ptr_of_string 
    (char ptr)
