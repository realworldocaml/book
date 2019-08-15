let string =
  view (char ptr)
    ~read:string_of_char_ptr
    ~write:char_ptr_of_string
