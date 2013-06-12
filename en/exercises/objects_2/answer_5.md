1.
  The queue can be defined by keeping track of the last element in the list of items,
  so that the method `add` adds the new element at the end of the list,
  instead of at the beginning.
  
```ocaml
  class int_queue =
  let nil = new nil in
  object
     inherit int_stack
  
     val mutable last = nil
  
     method add i =
        let new_last = new cons i nil in
        if last#is_nil then
           items <- new_last
        else
           last#set_tl new_last;
        last <- new_last
  end
```

1.
  The two data structures have the same type but they are semantically different.
  A queue refines the stack implementation, but it does not behave like a stack.
  We would not normally say that a queue is-a stack.

