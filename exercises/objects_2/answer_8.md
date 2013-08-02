1.
  The scheduling queue can be implemented using the `Queue` standard library.
  This will use a FIFO policy, which is fair.
  
  The scheduler has two methods.  The method `main` runs the simulation until the
  queue is empty.  The method `connect_input` takes a wire and a function, creates
  a terminal object, and add the object to the wire.  When the terminal is set, it gets
  added to the scheduling queue.  Note that the field `event_queue` is accessible
  in the inner terminal object.
  
```ocaml
  type terminal = < set : bool -> unit; execute : unit >
  
  class scheduler =
  object (self : 'self)
     val event_queue : terminal Queue.t = Queue.create ()
  
     method main =
         while not (Queue.is_empty event_queue) do
            (Queue.take event_queue)#execute
         done
     method connect_input (wire : wire) (f : bool -> unit) =
        let term =
           object (term_self)
              val mutable value = false
              method set x =
                 value <- x;
                 Queue.add term_self event_queue
              method execute = f value
           end
        in
        wire#add_terminal term
  end;;
  
  let the_scheduler = new scheduler;;
```
  The only other change to the simulation is to the methods `connect_input_?`.
  
```ocaml
  class virtual two_input_gate =
  object (self : 'self)
     inherit gate
     val mutable a = false
     val mutable b = false
     method private set_input_a x = a <- x; self#set_output
     method private set_input_b x = b <- x; self#set_output
     method connect_input_a (wire : wire) =
        the_scheduler#connect_input wire self#set_input_a
     method connect_input_b (wire : wire) =
        the_scheduler#connect_input wire self#set_input_b
  end
```

