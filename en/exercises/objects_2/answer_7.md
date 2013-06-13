1.
  For the methods `connect_input_a` we need to construct a terminal object
  that performs the appropriate action.
```ocaml
     method connect_input_a (wire : wire) =
         wire#add_terminal (object method set x = self#set_input_a x end)
```

1.
```ocaml
  class three_input_gate =
  object
     inherit two_input_gate
     val mutable c = false
     method private set_input_c x = c <- x; self#set_output
     method connect_input_c (wire : wire) =
         wire#add_terminal (object method set x = self#set_input_c x end)
```

1.
  It would be slightly simpler because the input terminal could be set without the intermediate terminal object.
  The connect methods would have the following form.
  
```ocaml
     method connect_input_a (wire : wire) =
        wire#add_terminal self#set_input_a
```

1.
  The conditional prevents activity from propagating if it does not change the circuit values.
  It also means that the simulation will terminate, even for cyclic circuits, if the circuit becomes quiescent.

1.
```ocaml
  let gate1 = new nor2;;
  let gate2 = new nor2;;
  let wire1 = new wire;;
  let wire2 = new wire;;
  gate1#connect_output wire1;;
  gate2#connect_output wire2;;
  gate1#connect_input_b wire2;;
  gate2#connect_input_a wire1;;
```

