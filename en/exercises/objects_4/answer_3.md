1.
  When \lstinline$compare$ is implemented as a method, the equality
  function \lstinline$equal$ must also be a method.  
  
```ocaml
  class virtual ['key, 'value] map =
    object (self : 'self)
      val elements : ('key * 'value)
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (self#equal key) elements)
  
      method private equal key1 (key2, _) = compare key1 key2 = Equal
      method private virtual compare : 'key -> 'key -> ordering
    end;;
  
  class ['value] int_map =
    object (self : 'self)
      inherit [int, 'value] map
  
      method private compare i j =
        if i < j then Smaller
        else if i > j then Larger
        else Equal
    end
```

