1.
  To keep it simple, we'll use exceptions both as tags and actual values.
  
```ocaml
  type actual = exn list -> exn
  type animal = < actual : actual; eat : unit >
  type dog = < actual : actual; eat : unit; bark : unit >
  type cat = < actual : actual; eat : unit; meow : unit >
  type hound = < actual : actual; eat : unit; bark : unit; howl : unit >
  
  exception DogTag
  exception CatTag
  exception HoundTag
  
  exception Dog of dog
  exception Cat of cat
  exception Hound of hound
  
  let fido : hound =
  object (self)
     method actual tags =
        match tags with
           HoundTag :: _ -> Hound self
         | DogTag :: _ -> Dog (self :> dog)
         | _ :: tags -> self#actual tags
         | [] -> Not_found
     method eat = ()
     method bark = ()
     method howl = ()
  end;;
  
  let chorus (animals : animal list) =
     List.iter (fun animal ->
        match animal#actual [DogTag; CatTag] with
           Dog dog -> dog#bark
         | Cat cat -> cat#meow
         | _ -> ()) animals
```

