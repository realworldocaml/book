1.
1.
  The complete solution is mainly unchanged from the code that uses exceptions.
  
```ocaml
  type 'a animal = < actual : 'a; eat : unit >
  type 'a dog = < actual : 'a; eat : unit; bark : unit >
  type 'a cat = < actual : 'a; eat : unit; meow : unit >
  
  type 'a tag = [> `Dog of 'a tag dog | `Cat of 'a tag cat ] as 'a
  
  let fido : 'a tag dog =
  object (self)
     method actual = `Dog self
     method eat = ()
     method bark = ()
  end;;
  
  let daphne : 'a tag cat =
  object (self)
     method actual = `Cat self
     method eat = ()
     method meow = ()
  end;;
  
  let animals = [(fido :> 'a tag animal); (daphne :> 'a tag animal)];;
  
  let chorus (animals : 'a tag animal list) =
     List.iter (fun animal ->
        match animal#actual with
           `Dog dog -> dog#bark
         | `Cat cat -> cat#meow
         | _ -> ()) animals
```
  
1. The type variable `'a` stands for the real type of tags.
  The tag type contains at least the tags ``Dog` and ``Cat`, but it is an open
  type, so the actual type `'a` may contain additional constructors.
  
1. Since lizards don't vocalize, we really don't need to change any of the existing
  code.  We simple add the new object with a new tag.
  
```ocaml
  let fred : 'a tag lizard =
  object (self)
     method actual = `Lizard self
     method eat = ()
  end;;
```

