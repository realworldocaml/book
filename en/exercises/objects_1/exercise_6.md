  
## Exercise
  The narrowing technique on page~\pageref{page:narrowing-with-exceptions} skirts an important
  problem---what if the inheritance hierarchy has multiple levels?  For example, we might have the
  following relationships.
  
```ocaml
  hound $\subtype$ dog $\subtype$ animal
  tabby $\subtype$ cat $\subtype$ animal
```
  In a \misspelled{na\"\i{}ve} implementation, typecases would have to be updated whenever a new tag is added.  For example, the
  `chorus` function might require at least four cases.
  
```ocaml
  let chorus (animals : animal list) =
     List.iter (fun animal ->
        match animal#actual with
           Dog dog -> dog#bark
         | Hound hound -> hound#bark
         | Cat cat -> cat#meow
         | Tabby tabby -> tabby#meow
         | _ -> ()) animals
```
  This is undesirable of course, since the `chorus` function cares only about the general
  cases `dog` and `cat`.
  
  Modify the implementation so that the method `actual` takes a list of acceptable tags as
  an argument.  For example, for a hound `hound`, the expression
  `hound#actual [CatTag; DogTag]` would evaluate to `Dog hound`;
  but `hound#actual [HoundTag; DogTag; CatTag]` would evaluate to `Hound hound`.
  
