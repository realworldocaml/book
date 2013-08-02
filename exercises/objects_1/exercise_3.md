  
## Exercise
  Suppose we wish to enforce the fact that a program contains only one copy of an object.  For
  example, the object may be an accounting object, and we wish to make sure the object is never copied
  or forged.
  
  The standard library module `Oo` contains a function that copies any object.
  
```ocaml
     val copy : (< .. > as 'a) -> 'a
```
  Modify the following object so that it refuses to work after being copied.
  
```ocaml
  let my_account =
  object
     val mutable balance = 100
     method withdraw =
        if balance = 0 then
           raise (Failure "account is empty");
        balance <- balance - 1
  end
```
  
