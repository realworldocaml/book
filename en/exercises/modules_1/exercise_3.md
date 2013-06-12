  
## Exercise
  \index{interfaces!automatically generating}
  When a program is begin developed, it is sometimes convenient to have the
  compiler produce a `.mli` file automatically, using
  the `-i` option to `ocamlc`.  For
  example, suppose we have an implementation
  file `set.ml` containing the following definitions.
  
```ocaml
  type 'a set = 'a list
  let empty = []
  let add x s = x :: s
  let mem x s = List.mem x s
```
  Inferring types, we obtain the following output.  The output can then
  be edited to produce the desired `set.mli` file.
```ocaml
  type 'a set = 'a list
  val empty : 'a list
  val add : 'a -> 'a list -> 'a list
  val mem : 'a -> 'a list -> bool
```
  
1.
  
  The output produced by `ocamlc -i` is not abstract---the
  declarations use the type
  `'a list`,
  not `'a set`.  Instead of editing all the occurrences by hand, is
  there a way to get `ocamlc -i` to produce the right output
  automatically?
  
1.
  
  In some cases, `ocamlc -i` produces illegal output.
  What is the inferred interface for the following program?  What is
  wrong with it?  Can it be fixed?
  
```ocaml
  let cell = ref []
  let push i = cell := i :: !cell
  let pop () =
     match !cell with
        [] -> raise (Invalid_argument "pop")
      | i :: t ->
          cell := t;
          i
```
  
