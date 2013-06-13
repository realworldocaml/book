  
## Exercise
  For each of the following instances of types $t_1$ and $t_2$,
  determine whether $t_1$ is a subtype of $t_2$---that is, whether $t_1 \subtype t_2$.
  Assume the following class declarations and relations.
  
  \begin{center}
  \begin{tabular}{|l|}
  \hline
  Subtyping relations\\
  \hline
  `dog $\subtype$ animal`\\
  `cat $\subtype$ animal`\\
  \hline
  \end{tabular}
  \end{center}
  
1.
  
```ocaml
  type $t_1$ = animal -> cat
  type $t_2$ = dog -> animal
```
  
1.
  
```ocaml
  type $t_1$ = animal ref
  type $t_2$ = cat ref
```
  
1.
  
```ocaml
  type 'a cl = < f : 'a -> 'a >
  type $t_1$ = dog cl
  type $t_2$ = animal cl
```
  
1.
  
```ocaml
  type 'a cl = < x : 'a ref >
  type $t_1$ = dog cl
  type $t_2$ = animal cl
```
  
1.
  
```ocaml
  type 'a c1 = < f : 'a -> unit; g : unit -> 'a >
  type 'a c2 = < f : 'a -> unit >
  type $t_1$ = animal c1
  type $t_2$ = cat c2
```
  
1.
  
```ocaml
  type $t_1$ = ((animal -> animal) -> animal) -> animal
  type $t_2$ = ((cat -> animal) -> dog) -> animal
```
  
  
