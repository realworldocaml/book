1.
  Each of the constructs in the pseudo-code can be translated directly to OCaml.  However, it is
  slightly more efficient to avoid the use of reference cells, and translate the while-loop as a
  recursive function.
  
```ocaml
  let insert a i =
     let x = a.(i) in
     let rec loop j =
        if j >= 0 && a.(j) > x then begin
           a.(j) <- a.(j - 1);
           loop (j - 1)
        end
        else
           j
     in
     let j = loop (i - 1) in
     a.(j) <- x
  
  and insertion_sort a =
     for i = 1 to Array.length a - 1 do
        insert a i
     done
```

