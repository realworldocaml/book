1.
  The queue could simply be represented as a list, but then one of the operations `add` or
  `take` would take time $O(m)$ where $m$ is the length of the queue.
  
  The two list `($\ms{front$, $\ms{back}$)`} representation is a little better.  To make the
  data structure persistent, the functions `add` and `take` produce new queues.
  
```ocaml
  type 'a queue = ('a list * 'a list) ref
  
  let empty = ref ([], [])
  
  let add queue x =
     let (front, back) = !queue in
     ref (x :: front, back)
  
  let rec take queue =
     match !queue with
        [], [] -> raise (Invalid_argument "queue")
      | front, x :: back ->
          x, ref (front, back)
      | front, [] ->
          queue := ([], List.rev front);
          take queue
```
  The side effect on line 15 does not affect persistence because the queue membership is preserved.
  However, efficiency is still not optimal.  Consider a sequence of $m$ `add`s, followed by $m$ `take`s.
  
```ocaml
  let $q_1$ = add empty 1
  let $q_2$ = add $q_1$ 2
  ...
  let $q_m$ = add $q_{m - 1}$ $m$
  let $x_m$, _ = take $q_m$
  let $x_{m - 1}$, _ = take $q_{m - 1}$
  ...
  let $x_1$, _ = take $q_1$
```
  Each operation `take $q_i$` shifts $i$ elements of the queue, so the total time is $O(m^2)$.
  
  Okasaki gives an efficient implementation, summarized below in OCaml.  A function
  `maybe_shift` is used to shift the queue whenever the front becomes longer than the back.
  The list lengths are needed, so the queue is a 4-tuple
  `($\ms{front$, $|\ms{front}|$, $\ms{back}$, $|\ms{back}|$)`}.
  
```ocaml
  type 'a queue = 'a lazy_list * int * 'a lazy_list * int
  
  let empty = (Nil, 0, Nil, 0)
  
  let insert (front, flen, back, blen) x =
     maybe_shift (cons x front) (flen + 1) back blen
  
  let remove (front, flen, back, blen) =
     head back, maybe_shift front flen (tail back) (blen - 1)
  
  let maybe_shift front flen back blen =
     if flen <= blen then
        (front, flen, back, blen)
     else
        (Nil, 0, shift front back [], flen + blen)
  
  let shift front back l =
     if is_nil back then
        cons (head front) l
     else
        lazy_cons (head back) (fun () ->
           shift (tail front) (tail back) (cons (head front) l))
```

