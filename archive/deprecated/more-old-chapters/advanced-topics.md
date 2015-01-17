Advanced Topics
================================================

_(jyh: This is a placeholder chapter for extra material.)_

## Hybrid recursion

In general, the choice of whether to use regular recursion vs. tail
recursion is not immediately obvious.  Regular recursion is often
better for small lists (and other data structures), but it is better
to use tail recursion for very large lists -- especially because stack
sizes limit the number of recursive calls.

Core takes a hybrid approach that can be illustrated with the
implementation of the function `Core_list.map`.

```ocaml
let map_slow l ~f = rev (rev_map l ~f);;

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] -> let f1 = f x1 in [f1]
  | [x1; x2] -> let f1 = f x1 in let f2 = f x2 in [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
	let f2 = f x2 in
	let f3 = f x3 in
	[f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
	let f2 = f x2 in
	let f3 = f x3 in
	let f4 = f x4 in
	[f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in 
	let f2 = f x2 in
	let f3 = f x3 in
	let f4 = f x4 in
	let f5 = f x5 in
	f1 :: f2 :: f3 :: f4 :: f5 ::
      (if ctr > 1000 then map_slow ~f tl else count_map ~f tl (ctr + 1));;

let map l ~f = count_map ~f l 0;;
```

For performance, there are separate patterns for small lists with up
to 4 elements, then a recursive case for lists with five or more
elements.  The `ctr` value limits the recursion -- regular recursion
is used for up to 1000 recursive calls (which includes lists with up
to 4000 elements), then the tail-recursive function `map_slow` is used
for any remainder.

As an aside, you might wonder why this implementation uses explicit
let-definitions for the result values `f1`, `f2`, etc.  The reason is
to force the order of evaluation, so that the the function `f` is
always applied to the list values left-to-right (starting with the
first element in the list).  In an expression like
`[f x1; f x2; f x3]` the order of evaluation is not specified by the
language, any of the subexpressions might be evaluated first (though
we would often expect evaluation order to be either left-to-right or
right-to-left).  For functions that perform I/O, or have other
side-effects, left-to-right evaluation order is important (and
required).

