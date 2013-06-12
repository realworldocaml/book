  
## Exercise
  A type of unary (base-1) natural numbers can be defined as follows,
  
```ocaml
  type unary_number = Z | S of unary_number
```
  where `Z` represents the number zero, and if $i$ is a unary number, then `S $i$`
  is $i + 1$.  For example, the number 5 would be represented as `S (S (S (S (S Z))))`.
  
1. Write a function to add two unary numbers.  What is the complexity of your function?
  
  
1. Write a function to multiply two unary numbers.
  
