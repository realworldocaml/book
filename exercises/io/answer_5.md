1.
  Here is one version.
```ocaml
  let rec print_exp = function
     Int i -> print_int i
   | String s -> printf "\"%s\"" s
   | List el -> print_char '('; print_exp_list el; print_char ')'
  
  and print_exp_list = function
     [] -> ()
   | [e] -> print_exp e
   | e :: el ->
       print_exp e;
       print_char ' ';
       print_exp_list el
```
  The `printf`-style functions provide a slightly more concise implementation.
  
```ocaml
  let rec print_exp out_chan = function
     Int i -> output_int out_chan i
   | String s -> fprintf out "\"%s\"" s
   | List el -> fprintf out "(%a)" print_exp_list el
  
  and print_exp_list out_chan = function
     [] -> ()
   | [e] -> print_exp out_chan e
   | e :: el -> fprintf out_chan "%a %a" print_exp e print_exp_list el
```

