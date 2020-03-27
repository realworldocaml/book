// Example inspired by Dario Teixeira's question.
%token A B
%start<unit> main
%nonassoc block_is_finished
%nonassoc A
%%
main:  block* B   {}
block: items      {}
items: item       {} %prec block_is_finished
     | item items {}
item:  A          {}
