(* This partial grammar specification defines the syntax of
   expressions in reverse Polish notation. Parentheses are
   meaningless, and unary minus is not supported (some other symbol
   than MINUS would be required in order to avoid an ambiguity). *)

%%

%public expr:
| i = INT
    { i }
| e1 = expr e2 = expr PLUS
    { e1 + e2 }
| e1 = expr e2 = expr MINUS
    { e1 - e2 }
| e1 = expr e2 = expr TIMES
    { e1 * e2 }
| e1 = expr e2 = expr DIV
    { e1 / e2 }

