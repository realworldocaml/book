(* This file is the interface (aka exposed API, or signature) for the [Hello]
   module. It only serves as documenting the module and selecting the functions
   to expose. The actual implementation of the functions are located in the
   module implementation [hello.ml]. *)

(** This is a docstring, as it starts with "(**", as opposed to normal comments
    that start with "(*".

    The top-most docstring of the module should contain a description of the
    module, what it does, how to use it, etc.

    The function-specific documentation located below the function signatures. *)

val greet : string -> string
(** This is the docstring for the [greet] function.

    You can read more about the ocamldoc syntax used to write docstrings here:

    https://ocamlverse.github.io/content/documentation_guidelines.html

    A typical documentation for this function would be:

    Returns a greeting message.

    {4 Examples}

    {[ print_endline @@ greet "Jane" ]} *)
