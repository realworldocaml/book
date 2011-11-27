# Modules

This chapter will focus on OCaml's module system, which we've already
seen in action in small ways in the preceding chapters.  For example,
the `List` module has shown up repeatedly as a place to find useful
functions like `List.map`.  Modules are more than just places to file
away your code; they also serve as abstraction boundaries that divide
a program into conceptual units.  

We discuss how modules work as abstraction
boundaries, let's describe how OCaml works with files as compilation
units.

## Files as compilation units

The simplest way of organizing an OCaml program is to have a program
live within a single file.  We'll consider the example a one-file
program that removes duplicate lines from an input file. That is, the
program should read its input a line at a time, printing the line only
if it hasn't seen it before.

In this implementation, we'll use a list to keep track of which lines
have been read. The program can then be as a single recursive function
that reads a line of input, compares it with lines that have been
previously read, and outputs the line if it has not been read. The
entire program is implemented in the single file `unique.ml`, whose
contents are shown below:

~~~~~~~~~~~~~~~~~~~~~~
open Core.Std

let rec unique already_read =
   output_string stdout "> ";
   flush stdout;
   let line = input_line stdin in
   if not (List.mem line already_read) then begin
      output_string stdout line;
      output_char stdout '\n';
      unique (line :: already_read)
   end else
     unique already_read

(* "Main program" *)
let () =
  try unique [] with End_of_file -> ()
~~~~~~~~~~~~~~~~~~~~~~

In this case, we can compile the entire program in a single step with
the command

~~~~~~~~~~~~~~
% ocamlc -o unique unique.ml
~~~~~~~~~~~~~~

where `ocamlc` is the OCaml compiler, `unique.ml` is the program file,
and the `-o` option is used to specify `unique` as the name of the
generated executable.

We can now run the program as follows:

~~~~~~~~~~~~~~
% ./unique
> Great Expectations
Great Expectations
> Vanity Fair
Vanity Fair
> Great Expectations
> Paradise Lost
Paradise Lost
~~~~~~~~~~~~~~

### Where is the main function?

Unlike C programs, OCaml program do not have a ``\texttt{main}''
function. When an OCaml program is evaluated, all the statements in
the implementation files are evaluated in order.  In general,
implementation files can contain arbitrary expressions, not just
function definitions. For this example, the main program is the
