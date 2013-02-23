## Input and Output

Imperative programming is about more than modifying in-memory
data-structures.  Any function that doesn't boil down to a
deterministic transformation of its inputs is imperative in nature,
and that includes operations that interact with the world outside of
your program.  An important example of this kind of interaction is
I/O, operations for reading or writing data to destinations such as
files, terminal input and output, and network sockets.

There are multiple I/O libraries in OCaml.  In this section we'll
discuss OCaml's simple buffered I/O that can be used through the
`In_channel` and `Out_channel` modules in Core.  Other I/O primitives
are also available through the `Unix` module in Core as well as
`Async`, the asynchronous I/O library that is covered in
[xref](#)concurrent-programming-with-async).  Note that most of the
functionality in Core's `In_channel`, `Out_channel` (and in the `Unix`
module) derives from the standard library.  Although we use Core's
interfaces here, most of what is described is equally applicable to
the standard library.

OCaml models input and output channels with separate types
`in_channel` and `out_channel`.  Each OCaml process has three standard
channels, mapping on to the three standard file descriptors in Unix.

* `In_channel.stdin`.  The "standard input" channel.  By default,
  input comes from the terminal, which handles keyboard input.

* `In_channel.stdout`.  The "standard output" channel.  By default,
  output written to `stdout` appears on the user terminal.

* `In_channel.stderr`.  The "standard error" channel.  This is similar
  to `stdout`, but is intended for error messages.

The values `stdin`, `stdout` and `stderr` are useful enough that they
are also available in the global name-space directly, without having
to go through the `In_channel` and `Out_channel` modules.

You can use these channels for writing a simple interactive program.
Here's an example.

```ocaml
```



* You should make sure to close files before they are collected by the
  GC.  If you don't close files before they're released, and you open
  a lot of files, it's very easy to end up with a resource leak.  In
  particular, on UNIX, you will eventually run out of file
  descriptors.
* Channels are buffered.  That means if you want to make sure that
  something shows up immediately, you need to make sure to explicitly
  flush the channel to which you are writing.
* When reading large data streams, you want to be able to consume
  information incrementally.  For line oriented output, convenient
  functions are provided for reading chunks of data at a time.  For
  more complex formats that you want to be able to handle
  incrementally, you're better off using a more sophisticated library
  like Async.







### Basic file input and output

It isn't necessary to perform all input through the standard channels.  There
are also functions to open files for reading and writing.

* `open_out : string -> out_channel` open a file for writing.

* `open_in : string -> in_channel` open a file for reading.

* `close_out : out_channel -> unit` close an output channel, flushing any pending
  output to the file.

* `close_in : in_channel -> unit` close an input channel.

The functions for input and output are similar to the `print_...` and `read_...`
functions, but they use the prefix `output_` and `input_` and they take a
channel as an argument.  Let's write a function to print the contents of a file.

```ocaml
# let cat filename =
   let inx = open_in filename in
   try
      while true do
         print_string (input_line inx); print_char '\n'
      done
   with End_of_file ->
      close_in inx;;
val cat : string -> unit = <fun>
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# close_out outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

It is important to close channels when you are finished with them, for two
reasons.  One reason is that the runtime system will usually have a limit on the
number of channels that may be open simultaneously.  To avoid problems, you
should close channels you are not using.  In addition, for efficiency, output is
buffered and written to output channels in larger blocks.  The output may not
actually be written to the file unless the `out_channel` is closed, or with an
explicit call to the `flush` function.

* `flush : out_channel -> unit`

```ocaml
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# cat "file.txt";;
- : unit = ()
# flush outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

### Formatted output with Printf

Output with the standard library functions like `output_char`, `output_int`,
`output_string`, etc. is simple, but there is little flexibility, and it is
often verbose.  OCaml also supports _formatted_ output using the `printf`
function, which is modeled after `printf` in the C standard library.  The
`printf` function takes a _format string_ that describe what to print and how to
format it, as well as arguments to be printed.

```ocaml
printf format arg1 ... argN
```

The format string can contain character literals, which are printed without
change, as well as conversion specifications, which specify how to print an
value that is provided as an argument.  A conversion specification begins with a
percent (`%`) character, some flags, and a type specification.  For example,
`%d` is a conversion specification for printing an integer, `%s` prints a
string, and `%f` prints a floating-point value.

```ocaml
# open Printf;;
# printf "int=%d string=%s float=%f\n" 10 "abc" 1e5;;
int=10 string=abc float=100000.000000
- : unit = ()
```

Conversions can also specify additional parameters for formatting the value.
The general form has flags, width specified in number of characters, and decimal
precision used for printing floating-point values.  These parameters are
optional.

```ocaml
% [flags] [width] [.precision] type
```

There are several flags of interest, the flag `'-'` left-justifies the output,
and the flag `'0'` pads numerical values with leading zeroes.
Let's write a program to print a price list in a tabular form.

```ocaml
# let print_prices prices =
    print_string "--------------------------------\n\
                  | Size   | Hexcode    | Price  |\n\
                  --------------------------------\n";
    List.iter (fun (size, code, price) ->
          printf "| %-6s | 0x%08x | %6.2f |\n" size code price) prices;
    print_string "--------------------------------\n";;
val print_prices : (string * int * float) list -> unit = <fun>
# print_prices
   [("small", 0x35, 1.02);
    ("medium", 0x7a1, 50.75);
    ("large", 0xbad, 400.8);
    ("vente", 0x11, 4136.);
    ("enormous", 100000, 100.)];;
--------------------------------
| Size   | Hexcode    | Price  |
--------------------------------
| small  | 0x00000035 |   1.02 |
| medium | 0x000007a1 |  50.75 |
| large  | 0x00000bad | 400.80 |
| vente  | 0x00000011 | 4136.00 |
| enormous | 0x000186a0 | 100.00 |
--------------------------------
- : unit = ()
```

The format specification `"| %-6s | 0x%08x | %6.2f |\n"` specifies that the
first argument is a string that is to be left-justified and printed six
characters wide; the second argument is an integer that should be printed in
hexidecimal (format type `x`) eight characters wide with leading zeroes; and the
third argument is a floating point value that should be printed right-justified,
six characters wide, with two digits after the decimal point.

Note that the width specifies a _minimum_ width.  If the value requires more width to print completely the width is increased.  The price `4136.00` and the size `enormous` both overflow the width, breaking the tabular form.

### Strong typing and format strings

In OCaml, `printf` is type safe.  The format is checked against the arguments at
compile time, and rejected if the format string is malformed, or if the format
does not match the arguments.  In the following examples, `%v` is not a valid
conversion specification, and floating-point values can't be printed with a
decimal conversion specification.

```ocaml
# printf "Hello %v" 1;;
Characters 7-17:
  printf "Hello %v" 1;;
         ^^^^^^^^^^
Error: Bad conversion %v, at char number 6 in format string ``Hello %v''
# printf "Hello %d" 1.;;
Characters 18-20:
  printf "Hello %d" 1.;;
                    ^^
Error: This expression has type float but an expression was expected of type
         int
```

A consequence of strong typing is that the format string must ultimately be a
string _literal_, known at compile time.  It can't be computed by the program at
runtime.

```ocaml
# let fmt = "%s";;
val fmt : string = "%s"
# printf fmt "Hello";;
Characters 7-10:
  printf fmt "Hello";;
         ^^^
Error: This expression has type string but an expression was expected of type
         ('a -> 'b, out_channel, unit) format =
           ('a -> 'b, out_channel, unit, unit, unit, unit) format6
```

Actually, this is not entirely true.  The `printf` function takes a format
string of type `('a, 'b, 'c) format` and it is only the type conversion from
`string` to `format` where the string is required to be a literal.

```ocaml
# let fmt : ('a, 'b, 'c) format = "Size: %s.\n";;
val fmt : (string -> 'c, 'b, 'c) format = <abstr>
# printf fmt "small";;
Size: small
- : unit = ()
```

### Output to channels, strings, and buffers

The function `printf` prints to the standard output channel `stdout`.  There are alse several variations.

* `eprintf format arg1 ... argN`.  Print to the standard error channel `stderr`.

* `fprintf channel format arg1 ... argN`.  Print to the channel `channel`.

* `sprintf format arg1 ... argN`.  Produce a string as output.

* `bprintf buffer format arg1 ... argN`.  Print to a string buffer
  `buffer` of type `Buffer.t`.

```ocaml
# sprintf "The number is %10d." 123;;
- : string = "The number is        123."
# let buf = Buffer.create 32;;
val buf : Buffer.t = <abstr>
# bprintf buf "%s\n" "First line";;
- : unit = ()
# bprintf buf "%20s.\n" "Second line";;
- : unit = ()
# Buffer.contents buf;;
- : string = "First line\n         Second line.\n"
```

### Formatted input with Scanf

The `scanf` function can be used for reading input, similar to
`printf`.  The general form takes a format string and a "receiver"
function that is applied to the input.  The format string contains
literal characters, which must be read literally, and conversion
specifications similar to `printf`.  The result of the receiver
function is returned as the result of `scanf`.

```ocaml
# scanf "%d" (fun i -> i);;
1871
- : int = 1871
```

For another example, consider the parsing a date string that might be
in one of two forms, _month day, year_ or _month/day/year_.  The
`scanf` functions raise the exception `Scan_failure` on error, so we
can read a date string by trying both kinds of format.

```ocaml
# type month = String of string | Int of int;;
type month = String of string | Int of int
# let scan_date_string s =
    try
      sscanf s "%s %d, %d" (fun month day year ->
             year, String month, day)
    with Scan_failure _ | End_of_file ->
      sscanf s "%d/%d/%d" (fun month day year ->
             year, Int month, day);;
val scan_date_string : string -> int * month * int = <fun>
# scan_date_string "May 3, 1921";;
- : int * month * int = (1921, String "May", 3)
# scan_date_string "10/11/2001";;
- : int * month * int = (2001, Int 10, 11)
# scan_date_string "May 3";;
Exception:
Scanf.Scan_failure
 "scanf: bad input at char number 0: ``character 'M' is not a decimal digit''".
```

## Summary

The OCaml language supports a fairly standard imperative programming model, with
looping, assignment, mutable arrays, records, and objects.  If desired, we can
write programs that correspond directly to what we would have written in some
other imperative language like C or Java.  Of course, doing so is really not the
best match -- if you want to write imperative programs, you should probably use
an imperative programming language.

However, there are times when imperative programming might provide efficiency
(as with lazy evaluation, or memoization), or you might require techniques or
data structures that are traditional imperative (like graphs represented with
adjacency lists), and in these cases OCaml usually shines.  Used with
discretion, imperative programming can lead to smaller, simpler programs.


## Detritus

* `print_char : char -> unit` prints a single character to `stdout`.

* `print_string : string -> unit` prints a `string` to `stdout` as a
  sequence of characters, without quotes.

* `print_int : int -> unit` prints an integer to `stdout`.

* `print_float : float -> unit` prints a floating-point number to
  `stdout`.

Functions to write to `stderr` have similar names, using the prefix `prerr_`
rather than `print_` (for example, `prerr_string`).

```ocaml
# let i = 1;;
val i : int = 1
# print_string "The value of i is "; print_int i; print_string ".\n";;
The value of i is 1.
- : unit = ()
```

Input is similar, but there are only a few functions.

* `read_line : unit -> string` reads a line of input from `stdin`.

* `read_int : unit -> int` reads a decimal integer from `stdin`.

* `read_float : unit -> float` reads a floating-point number from `stdin`.

These functions raise the exception `End_of_file` if the input channel is
terminated before the input is read.

Here is a function to read a sequence of integers from `stdin`, sorting them and
printing the result to `stdout`.


-------------





## Input and Output

Input and output is another kind of imperative operation, where the
purpose is to either read input from a file, stream, or device,
_consuming_ the input by side-effect; or write output to a file,
stream, or device, _modifying_ the output by side-effect.  There are
several I/O libraries in OCaml.  There is the basic builtin I/O
library in the `Pervasives` module, and there are moe advanced I/O
libraries in the `Unix` and `Async` modules.  Let's look at the basic
library -- the advanced libraries will be described elsewhere.

For basic I/O OCaml models input and output with _channels_.  An
`in_channel` is used for reading input, and and `out_channel` for
producing output.  Each OCaml process has three standard channels,
similar to the three standard files in Unix.

* `stdin : in_channel`.  The "standard input" channel.  By default, input comes
  from the terminal, which handles keyboard input.

* `stdout : out_channel`.  The "standard output" channel.  By default, output
  written to `stdout` appears on the user terminal.

* `stderr : out_channel`.  The "standard error" channel.  This is similar to
  `stdout`, but it is intended for error messages.

The standard library has several functions for reading from and writing to the
standard channels.

* `print_char : char -> unit` prints a single character to `stdout`.

* `print_string : string -> unit` prints a `string` to `stdout` as a sequence of
  characters, without quotes.

* `print_int : int -> unit` prints an integer to `stdout`.

* `print_float : float -> unit` prints a floating-point number to `stdout`.

Functions to write to `stderr` have similar names, using the prefix `prerr_`
rather than `print_` (for example, `prerr_string`).

```ocaml
# let i = 1;;
val i : int = 1
# print_string "The value of i is "; print_int i; print_string ".\n";;
The value of i is 1.
- : unit = ()
```

Input is similar, but there are only a few functions.

* `read_line : unit -> string` reads a line of input from `stdin`.

* `read_int : unit -> int` reads a decimal integer from `stdin`.

* `read_float : unit -> float` reads a floating-point number from `stdin`.

These functions raise the exception `End_of_file` if the input channel is
terminated before the input is read.

Here is a function to read a sequence of integers from `stdin`, sorting them and
printing the result to `stdout`.

```ocaml
# let collate_input () =
    In_channel.input_lines stdin
    |> List.map ~f:Int.of_string
    |> List.sort ~cmp:Int.compare
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:" "
    |> print_endline
  ;;
val collate_input : unit -> unit = <fun>
# collate_input ();;
8
56
2
34
-120
19
-120 2 8 19 34 56
- : unit = ()
```

Using exceptions to signal the end of input is a little awkward.  The
`read_input` loop above converts the exception into an `int option`, so that
matching can be performed with `match` and the function is tail-recursive.  The
following function is _not_ tail recursive, and should be avoided.

```ocaml
  (* AVOID -- non tail-recursive input reader *)
# let rec read_input items =
   try read_input (read_int () :: items) with
      End_of_file -> items;;
val read_input : int list -> int list = <fun>
# read_input [];;
34
45
56
-1
- : int list = [-1; 56; 45; 34]
```

Another way to address the input issue is to use iteration, rather than
recursion.  This requires collecting the input in a container than can be
mutated by side-effect.

```ocaml
# let read_input () =
   let items = ref [] in
   try
      while true do
         items := read_int () :: !items
      done;
      []  (* not reached *)
   with End_of_file ->
      !items;;
val read_input : unit -> int list = <fun>
# read_input ();;
45
78
-345
- : int list = [-345; 78; 45]
```

In this loop, the value returns from a successful call to `read_int ()` is added
to the `items` list by side-effect.  When `read_int ()` reaches the end of
input, it raises the `End_of_file` exception, terminating the loop.  The `while`
loop never terminates; the `[]` value afterward is only to satisfy the type
checker.

### Basic file input and output

It isn't necessary to perform all input through the standard channels.  There
are also functions to open files for reading and writing.

* `open_out : string -> out_channel` open a file for writing.

* `open_in : string -> in_channel` open a file for reading.

* `close_out : out_channel -> unit` close an output channel, flushing any pending
  output to the file.

* `close_in : in_channel -> unit` close an input channel.

The functions for input and output are similar to the `print_...` and `read_...`
functions, but they use the prefix `output_` and `input_` and they take a
channel as an argument.  Let's write a function to print the contents of a file.

```ocaml
# let cat filename =
   let inx = open_in filename in
   try
      while true do
         print_string (input_line inx); print_char '\n'
      done
   with End_of_file ->
      close_in inx;;
val cat : string -> unit = <fun>
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# close_out outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

It is important to close channels when you are finished with them, for two
reasons.  One reason is that the runtime system will usually have a limit on the
number of channels that may be open simultaneously.  To avoid problems, you
should close channels you are not using.  In addition, for efficiency, output is
buffered and written to output channels in larger blocks.  The output may not
actually be written to the file unless the `out_channel` is closed, or with an
explicit call to the `flush` function.

* `flush : out_channel -> unit`

```ocaml
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# cat "file.txt";;
- : unit = ()
# flush outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

### Formatted output with Printf

Output with the standard library functions like `output_char`, `output_int`,
`output_string`, etc. is simple, but there is little flexibility, and it is
often verbose.  OCaml also supports _formatted_ output using the `printf`
function, which is modeled after `printf` in the C standard library.  The
`printf` function takes a _format string_ that describe what to print and how to
format it, as well as arguments to be printed.

```ocaml
printf format arg1 ... argN
```

The format string can contain character literals, which are printed without
change, as well as conversion specifications, which specify how to print an
value that is provided as an argument.  A conversion specification begins with a
percent (`%`) character, some flags, and a type specification.  For example,
`%d` is a conversion specification for printing an integer, `%s` prints a
string, and `%f` prints a floating-point value.

```ocaml
# open Printf;;
# printf "int=%d string=%s float=%f\n" 10 "abc" 1e5;;
int=10 string=abc float=100000.000000
- : unit = ()
```

Conversions can also specify additional parameters for formatting the value.
The general form has flags, width specified in number of characters, and decimal
precision used for printing floating-point values.  These parameters are
optional.

```ocaml
% [flags] [width] [.precision] type
```

There are several flags of interest, the flag `'-'` left-justifies the output,
and the flag `'0'` pads numerical values with leading zeroes.
Let's write a program to print a price list in a tabular form.

```ocaml
# let print_prices prices =
    print_string "--------------------------------\n\
                  | Size   | Hexcode    | Price  |\n\
                  --------------------------------\n";
    List.iter (fun (size, code, price) ->
          printf "| %-6s | 0x%08x | %6.2f |\n" size code price) prices;
    print_string "--------------------------------\n";;
val print_prices : (string * int * float) list -> unit = <fun>
# print_prices
   [("small", 0x35, 1.02);
    ("medium", 0x7a1, 50.75);
    ("large", 0xbad, 400.8);
    ("vente", 0x11, 4136.);
    ("enormous", 100000, 100.)];;
--------------------------------
| Size   | Hexcode    | Price  |
--------------------------------
| small  | 0x00000035 |   1.02 |
| medium | 0x000007a1 |  50.75 |
| large  | 0x00000bad | 400.80 |
| vente  | 0x00000011 | 4136.00 |
| enormous | 0x000186a0 | 100.00 |
--------------------------------
- : unit = ()
```

The format specification `"| %-6s | 0x%08x | %6.2f |\n"` specifies that the
first argument is a string that is to be left-justified and printed six
characters wide; the second argument is an integer that should be printed in
hexidecimal (format type `x`) eight characters wide with leading zeroes; and the
third argument is a floating point value that should be printed right-justified,
six characters wide, with two digits after the decimal point.

Note that the width specifies a _minimum_ width.  If the value requires more width to print completely the width is increased.  The price `4136.00` and the size `enormous` both overflow the width, breaking the tabular form.

### Strong typing and format strings

In OCaml, `printf` is type safe.  The format is checked against the arguments at
compile time, and rejected if the format string is malformed, or if the format
does not match the arguments.  In the following examples, `%v` is not a valid
conversion specification, and floating-point values can't be printed with a
decimal conversion specification.

```ocaml
# printf "Hello %v" 1;;
Characters 7-17:
  printf "Hello %v" 1;;
         ^^^^^^^^^^
Error: Bad conversion %v, at char number 6 in format string ``Hello %v''
# printf "Hello %d" 1.;;
Characters 18-20:
  printf "Hello %d" 1.;;
                    ^^
Error: This expression has type float but an expression was expected of type
         int
```

A consequence of strong typing is that the format string must ultimately be a
string _literal_, known at compile time.  It can't be computed by the program at
runtime.

```ocaml
# let fmt = "%s";;
val fmt : string = "%s"
# printf fmt "Hello";;
Characters 7-10:
  printf fmt "Hello";;
         ^^^
Error: This expression has type string but an expression was expected of type
         ('a -> 'b, out_channel, unit) format =
           ('a -> 'b, out_channel, unit, unit, unit, unit) format6
```

Actually, this is not entirely true.  The `printf` function takes a format
string of type `('a, 'b, 'c) format` and it is only the type conversion from
`string` to `format` where the string is required to be a literal.

```ocaml
# let fmt : ('a, 'b, 'c) format = "Size: %s.\n";;
val fmt : (string -> 'c, 'b, 'c) format = <abstr>
# printf fmt "small";;
Size: small
- : unit = ()
```

### Output to channels, strings, and buffers

The function `printf` prints to the standard output channel `stdout`.  There are alse several variations.

* `eprintf format arg1 ... argN`.  Print to the standard error channel `stderr`.

* `fprintf channel format arg1 ... argN`.  Print to the channel `channel`.

* `sprintf format arg1 ... argN`.  Produce a string as output.

* `bprintf buffer format arg1 ... argN`.  Print to a string buffer
  `buffer` of type `Buffer.t`.

```ocaml
# sprintf "The number is %10d." 123;;
- : string = "The number is        123."
# let buf = Buffer.create 32;;
val buf : Buffer.t = <abstr>
# bprintf buf "%s\n" "First line";;
- : unit = ()
# bprintf buf "%20s.\n" "Second line";;
- : unit = ()
# Buffer.contents buf;;
- : string = "First line\n         Second line.\n"
```

### Formatted input with Scanf

The `scanf` function can be used for reading input, similar to
`printf`.  The general form takes a format string and a "receiver"
function that is applied to the input.  The format string contains
literal characters, which must be read literally, and conversion
specifications similar to `printf`.  The result of the receiver
function is returned as the result of `scanf`.

```ocaml
# scanf "%d" (fun i -> i);;
1871
- : int = 1871
```

For another example, consider the parsing a date string that might be
in one of two forms, _month day, year_ or _month/day/year_.  The
`scanf` functions raise the exception `Scan_failure` on error, so we
can read a date string by trying both kinds of format.

```ocaml
# type month = String of string | Int of int;;
type month = String of string | Int of int
# let scan_date_string s =
    try
      sscanf s "%s %d, %d" (fun month day year ->
             year, String month, day)
    with Scan_failure _ | End_of_file ->
      sscanf s "%d/%d/%d" (fun month day year ->
             year, Int month, day);;
val scan_date_string : string -> int * month * int = <fun>
# scan_date_string "May 3, 1921";;
- : int * month * int = (1921, String "May", 3)
# scan_date_string "10/11/2001";;
- : int * month * int = (2001, Int 10, 11)
# scan_date_string "May 3";;
Exception:
Scanf.Scan_failure
 "scanf: bad input at char number 0: ``character 'M' is not a decimal digit''".
```

## Summary

The OCaml language supports a fairly standard imperative programming model, with
looping, assignment, mutable arrays, records, and objects.  If desired, we can
write programs that correspond directly to what we would have written in some
other imperative language like C or Java.  Of course, doing so is really not the
best match -- if you want to write imperative programs, you should probably use
an imperative programming language.

However, there are times when imperative programming might provide efficiency
(as with lazy evaluation, or memoization), or you might require techniques or
data structures that are traditional imperative (like graphs represented with
adjacency lists), and in these cases OCaml usually shines.  Used with
discretion, imperative programming can lead to smaller, simpler programs.


