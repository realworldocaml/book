# The Compiler Backend: Byte-code and Native-code

In this chapter, we'll cover the following topics:

* the bytecode `ocamlc` compiler and `ocamlrun` interpreter.
* the native code `ocamlopt` code generator, and debugging and profiling native code.

## Generating portable bytecode

After the lambda form has been generated, we are very close to having
executable code.  The OCaml tool-chain branches into two separate compilers at
this point.  We'll describe the the bytecode compiler first, which
consists of two pieces:

* `ocamlc` compiles files into a bytecode that is a close mapping to the lambda form.
* `ocamlrun` is a portable interpreter that executes the bytecode.

The big advantage of using bytecode is simplicity, portability and compilation
speed.  The mapping from the lambda form to bytecode is straightforward, and
this results in predictable (but slow) execution speed.

The interpreter uses the OCaml stack and an accumulator to store values. It
only has seven registers in total: the program counter, stack pointer,
accumulator, exception and argument pointers, and environment and global data.

You can display the bytecode instructions in textual form via `-dinstr`.  Try
this on one of our earlier pattern matching examples.

```console
$ ocamlc -dinstr pattern_monomorphic_exhaustive.ml 
	branch L2
L1:	acc 0
	switch 6 5 4 3/
L6:	const 100
	return 1
L5:	const 101
	return 1
L4:	const 102
	return 1
L3:	const 103
	return 1
L2:	closure L1, 0
	push
	acc 0
	makeblock 1, 0
	pop 1
	setglobal Pattern_monomorphic_exhaustive!
```

The bytecode above has been simplified from the lambda form into a set of
simple instructions that are executed in serial by the interpreter.

There are around 140 instructions in total, but most are just minor variants of
commonly encountered operations (e.g. function application at a specific
arity).  You can find full details [online](http://cadmium.x9c.fr/distrib/caml-instructions.pdf).

<note>
<title>Where did the bytecode instruction set come from?</title>

The bytecode interpreter is much slower than compiled native code, but is still
remarkably performant for an interpreter without a JIT compiler.  Its
efficiency can be traced back to Xavier Leroy's ground-breaking work in 1990 on
["The ZINC experiment: An Economical Implementation of the ML
Language"](http://hal.inria.fr/docs/00/07/00/49/PS/RT-0117.ps).

This paper laid the theoretical basis for the implementation of an instruction
set for a strictly evaluated functional language such as OCaml.  The bytecode
interpreter in modern OCaml is still based on the ZINC model.  The native code
compiler uses a different model since it uses CPU registers for function calls
instead of always passing arguments on the stack as the bytecode interpreter
does.

Understanding the reasoning behind the different implementations of the
bytecode interpreter and the native compiler is a very useful exercise for any
budding language hacker.

</note>

### Compiling and linking bytecode 

The `ocamlc` command compiles individual `ml` files into bytecode files that
have a `cmo` extension.  The compiled bytecode files are matched with the
associated `cmi` interface which contains the type signature exported to
other compilation units.

A typical OCaml library consists of multiple source files and hence multiple
`cmo` files that all need to passed on the command line to use the library.
The compiler can combine these into a more convenient archive file by using the
`-a` flag.  Bytecode archives are denoted by the `cma` extension.

The individual objects in the library are linked as regular `cmo` files in the
order specified when the library file was built.  If an object file within the
library isn't referenced elsewhere in the program, then it isn't included in
the final binary unless the `-linkall` flag forces its inclusion.  This
behaviour is analogous to how C handles object files and archives (`.o` and
`.a` respectively).

The bytecode files are then linked together with the OCaml standard library to
produce an executable program.  The order in which `.cmo` arguments are
presented on the command line defines the order in which compilation units are
initialized at runtime.  Remember that OCaml has no single `main` function like
C, so this link is order is more important than in C.

### Executing bytecode

The bytecode runtime comprises three parts: the bytecode interpreter, garbage
collector, and a set of C functions that implement the primitive operations.
The bytecode contains instructions to call these C functions when required.

The OCaml linker produces bytecode targeted the standard OCaml runtime by
default, and so needs to know about any C functions that are referenced from
other libraries that aren't loaded by default.

Information about these extra libraries can be specified while linking a
bytecode archive.


```console
$ ocamlc -a -o mylib.cma a.cmo b.cmo -dllib -lmylib
```

The `dllib` flag embeds the arguments in the archive file.  Any subsequent
packages linking this archive will also include the extra C linking directive.
This in turn lets the interpreter dynamically load the external library symbols
when it executes the bytecode.

You can also generate a complete standalone executable that bundles the
`ocamlrun` interpreter with the bytecode in a single binary.  This is known as
a *custom runtime* mode and is built as follows.

```console
$ ocamlc -a -o mylib.cma -custom a.cmo b.cmo -cclib -lmylib
```

The custom mode is the most similar mode to native code compilation, as both
generate standalone executables.  There are quite a few other options available
for compiling bytecode (notably with shared libraries or building custom
runtimes).  Full details can be found in the
[manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual022.html).

### Embedding OCaml bytecode in C

A consequence of using the bytecode compiler is that the final link phase must
be performed by `ocamlc`.  However, you might sometimes want to embed your OCaml
code inside an existing C application.  OCaml also supports this mode of operation
via the `-output-obj` directive.

This mode causes `ocamlc` to output a C object file that containing the
bytecode for the OCaml part of the program, as well as a `caml_startup`
function.  All of the OCaml modules are linked into this object file as
bytecode, just as they would be for an executable.

This object file can then be linked with C code using the standard C compiler,
and only needs the bytecode runtime library (which is installed as
`libcamlrun.a`).  Creating an executable just requires you to link the runtime
library with the bytecode object file.  Here's an example to show how it all
fits together.

Create two OCaml source files that contain a single print line.

```console
$ cat embed_me1.ml 
let () = print_endline "hello embedded world 1"
$ cat embed_me2.ml 
let () = print_endline "hello embedded world 2"
```

Next, create a C file which will be your main entry point.

```c
/* main.c */
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

int 
main (int argc, char **argv)
{
  puts("Before calling OCaml");
  caml_startup (argv);
  puts("After calling OCaml");
  return 0;
}
```

Now compile the OCaml files into a standalone object file.

```console
$ ocamlc -output-obj -o embed_out.o embed_me1.ml embed_me2.ml
```

After this point, you no longer need the OCaml compiler, as `embed_out.o` has
all of the OCaml code compiled and linked into a single object file.  Compile
an output binary using gcc to test this out.

```console
$ gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lcamlrun -ltermcap \
  -o final_out embed_out.o main.c
$ ./final_out 
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

Once inconvenience with `gcc` is that you need to specify the location of the
OCaml library directory.  The OCaml compiler can actually handle C object and
sources directly.  It passes these through to the system C compiler but adds
its standard directory and runtime on the way.  You can thus compile the
previous object file much more simply with `ocamlc`.

```console
$ ocamlc -o final_out2 embed_out.o main.c
$ ./final_out2
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

You can inspect the commands that `ocamlc` is invoking by adding `-verbose` to
the command line.  You can even obtain the C source code to the `-output-obj`
result by specifying a `.c` output file extension instead of the `.o` we used
earlier.

```console
$ ocamlc -output-obj -o embed_out.c embed_me1.ml embed_me2.ml
$ cat embed_out.c
```

Embedding OCaml code like this lets you write OCaml that interfaces with any
environment that works with a C compiler.   You can even cross back from the C
code into OCaml by using the `Callback` module to register named entry points
in the OCaml code.  This is explained in detail in the [interfacing with
C](http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html#toc149) section of
the OCaml manual.

## Compiling fast native code

The native code compiler is ultimately the tool that most production OCaml code
goes through.  It compiles the lambda form into fast native code executables,
with cross-module inlining and additional optimization passes that the bytecode
interpreter doesn't perform.  Care is taken to ensure compatibility with the
bytecode runtime, so the same code should run identically when compiled with
either toolchain.

The `ocamlopt` command is the frontend to the native code compiler, and has a
very similar interface to `ocamlc`.  It also accepts `ml` and `mli` files, but
compiles them to:

* A `.o` file containing native object code.
* A `.cmx` file containing extra information for linking and cross-module optimization.
* A `.cmi` compiled interface file that is the same as the bytecode compiler.

When the compiler links modules together into an executable, it uses the
contents of the `cmx` files to perform cross-module inlining across compilation
units.  This can be a significant speedup for standard library functions that
are frequently used outside of their module.

Collections of `.cmx` and `.o` files can also be be linked into a `.cmxa`
archive by passing the `-a` flag to the compiler.  However, unlike the bytecode
version, you must keep the individual `cmx` files in the compiler search path
so that they are available for cross-module inlining.  If you don't do this,
the compilation will still succeed, but you will have missed out on an
important optimization and have slower binaries.

### Inspecting assembly output

The native code compiler generates assembly language that is then passed to the
system assembler for compiling into object files.  You can get `ocamlopt` to
output the assembly by passing the `-S` flag to the compiler command-line.

The assembly code is highly architecture specific, so the discussion below
assumes an Intel or AMD 64-bit platform.  We've generated the example code
using `-inline 20` and `-nodynlink` since it's best to generate assembly code
with the full optimizations that the compiler supports. Even though these
optimizations make the code a bit harder to read, it will give you a more
accurate picture of what executes on the CPU.  Don't forget that you can use
the lambda code from earlier to get a slightly higher level picture of the code
if you get lost in the more verbose assembly.

#### The impact of polymorphic comparison

We warned you earlier in [xref](#maps-and-hashtables) that using polymorphic
comparison is both convenient and perilous.  Let's look at precisely what
the difference is at the assembly language level now.

First create a comparison function where we've explicitly annotated
the types, so the compiler knows that only integers are being compared.

```ocaml
(* compare_mono.ml *)
let cmp (a:int) (b:int) =
  if a > b then a else b
```

Now compile this into assembly and read the resulting `compare_mono.S` file.

```console
$ ocamlopt -inline 20 -nodynlink -S compare_mono.ml
$ cat compare_mono.S
```

If you've never seen assembly language before then the contents may be rather
scary.  While you'll need to learn x86 assembly to fully understand it, we'll
try to give you some basic instructions to spot patterns in this section.  The
excerpt of the implementation of the `cmp` function can be found below.

```
_camlCompare_mono__cmp_1008:
	.cfi_startproc
.L101:
	cmpq	%rbx, %rax
	jle	.L100
	ret
	.align	2
.L100:
	movq	%rbx, %rax
	ret
	.cfi_endproc
```

The `_camlCompare_mono__cmp_1008` is an assembly label that has been computed
from the module name (`Compare_mono`) and the function name (`cmp_1008`).  The
numeric suffix for the function name comes straight from the lambda form (which
you can inspect using `-dlambda`, but in this case isn't necessary).

The arguments to `cmp` are passed in the `%rbx` and `%rax` registers, and
compared using the `jle` "jump if less than or equal" instruction.  This
requires both the arguments to be immediate integers to work.  Now let's see
what happens if our OCaml code omits the type annotations and is a polymorphic
comparison instead.

```ocaml
(* compare_poly.ml *)
let cmp a b =
  if a > b then a else b
```

Compiling this code with `-S` results in a significantly more complex assembly
output for the same function.

```
_camlCompare_poly__cmp_1008:
        .cfi_startproc
        subq    $24, %rsp
        .cfi_adjust_cfa_offset  24
.L101:
        movq    %rax, 8(%rsp)
        movq    %rbx, 0(%rsp)
        movq    %rax, %rdi
        movq    %rbx, %rsi
        leaq    _caml_greaterthan(%rip), %rax
        call    _caml_c_call
.L102:
        leaq    _caml_young_ptr(%rip), %r11
        movq    (%r11), %r15
        cmpq    $1, %rax
        je      .L100
        movq    8(%rsp), %rax
        addq    $24, %rsp
        .cfi_adjust_cfa_offset  -24
        ret
        .cfi_adjust_cfa_offset  24
        .align  2
.L100:
        movq    0(%rsp), %rax
        addq    $24, %rsp
        .cfi_adjust_cfa_offset  -24
        ret
        .cfi_adjust_cfa_offset  24
        .cfi_endproc
```

The `.cfi` directives are assembler hints that contain Call Frame Information
that lets the GNU debugger provide more sensible backtraces, and have no effect
on runtime performance.  Notice that the rest of the implementation is no
longer a simple register comparison.  Instead, the arguments are pushed on the
stack (the `%rsp` register) and a C function call is invoked by placing a
pointer to `caml_greaterthan` in `%rax` and jumping to `caml_c_call`.

OCaml on 64-bit Intel architectures caches the location of the minor heap in
the `%r11` register since it's so frequently referenced in OCaml functions.
This register isn't guaranteed to be preserved when calling into C code (which
can clobber `%r11` for its own purposes), and so `%r11` is restored after
returning from the `caml_greaterthan` call.  Finally the return value of the
comparison is popped from the stack and returned.

<tip>
<title>Reading the implementation of the C primitives</title>

If you have a copy of the OCaml source tree handy, it's worth reading through
the definition of `caml_greaterthan()`.  The built-in primitives for
polymorphic comparison can be found in `caml/byterun/compare.c`.

The key function is `compare_val()`, which directly examines the runtime
representation of two OCaml values to decide which is greater.  This requires
the header tag to be examined, and recursive structures must be tested
step-by-step.

Avoiding running all of this code is why you should try to write explicit
comparison functions in OCaml instead.

</tip>

#### Benchmarking polymorphic comparison

You don't have to fully understand the intricacies of assembly language to see
that this polymorphic comparison is much heavier than the simple monomorphic
integer comparison from earlier.  Let's confirm this hypothesis again by
writing a quick `Core_bench` test with both functions.

```ocaml
$ cat bench_poly_and_mono.ml 
open Core.Std
open Core_bench.Std

let polymorphic_compare () =
  let cmp a b = if a > b then a else b in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let monomorphic_compare () =
  let cmp (a:int) (b:int) =
    if a > b then a else b in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let tests = [
    "Polymorphic comparison", polymorphic_compare;
    "Monomorphic comparison", monomorphic_compare ]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run
```

Running this shows quite a significant runtime difference between the two.

```console
$ ./bench_poly_and_mono.native 
Estimated testing time 20s (change using -quota SECS).
┌────────────────────────┬───────────┬───────────────┬────────────┐
│ Name                   │ Time (ns) │     Time 95ci │ Percentage │
├────────────────────────┼───────────┼───────────────┼────────────┤
│ Polymorphic comparison │    10_087 │ 10_080-10_096 │     100.00 │
│ Monomorphic comparison │    585.51 │ 584.60-586.57 │       5.80 │
└────────────────────────┴───────────┴───────────────┴────────────┘
```

We see that the polymorphic comparison is close to 20 times slower!  These
results shouldn't be taken too seriously as this is a very narrow test, which
like all such microbenchmarks aren't representative of more complex codebases.
However, if you're building numerical code that runs many iterations in a tight
inner loop, it's worth manually peering at the produced assembly code to see if
you can hand-optimize it.

### Debugging native code binaries

The native code compiler builds executables that can be debugged using
conventional system debuggers such as GNU `gdb`.  You need to compile your
libraries with the `-g` option to add the debug information to the output, just
as you need to with C compilers.

Extra debugging information is inserted into the output assembly when the
library is compiled in debug mode.  These include the CFI stubs you will have
noticed in the profiling output earlier (`.cfi_start_proc` and `.cfi_end_proc`
to delimit an OCaml function call, for example).

#### Understanding name mangling

So how do you refer to OCaml functions into an interactive debugger like `gdb`?
The first thing you need to know is how function names compile down into C
symbols; a procedure generally called *name mangling*.

Each OCaml source file is compiled into a native object file that must export a
unique set of symbols to comply with the C binary interface.  This means that
any OCaml values that may be used by another compilation unit need to be mapped
into a symbol name.  This mapping fhas to account for OCaml language features
such as nested modules, anonymous functions and variable names that shadow each
other.

The conversion follows some straightforward rules for named variables and
functions:

* The symbol is prefixed by `caml` and the local module name, with dots
  replaced by underscores.
* This is followed by a double `__` suffix and the variable name.
* The variable name is also suffixed by a `_` and a number.  This is
  the result of the lambda compilation that replaces each variable name
  with a unique value within the module.  You can determine this number
  by examining the `-dlambda` output from `ocamlopt`.

Anonymous functions are hard to predict without inspecting intermediate
compiler output.  If you need to debug them it's usually easier to modify the
source code to let-bind the anonymous function to a variable name.

#### Interactive breakpoints with the GNU debugger

Let's see name mangling in action with some interactive debugging in the
GNU `gdb` debugger.

<caution>
<title>Beware `gdb` on MacOS X</title>

The examples here assume that you are running `gdb` on either Linux or FreeBSD.
MacOS X does have `gdb` installed, but it's a rather quirky experience that
doesn't reliably interpret the debugging information contained in the native
binaries. This can result in function names showing up as raw symbols such as
`.L101` instead of their more human-readable form.

For OCaml 4.1, we'd recommend you do native code debugging on an alternate
platform such as Linux, or manually look at the assembly code output to map the
symbol names onto their precise OCaml functions.

</caution>

Let's write a mutually recursive function that selects alternating values from
a list.  This isn't tail recursive and so our stack size will grow as we
single-step through the execution.

```ocaml
(* alternate_list.ml : select every other value from an input list *)
open Core.Std

let rec take =
  function
  |[] -> []
  |hd::tl -> hd :: (skip tl)
and skip =
  function
  |[] -> []
  |hd::tl -> take tl

let () =
  take [1;2;3;4;5;6;7;8;9]
  |> List.map ~f:string_of_int
  |> String.concat ~sep:","
  |> print_endline
```

Compile and run this with debugging symbols. You should see the following
output:

```console
$ ocamlfind ocamlopt -g -package core -thread -linkpkg -o alternate alternate_list.ml
$ ./alternate
1,3,5,7,9
```

Now we can run this interactively within `gdb`.

```console
$ gdb ./alternate
GNU gdb (GDB) 7.4.1-debian
Copyright (C) 2012 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.  Type "show copying"
and "show warranty" for details.
This GDB was configured as "x86_64-linux-gnu".
For bug reporting instructions, please see:
<http://www.gnu.org/software/gdb/bugs/>...
Reading symbols from /home/avsm/alternate...done.
(gdb)
```

The `gdb` prompt lets you enter debug directives.  Let's set the program
to break just before the first call to `take`.

```console
(gdb) break camlAlternate_list__take_69242 
Breakpoint 1 at 0x5658d0: file alternate_list.ml, line 5.
```

We used the C symbol name by following the name mangling rules defined
earlier.  A convenient way to figure out the full name is by tab-completion.
Just type in a portion of the name and press the `<tab>` key to see
a list of possible completions.

Once you've set the breakpoint, start the program executing.

```console
(gdb) run
Starting program: /home/avsm/alternate
[Thread debugging using libthread_db enabled]
Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".

Breakpoint 1, camlAlternate_list__take_69242 () at alternate_list.ml:5
4	  function
```

The binary has run until the first take invocation and stopped, waiting
for further instructions.  GDB has lots of features, so let's continue
the program and check the stacktrace after a couple of recursions.

```console
(gdb) cont
Continuing.

Breakpoint 1, camlAlternate_list__take_69242 () at alternate_list.ml:5
4	  function
(gdb) cont
Continuing.

Breakpoint 1, camlAlternate_list__take_69242 () at alternate_list.ml:5
4	  function
(gdb) bt
#0  camlAlternate_list__take_69242 () at alternate_list.ml:4
#1  0x00000000005658e7 in camlAlternate_list__take_69242 () at alternate_list.ml:6
#2  0x00000000005658e7 in camlAlternate_list__take_69242 () at alternate_list.ml:6
#3  0x00000000005659f7 in camlAlternate_list__entry () at alternate_list.ml:14
#4  0x0000000000560029 in caml_program ()
#5  0x000000000080984a in caml_start_program ()
#6  0x00000000008099a0 in ?? ()
#7  0x0000000000000000 in ?? ()
(gdb) clear camlAlternate_list__take_69242
Deleted breakpoint 1 
(gdb) cont
Continuing.
1,3,5,7,9
[Inferior 1 (process 3546) exited normally]
```

The `cont` command resumes execution after a breakpoint has paused it, `bt`
displays a stack backtrace, and `clear` deletes the breakpoint so that the
application can execute until completion.  GDB has a host of other features
we won't cover here, but you view more guidelines via Mark Shinwell's talk
on ["Real-world debugging in OCaml"](http://www.youtube.com/watch?v=NF2WpWnB-nk<).

One very useful feature of OCaml native code is that C and OCaml both share the
same stack.  This means that GDB backtraces can give you a combined view of
what's going on in your program *and* runtime library.  This includes any calls
to C libraries or even callbacks into OCaml from the C layer if you're in an
embedded environment.

### Profiling native code 

The recording and analysis of where your application spends its execution time
is known as *performance profiling*.
OCaml native code binaries can be profiled just like any other C binary, by
using the name mangling described earlier to map between OCaml variable names
and the profiler output.

Most profiling tools benefit from having some instrumentation included in the
binary.  OCaml supports two such tools:

* GNU Gprof to measure execution time and call graphs.
* The [Perf](https://perf.wiki.kernel.org/) profiling framework in modern versions of Linux.

#### Gprof

Gprof produces an execution profile of an OCaml program by recording a call
graph of which functions call each other, and recording the time these calls
take during the program execution.

Getting precise information out of Gprof requires passing the `-p` flag to the
native code compiler when compiling *and* linking the binary.  This generates
extra code that records profile information to a file called `gmon.out` when
the program is executed.  This profile information then can then be examined
using Gprof.

#### Perf

Perf is a more modern alternative to Gprof that doesn't require you to
instrument the binary.  Instead, it uses hardware counters and debug
information within the binary to record information accurately.

Run Perf on a compiled binary to record information first.  We'll use our
write barrier benchmark from earlier which measures memory allocation versus
in-place modification.

```console
$ perf record -g ./barrier.native 
Estimated testing time 20s (change using -quota SECS).
┌───────────┬───────────┬─────────────────────┬────────────┐
│ Name      │ Time (ns) │           Time 95ci │ Percentage │
├───────────┼───────────┼─────────────────────┼────────────┤
│ mutable   │ 7_306_219 │ 7_250_234-7_372_469 │      96.83 │
│ immutable │ 7_545_126 │ 7_537_837-7_551_193 │     100.00 │
└───────────┴───────────┴─────────────────────┴────────────┘
[ perf record: Woken up 11 times to write data ]
[ perf record: Captured and wrote 2.722 MB perf.data (~118926 samples) ]
```

When this completes, you can interactively explore the results.

```console
$ perf report -g
+  48.86%  barrier.native  barrier.native     [.] camlBarrier__test_immutable_69282
+  30.22%  barrier.native  barrier.native     [.] camlBarrier__test_mutable_69279
+  20.22%  barrier.native  barrier.native     [.] caml_modify
```

This trace broadly reflects the results of the benchmark itself.  The mutable 
benchmark consists of the combination of the call to `test_mutable` and the
`caml_modify` write barrier function in the runtime.  This adds up to slightly
over half the execution time of the application.

Perf has a growing collection of other commands that let you archive these
runs and compare them against each other.  You can read more on the [homepage](http://perf.wiki.kernel.org).

<tip>
<title>Using the frame-pointer to get more accurate traces</title>

Although Perf doesn't require adding in explicit probes to the binary,
it does need to understand how to unwind function calls so that the kernel
can accurately record the function backtrace for every event.

OCaml stack frames are too complex for Perf to understand directly, and so
it needs the compiler to fall back to using the same conventions as C for
function calls.  On 64-bit Intel systems, this means that a special register
known as the *frame pointer* is used to record function call history.

Using the frame pointer in this fashion means a slowdown (typically around
3-5%) since it's no longer available for general-purpose use.  OCaml 4.1 thus
makes the frame pointer an optional feature that can be used to improve the
resolution of Perf traces.

OPAM provides a compiler switch that compiles OCaml with the frame pointer
activated.

```console
$ opam switch 4.01.0dev+fp
```

Using the frame pointer changes the OCaml calling convention, but OPAM takes
care of recompiling all your libraries with the new interface.  You can read
more about this on the OCamlPro
[blog](http://www.ocamlpro.com/blog/2012/08/08/profile-native-code.html).

</tip>

### Embedding native code in C

The native code compiler normally links a complete executable, but can also
output a standalone native object file just as the bytecode compiler can.  This
object file has no further dependencies on OCaml except for the runtime library.

The native code runtime is a different library from the bytecode one and is
installed as `libasmrun.a` in the OCaml standard library directory. 

Try this custom linking by using the same source files from the bytecode
embedding example earlier in this chapter.

```console
$ ocamlopt -output-obj -o embed_native.o embed_me1.ml embed_me2.ml
$ gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lasmrun -ltermcap \
  -o final_out_native embed_native.o main.c
./final_out_native
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

The `embed_native.o` is a standalone object file that has no further references
to OCaml code beyond the runtime library, just as with the bytecode runtime.

<tip>
<title>Activating the debug runtime</title>

Despite your best efforts, it is easy to introduce a bug into some components
such as C bindings that cause heap invariants to be violated.  OCaml includes a
`libasmrund.a` variant of the runtime library that is compiled with extra
debugging checks that perform extra memory integrity checks during every
garbage collection cycle.  Running these extra checks will abort the program
nearer the point of corruption and help isolate the bug in the C code.

To use the debug library, just link your program with the `-runtime-variant d` flag.

```
$ ocamlopt -runtime-variant d -verbose -o hello hello.ml hello_stubs.c
$ ./hello 
### OCaml runtime: debug mode ###
Initial minor heap size: 2048k bytes
Initial major heap size: 992k bytes
Initial space overhead: 80%
Initial max overhead: 500%
Initial heap increment: 992k bytes
Initial allocation policy: 0
Hello OCaml World!
```

If you get an error that `libasmrund.a` is not found, then this is probably
because you're using OCaml 4.00 and not 4.01.  It's only installed by default
in the very latest version, which you should be using via the `4.01.0dev+trunk`
OPAM switch.

</tip>

## Summarising the file extensions

Here are some of the intermediate files generated by `ocamlc`:

Extension  Purpose
---------  -------
.ml        Source files for compilation unit module implementations.
.mli       Source files for compilation unit module interfaces. If missing, generated from the `.ml` file.
.cmi       Compiled module interface from a corresponding `.mli` source file.
.cmo       Compiled bytecode object file of the module implementation.
.cma       Library of bytecode object files packed into a single file.
.o         C source files are compiled into native object files by the system `cc`.

To obtain a bytecode executable, you need to compile a set of `cmo` object files, and then link them into an executable

### The `ocamlopt` native code compiler

Extension  Purpose
---------  -------
.cmi       Compiled module interface from a corresponding `.mli` source file. (_avsm_: this is not compatible with the ocamlc version iirc)
.o         Compiled native object file of the module implementation.
.cmx       Contains extra information for linking and cross-module optimization of the object file.
.cmxa/.a   Library of `cmx` and `o` units, stored in the `cmxa` and `a` files respectively.


