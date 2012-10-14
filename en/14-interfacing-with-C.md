# Interfacing with C

Much of the static type information contained within an OCaml program is
checked and discarded at compile time, leaving a much simpler *runtime*
representation for values.  OCaml adopts a uniform memory representation for
such memory values, and understanding this layout is important to writing fast
programs and interfacing with C libraries.

Let's first explain the memory layout of an OCaml program, and then move
onto how to interface it with other systems after that.

## Understanding the Heap

Every OCaml value starts with a single word (either 32- or 64-bit) that
represents either an unboxed integer or a pointer. If the lowest bit of the
word is non-zero, then the value is an integer.  Several OCaml types map onto
such integers, including `bool`, `int`, the empty list, `unit`, and some
variants.  Integers are the only unboxed runtime values in OCaml, and are thus
cheap to allocate and manipulate.  A `value` containing a pointer is stored
unmodified, and can be followed just like a C pointer.

Runtime values that are more complex than integers are stored in OCaml *blocks*.
An OCaml block consists of a header word followed by a variable number of data
words.   The header word is broken up as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
(TODO draw this properly)
+------------------------+-------+----------+----------+----------+----
| size of block in words |  col  | tag byte | value[0] | value[1] | ...
+------------------------+-------+----------+----------+----------+----
 <-either 22 or 54 bits-> <2 bit> <--8 bit-->
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The size part records the length of the block in words. Note that it is limited
to 22-bits on 32-bit platforms, which is the reason why OCaml strings are
limited to 16MB.  If you need bigger strings, then either switch to a 64-bit
host, or use the `Bigarray` module.  The 2-bit color is used by the garbage
collector to keep track of its scanning, and we will discuss it more later.
The tag byte is more important now, as it encodes the format of the subsequent
words.

(_avsm_: pointers to blocks actually point 4/8 bytes into it, for some efficiency
reason that I cannot recall right now).

(_avsm_: is the string padding trick to guarantee nul-termination worth mentioning
here? it seems more on topic for the C interface section)

<note>
<title>Why are OCaml integers missing a bit?</title>

Since the lowest bit of an OCaml value is reserved, native OCaml integers have
a maximum allowable length of 31- or 63-bits, depending on the host
architecture. The rationale for reserving the lowest bit is for efficiency.
Pointers always point to word-aligned addresses, and so their lower bits are
normally zero. By setting the lower bit to a non-zero value for integers, the
garbage collector can simply iterate over every header tag to distinguish
integers from pointers.  This reduces the garbage collection overhead on the
overall program.

</note>

## Linking to C libraries

 
