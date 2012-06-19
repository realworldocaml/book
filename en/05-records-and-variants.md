# Records and Variants

One of OCaml's best features is its system for declaring new
datatypes.  This system is both highly concise and very expressive,
allowing you to design types that very precisely match your
intentsions.

Two key elements of this system are _records_ and _variants_.  We
presented records and variants briefly in chapter {{{GUIDEDTOUR}}},
but in this section, we'll cover them in more depth, and presenting
some more realistic examples.  We'll start with records.

## Records

A record represents a collection of values stored together as one,
where each component is identified by a different field name.  The
basic syntax for a record type declaration is as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
type <record-name> =
  { <field-name> : <type-name> ;
    <field-name> : <type-name> ;
    ...
  }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So, for example, we could declare a type that summarizes information
about a given host as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type host_info =
    { hostname   : string;
      os_name    : string;
      os_release : string;
      cpu_arch   : string;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can construct an instance of this record type just as easily.
In the following code, we use the `Shell` module from `Core_extended`
to dispatch commands to the shell to extract the information we need
about the computer we're running on.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# open Core_extended.Std;;
# let my_host = { hostname   = Shell.sh_one "hostname";
                  os_name    = Shell.sh_one "uname -s";
                  os_release = Shell.sh_one "uname -r";
                  cpu_arch   = Shell.sh_one "uname -p";
                };;
val my_host : host_info =
  {hostname = "Yarons-MacBook-Air.local"; os_name = "Darwin";
   os_release = "11.4.0"; cpu_arch = "i386"}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And once we have a record value in hand, we can extract elements from
the record field using dot-notation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# my_host.cpu_arch;;
- : string = "i386"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Pattern matching

Another way of getting information out of a record is using a pattern
match.  Thus, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string { hostname = h; os_name = os; 
                            os_release = r; cpu_arch = c } =
       sprintf "%s (%s %s / %s)" h os r c;;
    val host_info_to_string : host_info -> string = <fun>
# host_info_to_string my_host;;
- : string = "Yarons-MacBook-Air.local (Darwin 11.4.0 / i386)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we've used a pattern match in the argument to capture the
relevant record fields.  We didn't need to use a match statement
because pattern matches of a record are _irrefutable_, meaning that a
a single pattern is guaranteed to match all values of the record type
in question.  In other words, all of the requirements imposed by the
pattern match are the enforced by the compiler, so that if your code
compiles, you can't have a runtime failure due to a failed match.
This is in contrast to lists, where it's easy to write a pattern match
that fails at runtime.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let first_plus_second (x :: y :: _) = x + y;;
Characters 22-43:
  let first_plus_second (x :: y :: _) = x + y;;
                        ^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val first_plus_second : int list -> int = <fun>
# first_plus_second [];;
Exception: (Match_failure "" 54 -135).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With records, this kind of failure is not possible, and so this
exhaustiveness check is not relevant.

OCaml has a different kind of exhaustiveness check, though, that does
matter for records.  In particular, OCaml offers a warning for missing
fields in a record pattern.  With that warning turned on, the
following code will return a warning about an inexhaustive match.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string
      { hostname = h; os_name = os; os_release = r } =
    sprintf "%s (%s %s)" h os r;;
    Characters 30-76:
        { hostname = h; os_name = os; os_release = r } =
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 9: the following labels are not bound in this record pattern:
cpu_arch
Either bind these labels explicitly or add `; _' to the pattern.
val host_info_to_string : host_info -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that, unlike our previous example, ignoring this warning won't
lead to a runtime error.  The warning is nonetheless worthwhile,
because it gives you an opportunity to notice that there is some data
that you're ignoring, which is often important.

Sometimes, we do want to ignore extra fields, and we can explicitly do
so by adding `_`, which silently matches all remaining fields, and
makes the error go away.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string
      { hostname = h; os_name = os; os_release = r; _ } =
    sprintf "%s (%s %s)" h os r;;
    val host_info_to_string : host_info -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Field punning

We can also use _field punning_ to bind the fields of a record to
variables of the same name.  Thus, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string { hostname; os_name; os_release; cpu_arch } =
     sprintf "%s (%s %s / %s)" hostname os_name os_release cpu_arch;;
  val host_info_to_string : host_info -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Field punning can also be used to construct a record.  For example, we
could have rewritten our code to generate the host info record as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let my_host =
    let hostname   = Shell.sh_one "hostname" in
    let os_name    = Shell.sh_one "uname -s" in
    let os_release = Shell.sh_one "uname -r" in
    let cpu_arch   = Shell.sh_one "uname -p" in
    { hostname; os_name; os_release; cpu_arch };;
val my_host : host_info =
  {hostname = "Yarons-MacBook-Air.local"; os_name = "Darwin";
   os_release = "11.4.0"; cpu_arch = "i386"}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, the use of field punning doesn't really clarify the
code.  But it can be quite useful when the computation of the
elememnts of the records is complex.  One common pattern is to produce
a function for constructing a record from labeled arguments.  In the
following example, we have a simple constructor, which as part of the
construction converts the hostname to lowercase.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_host_info ~hostname ~os_name ~os_release ~cpu_arch =
    let hostname = String.lowercase hostname in
    { hostname; os_name; os_release; cpu_arch };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This looks considerably better than it would without field punning:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
let create_host_info ~hostname ~os_name ~os_release ~cpu_arch =
    let hostname = String.lowercase hostname in
    { hostname = hostname ; os_name = os_name;
      os_release = os_release; cpu_arch = cpu_arch };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Together, labeled arguments, field names, and field and label punning,
encourage a style where you carry names through your codebase, across
different functions and modules.  This is generally good practice,
since it encourages uniform naming choices, which makes it easier for
someone new to the code to know what to expect.

### Collision of field names

One problem you can run into with record declarations is what happens
when you define two record types that share the same field names.
Consider the following example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type vec3d = { x: float; y: float; z: float };;
# type vec2d = { x: float; y: float };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, see what happens when we try to write a function that operates
over a vec3d.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let mag3d p = 
     let sqr x = x *. x in
     sqrt (sqr p.x +. sqr p.y +. sqr p.z);;
    Characters 79-80:
       sqrt (sqr p.x +. sqr p.y +. sqr p.z);;
                                       ^
Error: This expression has type vec2d but an expression was expected of type
         vec3d
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Essentially what's happened here is that the declaration of `vec3d`
has shadowed two of the record fields of `vec3d`, and now there's no
natural way to access them.

There are two standard idioms for dealing with this issue.  The first
is to add a prefix to each field name to make it unique.  Thus, we
might write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type vec3d = { v3_x: float; v3_y: float; v3_z: float };;
type vec3d = { v3_x : float; v3_y : float; v3_z : float; }
# type vec2d = { v2_x: float; v2_y: float };;
type vec2d = { v2_x : float; v2_y : float; }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another approach is to mint a module for each type.  This is actually
a broadly useful idiom, providing for each type a namespace within
which to put values related to the type that the module is about.
Thus, we might write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Vec3d = struct
    type t = { x: float; y: float; z: float }
    let mag p = 
       let sqr x = x *.x in
       sqrt (sqr p.x +. sqr p.y +. sqr p.z)
  end;;
          module Vec3d :
  sig type t = { x : float; y : float; z : float; } val mag : t -> float end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now use this function as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Vec3d.mag { Vec3d.x = 3.; Vec3d.y = 0.; Vec3d.z = -4. };;
- : float = 5.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that now we need to use the module name to qualify the field
name, because this code is written outside of the module where the
record is defined.  This qualification is actually only needed for one
record field, so this can be written more tersely as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Vec3d.mag { Vec3d. x = 3.; y = 0.; z = -4. };;
- : float = 5.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This works for record construction and pattern matching, so we can
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let flip_x { Vec3d. x; y; z } = { Vec3d. x = (-.x);y;z };;
val flip_x : Vec3d.t -> Vec3d.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Functional updates

A common idiom in functional programming is to take an existing record
field and change one or two of the fields.  For example, in the
`flip_x` example above, there was really only one field we wanted to
change, but we had to list all of the field names to construct the new
record.


### Polymorphic records

### Mutable record fields

### First-class fields

### Sharing common sub-structures

## Variants
