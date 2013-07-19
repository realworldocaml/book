# Records

One of OCaml's best features is its concise and expressive system for
declaring new datatypes, and records are a key element of that system.
We discussed records briefly in [xref](#a-guided-tour), but this
chapter will go into more depth, covering the details of how records
work, as well as advice on how to use them effectively in your
software designs.

A record represents a collection of values stored together as one,
where each component is identified by a different field name.  The
basic syntax for a record type declaration is as follows.

```frag
((typ ocamlsyntax)(name records/record.syntax))
```

Note that record field names must start with a lower-case letter.

Here's a simple example, a `host_info` record that summarizes
information about a given computer.

```frag
((typ ocamltop)(name records/main.topscript)(part 0))
```

We can construct a `host_info` just as easily.  The following code
uses the `Shell` module from `Core_extended` to dispatch commands to
the shell to extract the information we need about the computer we're
running on, and the `Time.now` call from Core's `Time` module.

```frag
((typ ocamltop)(name records/main.topscript)(part 1))
```

You might wonder how the compiler inferred that `my_host` is of type
`host_info`.  The hook that the compiler uses in this case to figure
out the type is the record field name.  Later in the chapter, we'll
talk about what happens when there is more than one record type in
scope with the same field name.

Once we have a record value in hand, we can extract elements from the
record field using dot-notation.

```frag
((typ ocamltop)(name records/main.topscript)(part 2))
```

When declaring an OCaml type, you always have the option of
parameterizing it by a polymorphic type.  Records are no different in
this regard.  So, for example, here's a type one might use to
timestamp arbitrary items.

```frag
((typ ocamltop)(name records/main.topscript)(part 3))
```

We can then write polymorphic functions that operate over this
parameterized type.

```frag
((typ ocamltop)(name records/main.topscript)(part 4))
```

## Patterns and exhaustiveness

Another way of getting information out of a record is by using a
pattern match, as in the definition of `host_info_to_string` below.

```frag
((typ ocamltop)(name records/main.topscript)(part 5))
```

Note that the pattern that we used had only a single case, rather than
using several cases separated by `|`'s.  We needed only one pattern
because record patterns are _irrefutable_, meaning that a record
pattern match will never fail at runtime.  This makes sense, because
the set of fields available in a record is always the same.  In
general, patterns for types with a fixed structure, like records and
tuples, are irrefutable, unlike types with variable structure like
lists and variants.

Another important characteristic of record patterns is that they don't
need to be complete; a pattern can mention only a subset of the fields
in the record.  This can be convenient, but it can also be error
prone.  In particular, this means that when new fields are added to
the record, code that should be updated to react to the presence of
those new fields will not be flagged by the compiler.

As an example, imagine that we wanted to add a new field to our
`host_info` record called `os_release`, as shown below.

```frag
((typ ocamltop)(name records/main.topscript)(part 6))
```

The code for `host_info_to_string` would continue to compile without
change.  In this particular case, it's pretty clear that you might
want to update `host_info_to_string` in order to include `os_release`,
and it would be nice if the type system would give you a warning about
the change.

Happily, OCaml does offer an optional warning for missing fields in a
record pattern.  With that warning turned on (which you can do in the
toplevel by typing `#warnings "+9" `), the compiler will warn about
the missing field.

```frag
((typ ocamltop)(name records/main.topscript)(part 7))
```

We can disable the warning for a given pattern by explicitly
acknowledging that we are ignoring extra fields.  This is done by
adding an underscore to the pattern, as shown below.

```frag
((typ ocamltop)(name records/main.topscript)(part 8))
```

It's a good idea to enable the warning for incomplete record matches,
and to explicitly disable it with an `_` where necessary.

<note> <title> Compiler warnings </title>

The OCaml compiler is packed full of useful warnings that can be
enabled and disabled separately.  For example, we could have found out
about warning 9, which was discussed above, as follows:

```frag
((typ console)(name records/warn_help.out))
```

You should think of OCaml's warnings as a powerful set of optional
static analysis tools, and eagerly enable them in your build
environment.  You don't typically enable all warnings, but the
defaults that ship with the compiler are pretty good.

The warnings used for building the examples in this book are specified
with the following flag.

```
-w @A-4-33-41-42-43-34-44
```

The syntax of this can be found by running `ocaml -help`, but this
particular invocation turns on all warnings as errors, disabling only
the numbers listed explicitly after the `A`.

Treating warnings as errors (_i.e._, making OCaml fail to compile any
code that triggers a warning) is good practice, since without it,
warnings are too often ignored during development.  When preparing a
package for distribution, however, this is a bad idea, since the list
of warnings may grow from one release of the compiler to another, and
so this may lead your package to fail to compile on newer compiler
releases.

</note>


## Field punning

When the name of a variable coincides with the name of a record field,
OCaml provides some handy syntactic shortcuts.  For example, the
pattern in the following function binds all of the fields in question
to variables of the same name.  This is called _field punning_.

```frag
((typ ocamltop)(name records/main.topscript)(part 9))
```

Field punning can also be used to construct a record.  Consider the
following code for generating a `host_info` record.

```frag
((typ ocamltop)(name records/main.topscript)(part 10))
```

In the above code, we defined variables corresponding to the record
fields first, and then the record declaration itself simply listed the
fields that needed to be included.

You can take advantage of both field punning and label punning when
writing a function for constructing a record from labeled arguments,
as shown below.

```frag
((typ ocamltop)(name records/main.topscript)(part 11))
```

This is considerably more concise than what you would get without
punning at all.

```frag
((typ ocamltop)(name records/main.topscript)(part 12))
```

Together, labeled arguments, field names, and field and label punning,
encourage a style where you propagate the same names throughout your
code-base.  This is generally good practice, since it encourages
consistent naming, which makes it easier to navigate the source.

## Reusing field names

Defining records with the same field names can be problematic.  Let's
consider a simple example: building types to represent the protocol
used for a logging server.

We'll describe three message types: `log_entry`, `heartbeat` and
`logon`.  The `log_entry` message is used to deliver a log entry to
the server; the `logon` message is sent to initiate a connection, and
includes the identity of the user connecting and credentials used for
authentication; and the `heartbeat` message is periodically sent by
the client to demonstrate to the server that the client is alive and
connected.  All of these messages include a session id and the time
the message was generated.

```frag
((typ ocamltop)(name records/main.topscript)(part 13))
```

Reusing field names can lead to some ambiguity.  For example, if we
want to write a function to grab the session_id from a record, what
type will it have?

```frag
((typ ocamltop)(name records/main.topscript)(part 14))
```

In this case, OCaml just picks the most recent definition of that
record field.  We can force OCaml to assume we're dealing with a
different type (say, a `heartbeat`) using a type annotation.

```frag
((typ ocamltop)(name records/main.topscript)(part 15))
```

While it's possible to resolve ambiguous field names using type
annotations, the ambiguity can be a bit confusing.  Consider the
following functions for grabbing the session id and status from a
heartbeat.

```frag
((typ ocamltop)(name records/main.topscript)(part 16))
```

Why did the first definition succeed without a type annotation and the
second one fail?  The difference is that in the first case, the
type-checker considered the `status_message` field first and thus
concluded that the record was a `heartbeat`.  When the order was
switched, the `session_id` field was considered first, and so that
drove the type to be considered to be a `logon`, at which point
`t.status_message` no longer made sense.

We can avoid this ambiguity altogether, either by using
non-overlapping field names or, more generally, by minting a module
for each type.  Packing types into modules is a broadly useful idiom
(and one used quite extensively by Core), providing for each type a
name-space within which to put related values.  When using this style,
it is standard practice to name the type associated with the module
`t`.  Using this style we would write:

```frag
((typ ocamltop)(name records/main.topscript)(part 17))
```

Now, our log-entry-creation function can be rendered as follows.

```frag
((typ ocamltop)(name records/main.topscript)(part 18))
```

The module name `Log_entry` is required to qualify the fields, because
this function is outside of the `Log_entry` module where the record
was defined.  OCaml only requires the module qualification for one
record field, however, so we can write this more concisely.  Note that
we are allowed to insert whitespace between the module-path and the
field name.

```frag
((typ ocamltop)(name records/main.topscript)(part 19))
```

This is not restricted to constructing a record; we can use the same
trick when pattern matching.

```frag
((typ ocamltop)(name records/main.topscript)(part 20))
```

When using dot-notation for accessing record fields, we can qualify
the field by the module directly.  

```frag
((typ ocamltop)(name records/main.topscript)(part 21))
```

The syntax here is a little surprising when you first encounter it.
The thing to keep in mind is that the dot is being used in two ways:
the first dot is a record field access, with everything to the right
of the dot being interpreted as a field name; the second dot is
accessing the contents of a module, referring to the record field
`important` from within the module `Log_entry`.  The fact that
`Log_entry` is capitalized and so can't be a field name is what
disambiguates the two uses.

For functions defined within the module where a given record is
defined, the module qualification goes away entirely.

## Functional updates

Fairly often, you will find yourself wanting to create a new record
that differs from an existing record in only a subset of the fields.
For example, imagine our logging server had a record type for
representing the state of a given client, including when the last
heartbeat was received from that client.  The following defines a type
for representing this information, as well as a function for updating
the client information when a new heartbeat arrives.

```frag
((typ ocamltop)(name records/main.topscript)(part 22))
```

This is fairly verbose, given that there's only one field that we
actually want to change, and all the others are just being copied over
from `t`.  We can use OCaml's _functional update_ syntax to do this
more tersely.  The syntax of a functional update is as follows.

```frag
((typ ocamlsyntax)(name records/functional_update.syntax))
```

The purpose of the functional update is to create a new record based
on an existing one, with a set of field changes layered on top.

Given this, we can rewrite `register_heartbeat` more concisely.

```frag
((typ ocamltop)(name records/main.topscript)(part 23))
```

Functional updates make your code independent of the identity of the
fields in the record that are not changing.  This is often what you
want, but it has downsides as well.  In particular, if you change the
definition of your record to have more fields, the type system will
not prompt you to reconsider whether your update code should affect
those fields.  Consider what happens if we decided to add a field for
the status message received on the last heartbeat.

```frag
((typ ocamltop)(name records/main.topscript)(part 24))
```

The original implementation of `register_heartbeat` would now be
invalid, and thus the compiler would warn us to think about how to
handle this new field.  But the version using a functional update
continues to compile as is, even though it incorrectly ignores the new
field.  The correct thing to do would be to update the code as
follows.

```frag
((typ ocamltop)(name records/main.topscript)(part 25))
```

## Mutable fields

Like most OCaml values, records are immutable by default.  You can,
however, declare individual record fields as mutable.  In the
following, we've made the last two fields of `client_info` mutable.

```frag
((typ ocamltop)(name records/main.topscript)(part 26))
```

We then use the `<-` operator for actually changing the state.  The
side-effecting version of `register_heartbeat` would be written as
follows.

```frag
((typ ocamltop)(name records/main.topscript)(part 27))
```

Note that mutable assignment, and thus the `<-` operator, is not
needed for initialization, because all fields of a record, including
mutable ones, are specified when the record is created.

OCaml's policy of immutable-by-default is a good one, but imperative
programming does have its place.  We'll discuss more about how (and
when) to use OCaml's imperative features in
[xref](#imperative-programming).

## First-class fields

Consider the following function for extracting the usernames from a
list of `Logon` messages.

```frag
((typ ocamltop)(name records/main.topscript)(part 28))
```

Here, we wrote a small function `(fun x -> x.Logon.user)` to access
the `user` field.  This kind of accessor function is a common enough
pattern that it would be convenient to generate them automatically.
The `fieldslib` syntax extension that ships with `Core` does just
that.

The `with fields` annotation at the end of the declaration of a record
type will cause the extension to be applied to a given type
declaration.  So, for example, we could have defined `Logon` as
follows.

```frag
((typ ocamlrawtop)(name records/main-29.rawscript))
```

Note that this will generate a _lot_ of output, because `fieldslib`
generates a large collection of helper functions for working with
record fields.  We'll only discuss a few of these; you can learn about
the remainder from the documentation that comes with `fieldslib`.

One of the functions we obtain is `Logon.user`, which we can use to
extract the user field from a logon message.

```frag
((typ ocamltop)(name records/main.topscript)(part 30))
```

In addition to generating field accessor functions, `fieldslib` also
creates a sub-module called `Fields` that contains a first-class
representative of each field, in the form of a value of type
`Field.t`.  The `Field` module provides the following functions:

* `Field.name`, which returns the name of a field
* `Field.get`, which returns the content of a field
* `Field.fset`, which does a functional update of a field
* `Field.setter`, which returns `None` if the field is not mutable or
  `Some f` if it is, where `f` is a function for mutating that field.

A `Field.t` has two type parameters: the first for the type of the
record, and the second for the type of the field in question.  Thus,
the type of `Logon.Fields.session_id` is `(Logon.t, string) Field.t`,
whereas the type of `Logon.Fields.time` is `(Logon.t, Time.t)
Field.t`.  Thus,  if you call `Field.get` on `Logon.Fields.user`,
you'll get a function for extracting the `user` field from a `Logon.t`.

```frag
((typ ocamltop)(name records/main.topscript)(part 31))
```

Thus, first parameter of the `Field.t` corresponds to the record you
pass to `get`, and the second argument corresponds to the value
contained in the field, which is also the return type of `get`.

The type of `Field.get` is a little more complicated than you might
naively expect from the above, as you can see below.

```frag
((typ ocamltop)(name records/main.topscript)(part 32))
```

The type is `Field.t_with_perm` rather than a simple `Field.t` because
fields have a notion of access control associated with them because
there are some special cases where we may expose the ability to read a
field but not the ability to do a functional update.

We can use first-class fields to do things like write a generic
function for displaying a record field.

```frag
((typ ocamltop)(name records/main.topscript)(part 33))
```

This takes three arguments: the `Field.t`, a function for converting
the contents of the field in question to a string, and a record from
which the field can be grabbed.

Here's an example of `show_field` in action.

```frag
((typ ocamltop)(name records/main.topscript)(part 34))
```

As a side note, the above is our first use of the `Fn` module (short
for "function") which provides a collection of useful primitives for
dealing with functions.  `Fn.id` is the identity function.

`fieldslib` also provides higher-level operators, like `Fields.fold`
and `Fields.iter`, which let you iterate over all the fields of a
record.  So, for example, in the case of `Logon.t`, the field iterator
has the following type.

```frag
((typ ocamltop)(name records/main.topscript)(part 35))
```

This is a bit daunting to look at, largely because of the access
control markers, but the structure is actually pretty simple.  Each
labeled argument is a function that takes a first-class field of the
necessary type as an argument.  Note that `iter` passes each of these
callbacks the `Field.t`, not the contents of the specific record
field.  The contents of the field, though, can be looked up using the
combination of the record and the `Field.t`.  

Now, let's use `Logon.Fields.iter` and `show_field` to print out all
the fields of a `Logon` record.

```frag
((typ ocamltop)(name records/main.topscript)(part 26))
```

One nice side effect of this approach is that it helps you adapt your
code when the fields of a record change.  If you were to add a field
to `Logon.t`, the type of `Logon.Fields.iter` would change along with
it, acquiring a new argument.  Any code using `Logon.Fields.iter`
won't compile until it's fixed to take this new argument into account.

This exhaustion guarantee is a valuable one.  Field iterators are
useful for a variety of record-related tasks, from building record
validation functions to scaffolding the definition of a web-form from
a record type, and such applications can benefit from the guarantee
that all fields of the record type in question have been considered.

