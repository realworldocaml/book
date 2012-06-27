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

### Reusing field names

Defining records with the same field names can be problematic.
Consider the following types, which represent messages a server might
receive from a client.  The two messages are the `logon`, which occurs
when a new client connects for the first time and includes the
identity of the user connecting and credentials used for
authentication, and a `heartbeat`, which each client generates
periodically to demonstrate to the server that the client is alive and
connected, and includes a status message.  Both these messages include
a session id and the time the message was generated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type heartbeat =
    { session_id: string;
      time: Time.t;
      status_message: string;
    };;
# type logon =
    { session_id: string;
      time: Time.t;
      user: string;
      credentials: string;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The fact that we reused the same field name between `heartbeat` and
`logon` causes trouble when we try to construct a `heartbeat`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_heartbeat ~session_id ~status_message =
     { time = Time.now (); session_id; status_message }
  ;;
    Characters 56-106:
       { time = Time.now (); session_id; status_message }
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The record field label status_message belongs to the type heartbeat
       but is mixed here with labels of type logon
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that the declaration of `logon` shadowed the
corresponding fields of `heartbeat`, so they can no longer be
referenced directly.  There are two common solutions to this problem.
The first is to add a prefix to each field name to make it unique.
Thus, we could define `heartbeat` and `logon` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type heartbeat =
    { heartbeat_session_id: string;
      heartbeat_time: Time.t;
      heartbeat_status_message: string;
    };;
# type logon =
    { logon_session_id: string;
      logon_time: Time.t;
      logon_client_addr: Unix.Inet_addr.t;
      logon_client: int;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This eliminates the collisions and is simple enough, but it requires
you to pick awkward names for your record fields, and adds needless
repetition and verbosity, both to your type declaration and more
importantly to every subsequent use of the fields in question.

Another approach is to mint a module for each type.  This is actually
a broadly useful idiom, providing for each type a namespace within
which to put related values.  Using this style we would write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Heartbeat = struct
    type t =
      { session_id: string;
        time: Time.t;
        status_message: string;
      }
  end
  module Logon = struct
    type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, we can write a function for creating a heartbeat as follows

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_heartbeat ~session_id ~status_message =
      { Heartbeat.time = Time.now ();
        Heartbeat.session_id; Heartbeat.status_message };;
val create_heartbeat :
  session_id:string -> status_message:string -> Heartbeat.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do need to use the module name to qualify the field names, because
this code is written outside of the module where the record is
defined.  This qualification is only required needed for one record
field, so this can be written more tersely as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_heartbeat ~session_id ~status_message =
      { Heartbeat. time = Time.now (); session_id; status_message };;
val create_heartbeat :
  session_id:string -> status_message:string -> Heartbeat.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For functions defined within the module, which is a quite common
pattern, the module qualification goes away entirely.

### Functional updates

One common operation on a record is to create a new record that
differs from an existing record in only a subset of the fields.  For
example, imagine that you had a record for keeping track of
information about a given client, including when the last heartbeat
was received from that client.  The following defines a type for
representing this information, as well as a function for updating the
client information when a new heartbeat arrives.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type client_info =
   { addr: Unix.Inet_addr.t;
     port: int;
     user: string;
     credentials: string;
     last_heartbeat_time: Time.t;
   };;
# let register_heartbeat t hb =
    { addr = t.addr;
      port = t.port;
      user = t.user;
      credentials = t.credentials;
      last_heartbeat_time = hb.Heartbeat.time;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is fairly verbose, given that there's only one field that we
actually want to change.  We can use OCaml's functional update syntax
to do this more tersely.  The syntax of a functional update is as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
{ <record-value> with <field> = <value>;
                      <field> = <value>;
                      ...
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thus, we can rewrite `register_heartbeat` as follows

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let register_heartbeat t hb =
    { t with last_heartbeat_time = hb.Heartbeat.time };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Obviously, there is more benefit the larger the record is.

One issue with functional updates is that they make your code
independent of the identity of the other fields in the record.  This
is often the right choice, but it does mean that if you change the
definition of your record to have more fields, the type system will
not prompt you to reconsider whether your update code should affect
those fields.  Consider what happens if we decided to add a field for
the last status message received.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type client_info =
   { addr: Unix.Inet_addr.t;
     port: int;
     user: string;
     credentials: string;
     last_heartbeat_time: Time.t;
     last_status_message: string;
   };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With `register_heartbeat` handling all fields explicitly, the compiler
will warn you that you need to handle this new field.  But the version
using a functional update continues to compile as is, even though in
truth, it should now behave as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let register_heartbeat t hb =
    { t with last_heartbeat_time = hb.Heartbeat.time;
             last_status_message = hb.Heartbeat.status_message;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The lesson here is that when you use a functional update, you should
be confident that the full effect of the update will be registered by
updating just the fields in question, even if the record changes
later.

### Mutable fields

Records are by default immutable, meaning that while you can do
functional updates, you can't normally change the value of a record
field.  It is, however, possible to declare a field as being mutable,
as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { mutable x: int;
             y: float;
           };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We'll talk about mutability more in chapter {{{MUTABILITY}}}

### First-class fields

Consider the following function for extracting the usernames from a
list of `Logon` messages.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let get_users logons =
     List.map logons ~f:(fun x -> x.Logon.user);;
  val get_hostnames : Logon.t list -> string list = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we wrote a small function `(fun x -> x.Logon.user)` to access
the `user` field.  This kind of accessor function is a common enough
pattern that that it would be convenient to generate them
automatically.  OCaml doesn't have any direct support for this, but
the `fieldslib` syntax extension that ships with `Core` does just
that.

`fieldslib` is invoked by putting the `with fields` annotation at the
end of the declaration of a record type.  So, for example, we could
have defined `Logon` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Logon = struct
    type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
    with fields
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given that definition, we can use the function `Logon.user` to extract
the user field from a logon message.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let get_users logons = List.map logons ~f:Logon.user;;
  val get_hostnames : Logon.t list -> string list = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to generating field accessor functions, `fieldslib` also
creates a sub-module called `Fields` that contains a first class
representative of each field, in the form of a value of type
`Field.t`.  A `Field.t` bundles up the following functionality of a
record filed:

* The name of the field as a string
* The ability to extract the field
* The ability to do a functional update of that field
* The (optional) ability to set the record field, which is present
  only if the field is mutable.

We can use these first class fields to do things like write a generic
function for displaying a record field.  The function `show_field`
takes three arguments: the `Field.t`, a function for converting the
contents of the field in question to a string, and the record type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let show_field field to_string record =
     sprintf "(%s: %s)" (Field.name field) (Field.get field record |! to_string);;
val show_field : ('a, 'b) Core.Std.Field.t -> ('b -> string) -> 'a -> string =
  <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And here, we show how these functions can be used to display
individual fields.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let logon = { Logon.
                session_id = "26685";
                time = Time.now ();
                user = "yminsky";
                credentials = "Xy2d9W"; }
  ;;
# show_field Logon.Fields.user Fn.id logon;;
- : string = "(user: yminsky)"
# extract Logon.Fields.time Time.to_string logon;;
- : string = "(time: 2012-06-26 18:44:13.807826)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`fieldslib` also provides higher-level operators, like `Fields.fold`
and `Fields.iter`, which let you operate over all of the fields of a
record.  For example, here's a function that uses show_field to print
out a representation of a complete record.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let print_logon logon =
    let print to_string field =
      printf "%s\n" (show_field field to_string logon)
    in
    Logon.Fields.iter
      ~session_id:(print Fn.id)
      ~time:(print Time.to_string)
      ~user:(print Fn.id)
      ~credentials:(print Fn.id)
  ;;
val print_logon : Logon.t -> unit = <fun>
# print_logon logon;;
(session_id: 26685)
(time: 2012-06-26 18:44:13.807826)
(user: yminsky)
(credentials: Xy2d9W)
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The advantage of this approach is that when the definition of `Logon`
changes, `iter` will change along with it, prompting you to fix
`print_logon` to match.

These field-based iterators are useful for a variety of practical
programming tasks, from building validation functions to scaffolding
the definition of a web-form based on a record type.

### Sharing common sub-structures

## Variants


## More advanced declarations

### Polymorphic types

### Recursive types
