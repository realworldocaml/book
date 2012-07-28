# Records and Variants

One of OCaml's best features is its concise and expressive system for
declaring new datatypes.  Two key elements of that system are
_records_ and _variants_, both of which we discussed briefly in
chapter {{{GUIDEDTOUR}}}.  In this chapter we'll cover records and
variants in more depth, showing some of the more advanced features, as
well as discuss how to use them effectively in your software designs.

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

Here's a simple example, a `host_info` record that summarizes
information about a given computer.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type host_info =
    { hostname   : string;
      os_name    : string;
      os_release : string;
      cpu_arch   : string;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can construct a `host_info` just as easily.  The following code
uses the `Shell` module from `Core_extended` to dispatch commands to
the shell to extract the information we need about the computer we're
running on.

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

Once we have a record value in hand, we can extract elements from the
record field using dot-notation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# my_host.cpu_arch;;
- : string = "i386"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Pattern matching

Another way of getting information out of a record is by using a
pattern match.  In the following example, we use a pattern-match on
the argument to a function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string { hostname = h; os_name = os;
                            os_release = r; cpu_arch = c } =
       sprintf "%s (%s %s / %s)" h os r c;;
    val host_info_to_string : host_info -> string = <fun>
# host_info_to_string my_host;;
- : string = "Yarons-MacBook-Air.local (Darwin 11.4.0 / i386)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice that we didn't need a match statement because we used only a
single pattern to match all possible records of type `host_info`.
This works because record patterns are _irrefutable_, meaning that a
single pattern is guaranteed to match any value of the type in
question.

In other words, an irrefutable pattern is one where all of the
requirements imposed by the pattern match are enforced by the type
system, so that code that compiles will never have a runtime failure
due to a failed match.  This is in contrast to, say, lists, where it's
easy to write a pattern match that fails at runtime.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let first_plus_second (x :: y :: _) = x + y;;
Characters 22-43:
  let first_plus_second (x :: y :: _) = x + y;;
                        ^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val first_plus_second : int list -> int = <fun>
# first_plus_second [5;3;6];;
- : int = 8
# first_plus_second [5];;
Exception: (Match_failure "" 1 22).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OCaml does offer an exhaustiveness check for record patterns, in
particular, a warning for missing fields in a record pattern.  With
that warning turned on (which you can do in the toplevel by typing
`#warnings "+9" `), the following code will complain about an
inexhaustive match.

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
lead to a runtime error.  The warning is nonetheless useful, because
it gives you an opportunity to notice that there is some data that
you're ignoring, which is often important.

We can disable the warning for a given pattern by explicitly
acknowledging that we are ignoring extra fields by adding `_` to the
pattern, as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string
      { hostname = h; os_name = os; os_release = r; _ } =
    sprintf "%s (%s %s)" h os r;;
    val host_info_to_string : host_info -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Field punning

When the name of a variable coincides with the name of a record field,
OCaml provides some handy syntactic shortcuts.  For example, the
pattern in the following function binds all of the fields in question
to variables of the same name.  This is called _field punning_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let host_info_to_string { hostname; os_name; os_release; cpu_arch } =
     sprintf "%s (%s %s / %s)" hostname os_name os_release cpu_arch;;
  val host_info_to_string : host_info -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Field punning can also be used to construct a record.  Consider the
following code for generating a `host_info` record.

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

In the above code, we defined variables corresponding to the record
fields first, and then the record declaration itself simply listed the
fields that needed to be included.

You can take advantage of both field punning and label punning when
writing a function for constructing a record from labeled arguments,
as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_host_info ~hostname ~os_name ~os_release ~cpu_arch =
    let hostname = String.lowercase hostname in
    { hostname; os_name; os_release; cpu_arch };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is considerably more concise than what you would get without
punning at all.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
let create_host_info ~hostname:hostname ~os_name:os_name
   ~os_release:os_release ~cpu_arch:cpu_arch =
    let hostname = String.lowercase hostname in
    { hostname = hostname ; os_name = os_name;
      os_release = os_release; cpu_arch = cpu_arch };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Together, labeled arguments, field names, and field and label punning,
encourage a style where you carry names through your codebase, across
different functions and modules.  This is generally good practice,
since it encourages consistent naming, which makes it easier for new
people to navigate your source.

### Reusing field names

Defining records with the same field names can be problematic.  Let's
consider a simple example: building types to represent the protocol
used for a logging server.  The following types represent messages a
server might receive from a client.

Below, the `log_entry` message is used to deliver a log entry to the
server for processing.  The `logon` message is sent when a client
initiates a connection, and includes the identity of the user
connecting and credentials used for authentication.  Finally, the
`heartbeat` message is periodically sent by the client to demonstrate
to the server that the client is alive and connected.  All of these
messages include a session id and the time the message was generated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type log_entry =
    { session_id: string;
      time: Time.t;
      important: bool;
      message: string;
    }
  type heartbeat =
    { session_id: string;
      time: Time.t;
      status_message: string;
    }
  type logon =
    { session_id: string;
      time: Time.t;
      user: string;
      credentials: string;
    }
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The fact that we reused field names will cause trouble when we try to
construct a message.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_log_entry ~session_id ~important message =
     { time = Time.now (); session_id; important; message }
  ;;
    Characters 75-129:
       { time = Time.now (); session_id; important; message }
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The record field label important belongs to the type log_entry
       but is mixed here with labels of type logon
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that the declaration of `logon` (and `heartbeat`)
shadowed some of the fields of `log_entry`.  As a result, the fields
`time` and `session_id` are assummed to be fields of `logon`, and
`important` and `message`, which were not shadowed, are assummed to be
a field of `log_entry`.  The compiler therefore complains that we're
trying to construct a record with fields from two different record
types.

There are two common solutions to this problem.  The first is to add a
prefix to each field name to make it unique.  Thus, we could define
`heartbeat` and `logon` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type log_entry =
    { log_entry_session_id: string;
      log_entry_time: Time.t;
      log_entry_important: bool;
      log_entry_message: string;
    }
  type heartbeat =
    { heartbeat_session_id: string;
      heartbeat_time: Time.t;
      heartbeat_status_message: string;
    }
  type logon =
    { logon_session_id: string;
      logon_time: Time.t;
      logon_user: string;
      logon_credentials: string;
    }
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This eliminates the collisions and is simple enough to do.  But it
leaves you with awkwardly named record fields, and adds needless
repetition and verbosity to your code.

Another approach is to mint a module for each type.  This is actually
a broadly useful idiom, providing for each type a namespace within
which to put related values.  Using this style we would write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Log_entry = struct
    type t =
      { session_id: string;
        time: Time.t;
        important: bool;
        message: string;
      }
  end
  module Heartbeat = struct
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

Now, our heartbeat-creation function can be rendered as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_log_entry ~session_id ~important message =
     { Log_entry.time = Time.now (); Log_entry.session_id;
       Log_entry.important; Log_entry.message }
  ;;
val create_log_entry :
  session_id:string -> important:bool -> string -> Log_entry.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The module name `Log_entry` is required to qualify the fields, because
this function is outside of the `Log_entry` module where the record
was defined.  OCaml only requires the module qualification for one
record field, however, so we can write this more concisely.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let create_log_entry ~session_id ~important message =
     { Log_entry. time = Time.now (); session_id; important; message }
  ;;
val create_log_entry :
  session_id:string -> important:bool -> string -> Log_entry.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For functions defined within the module where a given record is
defined, the module qualification goes away entirely.  And indeed, for
things like constructors, defining it within the module is often the
best solution.

### Functional updates

Fairly often, you will find yourself wanting to create a new record
that differs from an existing record in only a subset of the fields.
For example, imagine our logging server had a record type for
representing the state of a given client, including when the last
heartbeat was received from that client.  The following defines a type
for representing this information, as well as a function for updating
the client information when a new heartbeat arrives.

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
val register_heartbeat : client_info -> Heartbeat.t -> client_info = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is fairly verbose, given that there's only one field that we
actually want to change, and all the others are just being copied over
from `t`.  We can use OCaml's _functional update_ syntax to do this
more tersely.  The syntax of a functional update is as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
{ <record-value> with <field> = <value>;
                      <field> = <value>;
                      ...
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The purpose of the functional update is to create a new record based
on an existing one, with a set of field changes layered on top.

Given this, we can rewrite `register_heartbeat` more concisely.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let register_heartbeat t hb =
    { t with last_heartbeat_time = hb.Heartbeat.time };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functional updates make your code independent of the identity of the
fields in the record that are not changing.  This is often what you
want, but it has downsides as well.  In particular, if you change the
definition of your record to have more fields, the type system will
not prompt you to reconsider whether your update code should affect
those fields.  Consider what happens if we decided to add a field for
the status message received on the last heartbeat.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type client_info =
   { addr: Unix.Inet_addr.t;
     port: int;
     user: string;
     credentials: string;
     last_heartbeat_time: Time.t;
     last_heartbeat_status: string;
   };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The original implementation of `register_heartbeat` would now be
invalid, and thus the compiler would warn us to think about how to
handle this new field.  But the version using a functional update
continues to compile as is, even though it incorrectly ignores the new
field.  The correct thing to do would be to update the code as
follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let register_heartbeat t hb =
    { t with last_heartbeat_time   = hb.Heartbeat.time;
             last_heartbeat_status = hb.Heartbeat.status_message;
    };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The lesson here is that when you use a functional update, you should
be confident that the full effect of the update will be registered by
updating just the fields in question, even if the record changes
later.

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
automatically.  The `fieldslib` syntax extension that ships with
`Core` does just that.

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
val get_users : Logon.t list -> string list = <fun>
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
  only if the field is mutable.  We'll talk more about mutable record
  fields in chapter {{{MUTABILITY}}}.

We can use these first class fields to do things like write a generic
function for displaying a record field.  The function `show_field`
takes three arguments: the `Field.t`, a function for converting the
contents of the field in question to a string, and the record type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let show_field field to_string record =
     sprintf "%s: %s" (Field.name field) (Field.get field record |! to_string);;
val show_field : ('a, 'b) Field.t -> ('b -> string) -> 'a -> string =
  <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here's an example of `show_field` in action.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let logon = { Logon.
                session_id = "26685";
                time = Time.now ();
                user = "yminsky";
                credentials = "Xy2d9W"; }
  ;;
# show_field Logon.Fields.user Fn.id logon;;
- : string = "user: yminsky"
# show_field Logon.Fields.time Time.to_string logon;;
- : string = "time: 2012-06-26 18:44:13.807826"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`fieldslib` also provides higher-level operators, like `Fields.fold`
and `Fields.iter`, which let you iterate over all the fields of a
record.  The following function uses `Logon.Fields.iter` and
`show_field` to print out all the fields of a `Logon` record.

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
session_id: 26685
time: 2012-06-26 18:44:13.807826
user: yminsky
credentials: Xy2d9W
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The advantage of using field iterators is that when the definition of
`Logon` changes, `iter` will change along with it, prompting you to
handle whatever new cases arise.

Field iterators are useful for a variety of tasks, from building
validation functions to scaffolding the definition of a web-form based
on a record type, all with a guarantee that you've exhaustively
considered all elements of the field.

## Variants

Variant types are used to represent multiple different possibilities,
where each possibility is identified by a different _constructor_.
The syntax of a variant type declaration is as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
type <variant-name> =
  | <Constructor1> [of <arg1> * .. * <argn>]?
  | <Constructor2> [of <arg1> * .. * <argn>]?
  ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To give a better sense of why variants are useful, we'll walk through
a concrete example of variants in action.

### Example: terminal colors

Almost all terminals support a set of 8 basic colors, which we can
represent with the following variant type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type std_color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a particularly simple form of variant, in that the
constructors don't have arguments.  Such variants are very similar to
the enumerations found in many languages, including C and Java.

We can construct an instance of `std_color` by writing out the
appropriate constructor, as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# [Black;Blue;Red];;
- : std_color list = [Black; Blue; Red]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pattern matching can then be used to process a variant.  Consider the
following function which converts each `std_color` to a corresponding
integer.  As we'll see, these integers will be used to compute escape
codes for setting colors in the terminal.  The pattern match is used
to choose a different integer for each constructor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let std_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;
val std_color_to_int : std_color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the exhaustiveness checking on pattern matches means that
the compiler will warn us if we miss a case in the pattern match.

Using `std_color_to_int`, we can now generate the escape codes to
change the color of a given string.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_by_number number text =
    sprintf "\027[38;5;%dm%s\027[0m" number text;;
  val color_by_number : int -> string -> string = <fun>
# let s = color_by_number (std_color_to_int Blue) "Hello Blue World!";;
val s : string = "\027[38;5;4mHello Blue World!\027[0m"
# printf "%s\n" s;;
Hello Blue World!
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On most terminals, that last line is printed in blue.

#### Full terminal colors

The simple enumeration of `std_color` isn't enough to fully describe
the set of colors that a modern terminal can display.  `xterm`s (and
many other terminal programs) support 256 different colors, broken up
into the following groups.

- The 8 basic colors, in regular and bold versions.
- A $6 \times 6 \times 6$ RGB color cube
- A 24-level grayscale ramp

We can represent this more complicated colorspace as a variant, but
this time, the different constructors will have arguments, to describe
the data available in each case.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type weight = Regular | Bold
  type color =
  | Basic of basic__color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color cube *)
  | Gray  of int                   (* 24 grayscale levels *)
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to compute the color code for a `color`, we use pattern
matching to break down the `color` variant into the appropriate cases.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
val color_to_int : color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<sidebar><title>Static checks for pattern matches</title>

The pattern matching code for `color_to_int` benefits from a number of
static checks from the compiler.  If we were to change the definition
of color as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type color =
  | Basic of basic_color     (* basic colors *)
  | Bold  of basic_color     (* bold basic colors *)
  | RGB   of int * int * int (* 6x6x6 color cube *)
  | Gray  of int             (* 24 grayscale levels *)
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Then our definition of `color_to_int` would be wrong in two ways, both
of which the compiler would catch.  First, the type of the contents of
`Basic` have changed: first there were two arguments, and now only
one:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
Characters 40-60:
Error: This pattern matches values of type 'a * 'b
       but a pattern was expected which matches values of type basic_color
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once we fix that, however, there's another problem, which is that we
haven't handled the new case of the `Bold` constructor.  The compiler
will catch this as well:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
Characters 19-154:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
Bold _
val color_to_int : color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally leading us to the correct implementation:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | Bold  basic_color -> 8 + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
val color_to_int : color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</sidebar>


Using the above function, we can print text using the full set of
available colors.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_print color s =
     printf "%s\n" (color_by_number (color_to_int color) s);;
val color_print : color -> string -> unit = <fun>
# color_print (Basic (Red,Bold)) "A bold red!";;
A bold red!
- : unit = ()
# color_print (Gray 4) "A muted gray...";;
A muted gray...
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Combining records and variants

Variants and records are most effective when used in concert.
Consider again the record example from section [[REUSING FIELD
NAMES]].

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Log_entry = struct
  type t =
    { session_id: string;
      time: Time.t;
      important: bool;
      message: string;
    }
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you can see, record types act like conjuntions: a `Log_entry.t` has
a `session_id` _and_ a `time` _and_ an `imporant` flag _and_ a
`message`.  Variants, on the other hand, are more like disjunctions,
letting you write down a type that represents multiple different
possibilities.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type client_message = | Logon of Logon.t
                      | Heartbeat of Heartbeat.t
                      | Log_entry of Log_entry.t
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thus, a `client_message` is a `Logon` _or_ a `Heartbeat` _or_ a
`Log_entry`.  Indeed, if we want to write code that processes messages
generically, rather than code specialized to a fixed message type, we
need a type like `client_message` to unify the different message types
into one overarching type. 

Consider the following function that takes a list of `client_message`s
and returns all messages assocaited with a given user.  The code is
implemented as a fold over the list of messages, where the accumulator o

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) message ->
        match message with
        | Logon m ->
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions m.Logon.session_id)
          else acc
        | _ ->
          let session_id = match message with
            | Logon     m -> m.Logon.session_id
            | Heartbeat m -> m.Heartbeat.session_id
            | Log_entry m -> m.Log_entry.session_id
          in
          if Set.mem user_sessions session_id then
            (message::messages,user_sessions)
          else acc
      )
  in
  List.rev user_messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




This gets at the basic structure, but there are some problems with
this design.  Consider how you would write a function to extract the
time that a `client_message` happened.  You might write something like
this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let message_time = function
    | Logon m -> m.Logon.time
    | Heartbeat m -> m.Heartbeat.time
    | Log_entry m -> m.Log_entry.time
    ;;
val message_time : client_message -> Core.Std.Time.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This works, but it's unfortunate that we need to write what is
essentially the same code over and over, once for each message type.
We can get around this by refactoring our types to explicitly separate
which parts are shared and which parts are common.  The first step is
to cut down the definitions of the per-message records to just contain
the unique components of each message.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Log_entry = struct
    type t = { important: bool;
               message: string }
  end
  module Heartbeat = struct
    type t = { status_message: string; }
  end
  module Logon = struct
    type t = { user: string;
               credentials: string }
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can also define a variant type for capturing the different
details.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type details =
    | Logon     of Logon.t
    | Heartbeat of Heartbeat.t
    | Log_entry of Log_entry.t ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And then, we can define a record that contains the fields that are
common across all messages.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Common = struct
    type t = { session_id: string;
               time: Time.t; }
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A full message can then represented as a pair of a `Common.t` and a
`details`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let message_time (c,_) = c.Common.time;;
val message_time : Common.t * 'a -> Time.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now use the type `Common.t * message` to represent a message of
any type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let c = { Common. time = Time.now (); session_id = "abc" };;
# let l = { Logon.user = "yminsky"; credentials = "Xyx23djfD" };;
# (c,l);;
- : Common.t * Logon.t =
({Common.session_id = "abc"; Common.time = 2012-07-10 08:24:30.713823},
 {Logon.user = "yminsky"; Logon.credentials = "Xyx23djfD"})
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Indeed, we can use variants to solve some problems with the original
design of our message types.  Remember that all three of our message
types contained a `session_id` and `time` field with the same types
and the same meaning.   This repetition is problematic





### Polymorphic variants

## More advanced declarations

### Sharing across datatypes

### Polymorphic types

### Recursive types


### Detritus

Variants and records are closely related concepts.  Records combine
multiple types together in a conjunctive way: a logon record has a
session id _and_ a time _and_ a user _and_ credentials.  But
sometimes, you want to combine types in a disjunctive way.  For
example, if you wanted a type to represent all possible message, you'd
need something that was a log entry _or_ a heartbeat _or_ a log-on.
Variant types are used for just such declarations, as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type client_message = | Logon of Logon.t
                        | Heartbeat of Heartbeat.t
                        | Log_entry of Log_entry.t;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Logon`, `Heartbeat` and `Log_entry` are called constructors, and
they're what you use to create a `message`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let message =
     Log_entry (create_log_entry ~session_id:"345"
                ~important:false "Hello World");;
val message : client_message =
  Log_entry
   {Log_entry.session_id = "345";
    Log_entry.time = 2012-07-04 13:55:18.798289; Log_entry.important = false;
    Log_entry.message = "Hello World"}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
