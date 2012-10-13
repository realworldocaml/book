# Variants

Variant types are used to represent multiple different possibilities,
where each possibility is identified by a different _constructor_.
The syntax of a variant type declaration is as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
type <variant-name> =
  | <Constructor1> [of <arg1> * .. * <argn>]?
  | <Constructor2> [of <arg1> * .. * <argn>]?
  ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic purpose of variants is to effectively represent data that
may have multiple different cases.  We can give a better sense of the
utility of variants by walking through a concrete example, which we'll
do by thinking about how to represent terminal colors.

## Example: terminal colors

Almost all terminals support a set of 8 basic colors, which we can
represent with the following variant type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type basic_color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a particularly simple form of variant, in that the
constructors don't have arguments.  Such variants are very similar to
the enumerations found in many languages, including C and Java.

We can construct instances of `basic_color` by simply writing out the
constructors in question.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# [Black;Blue;Red];;
- : basic_color list = [Black; Blue; Red]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pattern matching can be used to process a variant.  The following
function uses pattern matching to convert `basic_color` to a
corresponding integer for use in creating color-setting escape codes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;
val basic_color_to_int : basic_color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the exhaustiveness checking on pattern matches means that
the compiler will warn us if we miss a color.

Using this function, we can generate the escape codes to change the
color of a given string.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_by_number number text =
    sprintf "\027[38;5;%dm%s\027[0m" number text;;
  val color_by_number : int -> string -> string = <fun>
# let s = color_by_number (basic_color_to_int Blue) "Hello Blue World!";;
val s : string = "\027[38;5;4mHello Blue World!\027[0m"
# printf "%s\n" s;;
Hello Blue World!
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On most terminals, that last line is printed in blue.

### Full terminal colors

The simple enumeration of `basic_color` isn't enough to fully describe
the set of colors that a modern terminal can display.  Many terminals,
including the venerable `xterm`, support 256 different colors, broken
up into the following groups.

- The 8 basic colors, in regular and bold versions.
- A $6 \times 6 \times 6$ RGB color cube
- A 24-level grayscale ramp

We can represent this more complicated color-space as a variant, but
this time, the different constructors will have arguments, to describe
the data available in each case.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type weight = Regular | Bold
  type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
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

<sidebar><title>Catch-all cases and refactoring</title>

OCaml's type system can act as a form of refactoring tool, where the
compiler warns you of places where your code needs to be adapted to
changes made elsewhere.  This is particularly valuable when working
with variant types.

Consider what would happen if we were to change the definition of
`color` to the following.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type color =
  | Basic of basic_color     (* basic colors *)
  | Bold  of basic_color     (* bold basic colors *)
  | RGB   of int * int * int (* 6x6x6 color cube *)
  | Gray  of int             (* 24 grayscale levels *)
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We've essentially broken out the `Basic` case into two cases, `Basic`
and `Bold`, and `Basic` has changed from having two arguments to one.
`color_to_int` as we wrote it still expects the old structure of the
variant, and if we try to compile that same code again, the compiler
will notice the discrepancy.

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

Here, the compiler is complaining that the `Basic` constructor is
assumed to have the wrong number of arguments.  If we fix that,
however, the compiler flag will flag a second problem, which is that
we haven't handled the new `Bold` constructor.

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

Fixing this now leads us to the correct implementation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | Bold  basic_color -> 8 + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
val color_to_int : color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you can see, the type system identified for us the places in our
code that needed to be fixed.  This refactoring isn't entirely free,
however.  To really take advantage of it, you need to write your code
in a way that maximizes the compiler's chances of helping you find
your bugs.  One important rule is to avoid catch-all cases in pattern
matches.

Here's an example of how a catch-all case plays in.  Imagine we wanted
a version of `color_to_int` that works on older terminals by rendering
the first 16 colors (the 8 `basic_color`s in regular and bold) in the
normal way, but rendering everything else as white.  We might have
written the function as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let oldschool_color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | _ -> basic_color_to_int White;;
val oldschool_color_to_int : color -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

But because the catch-all case encompasses all possibilities, the type
system will no longer warn us that we have missed the new `Bold` case
when we change the type to include it.  We can get this check back by
being more explicit about what we're ignoring.  We haven't changed the
behavior of the code, but we have improved our robustness to change.

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

## Combining records and variants

Records and variants are most effective when used in concert.
Consider again the type `Log_entry.t` from section [[REUSING FIELD
NAMES]]:

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

This record type combines multiple pieces of data into one value.  In
particular, a single `Log_entry.t` has a `session_id` _and_ a `time`
_and_ an `important` flag _and_ a `message`.  More generally, you can
think of record types as acting as conjunctions.  Variants, on the
other hand, are disjunctions, letting you represent multiple
possibilities, as in the following example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type client_message = | Logon of Logon.t
                      | Heartbeat of Heartbeat.t
                      | Log_entry of Log_entry.t
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A `client_message` is a `Logon` _or_ a `Heartbeat` _or_ a `Log_entry`.
If we want to write code that processes messages generically, rather
than code specialized to a fixed message type, we need something like
`client_message` to act as one overarching type for the different
possible messages.

You can increase the precision of your types by using variants to
represent structural differences between types, and records to
represent structure that is shared.  As an example, consider the
following function that takes a list of `client_message`s and returns
all messages generated by a given user.  The code in question is
implemented by folding over the list of messages, where the
accumulator is a pair of:

  - the set of session identifiers for the user that have been seen
    thus far.
  - the set of messages so far that are associated with the user.

Here's the concrete code.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) message ->
        match message with
        | Logon m ->
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions m.Logon.session_id)
          else acc
        | Heartbeat _ | Log_entry _ ->
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

There's one awkward bit about the code above, which is the calculation
of the session ids.  In particular, we have the following repetitive
snippet of code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
  let session_id = match message with
    | Logon     m -> m.Logon.session_id
    | Heartbeat m -> m.Heartbeat.session_id
    | Log_entry m -> m.Log_entry.session_id
  in
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This code effectively computes the session id for each underlying
message type.  The repetition in this case isn't that bad, but would
become problematic in larger and more complicated examples.

We can improve the code by refactoring our types to explicitly
separate which parts are shared and which are common.  The first step
is to cut down the definitions of the per-message records to just
contain the unique components of each message.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Log_entry = struct
  type t = { important: bool;
             message: string;
           }
end

module Heartbeat = struct
  type t = { status_message: string; }
end

module Logon = struct
  type t = { user: string;
             credentials: string;
           }
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can then define a variant type that covers the different possible
unique components.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type details =
| Logon of Logon.t
| Heartbeat of Heartbeat.t
| Log_entry of Log_entry.t
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Separately, we need a record that contains the fields that are common
across all messages.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Common = struct
  type t = { session_id: string;
             time: Time.t;
           }
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A full message can then represented as a pair of a `Common.t` and a
`details`.  Using this, we can rewrite our example above as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
        let session_id = common.Common.session_id in
        match details with
        | Logon m ->
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions session_id)
          else acc
        | Heartbeat _ | Log_entry _ ->
          if Set.mem user_sessions session_id then
            (message::messages,user_sessions)
          else acc
      )
  in
  List.rev user_messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the more complex match statement for computing the session
id has been replaced with the simple expression
`common.Common.session_id`.

This basic design is good in another way: it allows us to essentially
downcast to the specific message type once we know what it is, and
then dispatch code to handle just that message type.  In particular,
while we use the type `Common.t * details` to represent an arbitrary
message, we can use `Common.t * Logon.t` to represent a logon message.
Thus, if we had functions for handling individual message types, we
could write a dispatch function as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let handle_message server_state (common,details) =
  match details with
  | Log_entry m -> handle_log_entry server_state (common,m)
  | Logon     m -> handle_logon     server_state (common,m)
  | Heartbeat m -> handle_heartbeat server_state (common,m)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And it's explicit at the type level that `handle_log_entry` sees only
`Log_entry` messages, `handle_logon` sees only `Logon` messages, etc.

## Variants and recursive data structures

Another common application of variants is to represent tree-like
recursive data-structures.  One very simple application of this
technique is that of building an expression evaluator.

Imagine you wanted a small domain-specific language for expressing
Boolean expressions.  This can be useful in any application that
requires a filter language, which show up in everything from packet
analyzers to mail clients.

The declaration of the type for representing an expression is fairly
straight-forward --- we'll have one constructor for each different
kind of expression in our mini language, as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type 'a t =
  | Base  of 'a
  | Const of bool
  | And   of 'a t list
  | Or    of 'a t list
  | Not   of 'a t
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `Base` constructor is to allow us to include some set of base
predicates.  These base predicates are what tie the expressions in
question to the application.  Thus, if you were writing a filter
language for an email processor, your base predicates might have
things like what is found on the `to` or `subject` line of an email.

The definition of `t` is recursive, which means that we can construct
complex nested expressions based on this.  Here, we'll first declare a
type for the base predicates that encodes a very simple set of email
filters.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type mail_field = To | From | CC | Date | Subject
  type mail_filter = { field: mail_field;
                       contains: string }
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And now, we can construct a simple expression that uses `mail_filter`
for its base predicate as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# And [ Or [ Base { field = To; contains = "doligez" } ;
             Base { field = CC; contains = "doligez" } ];
        Base { field = Subject; contains = "runtime" } ];;
    - : mail_filter t =
And
 [Or
   [Base {field = To; contains = "doligez"};
    Base {field = CC; contains = "doligez"}];
  Base {field = Subject; contains = "runtime"}]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Being able to construct such expressions is all well and good, but to
do any real work, we need some way to evaluate these expressions.
Here's a piece of code to do just that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec eval t base_eval =
    let eval' t = eval t base_eval in
    match t with
    | Base  base -> base_eval base
    | Const bool -> bool
    | And   ts   -> List.for_all ts ~f:eval'
    | Or    ts   -> List.exists  ts ~f:eval'
    | Not   t    -> not (eval' t)
  ;;
val eval : 'a t -> ('a -> bool) -> bool = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we defined a helper function, `eval'`, which is the same as
`eval` except that it's specialized to use `base_eval`.  That's just
to remove a bit of boilerplate from the recursive applications of
`eval`.

The structure of the code is pretty straightforward --- we're just
walking over the structure of the data, doing the appropriate thing at
each state, which sometimes requires a recursive call and sometimes
doesn't.

Our `eval` function just walks the value in question.  We can also
write code to transform an expression, for example, by simplifying an
expression.  Here's a function that does just that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec simplify = function
    | Base _ | Const _ as x -> x
    | And ts ->
      let ts = List.map ~f:simplify ts in
      if List.exists ts ~f:(function Const false -> true | _ -> false)
      then Const false
      else And ts
    | Or ts ->
      let ts = List.map ~f:simplify ts in
      if List.exists ts ~f:(function Const true -> true | _ -> false)
      then Const true else Or ts
    | Not t ->
      match simplify t with
      | Const b -> Const (not b)
      | t -> Not t
  ;;
val simplify : 'a t -> 'a t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One thing to notice about the above code is that it uses a catch-all
case in the very last line within the `Not` case.  It's generally
better to be explicit about the cases you're ignoring.  Indeed, if we
change this snippet of code to be more explicit:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    | Not t ->
      match simplify t with
      | Const b -> Const (not b)
      | (And _ | Or _ | Base _ | Not _) -> Not t
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we can immediately notice that we've missed an important
simplification.  Really, we should have simplified double negation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    | Not t ->
      match simplify t with
      | Const b -> Const (not b)
      | Not t -> t
      | (And _ | Or _ | Base _ ) -> Not t
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All of this is more than a theoretical example.  There's a library
very much in this spirit already exists as part of `Core`, called
`Blang` (short for "boolean language"), and it gets a lot of practical
use in a variety of applications.

## Polymorphic variants

In addition to ordinary variant types, OCaml has a second kind of
variant type called _polymorphic variants_.  As we've seen, ordinary
variants are a powerful tool for describing and operating on complex
data-structures.  But variants have limitations as well.  One notable
limitation is that you can't share constructors between different
variant types.  To see what this means, let's consider an example.

Imagine that we have a new terminal type that adds yet more colors,
say, by adding an alpha channel, and we wanted a type to model those
colors.  We might model that as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type extended_color =
  | Basic of basic_color * weight  (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color space *)
  | RGBA  of int * int * int * int (* 6x6x6x6 color space *)
  | Gray  of int                   (* 24 grayscale levels *)
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that the constructors of this new type have no
relationship with the constructors of `color`.  In particular, we'd
like to be able to write the following function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let extended_color_to_int = function
    | RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On the face of it this looks reasonable enough, but it leads to the
following type error.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
Characters 93-98:
    | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
                                                          ^^^^^
Error: This expression has type extended_color
       but an expression was expected of type color
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that `extended_color` and `color` are in the compiler's
view distinct and unrelated types.  The compiler doesn't recognize
that, for instance, the `Basic` constructor in both types is in some
sense the same.  Moreover, the definition of `extended_color` shadows
the constructors from `color`, so we can no longer even create a
`color`, though we can deal with this problem by moving the
definitions into different modules, much as we did with records to
avoid collisions between field names.

Polymorphic variants allow a way around this problem entirely.  In the
following we'll rewrite our color code to use polymorphic variants.
As you can see, polymorphic variants are distinguished from ordinary
variants by the backtick at the beginning of the constructor.  We'll
start with `basic_color_to_int`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7
;;
val basic_color_to_int :
  [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ] ->
  int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you can see, we didn't need to declare the polymorphic variant
before using it; it was simply inferred from the code.  We can infer
more complicated polymorphic variants as well, as you can see here.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let color_to_int = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i
;;
val color_to_int :
  [< `Basic of
       [< `Black
        | `Blue
        | `Cyan
        | `Green
        | `Magenta
        | `Red
        | `White
        | `Yellow ] *
       [< `Bold | `Regular ]
   | `Gray of int
   | `RGB of int * int * int ] ->
  int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and indeed, the full `extended_color_to_int` function works exactly as
intended.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let extended_color_to_int = function
    | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color
;;
val extended_color_to_int :
  [< `Basic of
       [< `Black
        | `Blue
        | `Cyan
        | `Green
        | `Magenta
        | `Red
        | `White
        | `Yellow ] *
       [< `Bold | `Regular ]
   | `Gray of int
   | `RGB of int * int * int
   | `RGBA of int * int * int * int ] ->
  int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Polymorphic variants and subtyping

Seeing polymorphic variants in action makes them seem a little
magical, and indeed, understanding how polymorphic variants really
work is somewhat tricky.  Let's step back to work through some simple
numerical examples.

First, let's see what happens when we declare a simple variant for
representing integers.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let three = `Int 3;;
val three : [> `Int of int ] = `Int 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note the `>` at the beginning of the type.  This indicates that the
type of three is open to other variants.  In particular, we can put
`three` on a list with a value of a different variant.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let nums = [three; `Float 4.0];;
val nums : [> `Float of float | `Int of int ] list = [`Int 3; `Float 4.]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Roughly speaking, you can read the `>` as indicating "these variants
are present, and maybe more".  We can close the definition of `three`
by an explicit annotation, at which point, combining it with other
constructors no longer works.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let three : [`Int of int] = `Int 3 ;;
val three : [ `Int of int ] = `Int 3
# let nums = [three; `Float 4.];;
Characters 19-28:
  let nums = [three; `Float 4.];;
                     ^^^^^^^^^
Error: This expression has type [> `Float of float ]
       but an expression was expected of type [ `Int of int ]
       The second variant type does not allow tag(s) `Float
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In our example above, `three` was compatible with a particular set of
tags or more.  In some cases, we see types that are compatible with a
given set of types or less, as in the following case:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let add x y =
    match x,y with
    | `Int x, `Int y -> `Int (x + y)
    | `Float x, `Float y -> `Float (x +. y)
    | `Int i, `Float f | `Float f, `Int i -> `Float (f +. Float.of_int i)
  ;;
val add :
  [< `Float of float | `Int of int ] ->
  [< `Float of float | `Int of int ] -> [> `Float of float | `Int of int ] =
  <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, `add` can take any value that has floats or ints, but nothing
else.  Thus, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# add three three;;
- : [> `Float of float | `Int of int ] = `Int 6
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

but we can't write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# add three (`Rational (3,4));;
Characters 11-26:
  add three (`Rational (3,4));;
             ^^^^^^^^^^^^^^^
Error: This expression has type [> `Rational of int * int ]
       but an expression was expected of type
         [< `Float of float | `Int of int ]
       The second variant type does not allow tag(s) `Rational
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

