# Variants

Variant types are one of the most useful features of OCaml, and also
one of the most unusual.  Variants allow you to represent data that
may take on multiple different forms, where each form is marked by an
explicit tag.  Let's consider a simple concrete example of how to use
variant types: dealing with terminal colors.

Almost all terminals support a set of 8 basic colors.  We can
represent these with the following variant type.

```ocaml
# type basic_color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White ;;
```

Here, the different cases of the variant are separated by pipes, and
each case is a simple tag with no associated data.  This is similar to
the enumerations found in many languages, including C and Java.

We can construct instances of `basic_color` by simply writing down the
tags.

```ocaml
# [Black;Blue;Red];;
- : basic_color list = [Black; Blue; Red]
```

Pattern matching can be used to process a variant.  The following
function uses pattern matching to convert `basic_color` to a
corresponding integer for use in creating color-setting escape codes.

```ocaml
# let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;
val basic_color_to_int : basic_color -> int = <fun>
```

Note that the exhaustiveness checking on pattern matches means that
the compiler will warn us if we miss a color.

Using this function, we can generate the escape codes to change the
color of a given string.

```ocaml
# let color_by_number number text =
    sprintf "\027[38;5;%dm%s\027[0m" number text;;
  val color_by_number : int -> string -> string = <fun>
# let s = color_by_number (basic_color_to_int Blue) "Hello Blue World!";;
val s : string = "\027[38;5;4mHello Blue World!\027[0m"
# printf "%s\n" s;;
Hello Blue World!
- : unit = ()
```

On most terminals, that last line is printed in blue.

The simple enumeration of `basic_color` isn't enough to fully describe
the set of colors that a modern terminal can display.  Many terminals,
including the venerable `xterm`, support 256 different colors, broken
up into the following groups.

- The 8 basic colors, in regular and bold versions.
- A $6 \times 6 \times 6$ RGB color cube
- A 24-level grayscale ramp

We can represent this more complicated color-space as a variant, but
this time, the different tags will have arguments, to describe
the data available in each case.

```ocaml
# type weight = Regular | Bold
  type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color cube *)
  | Gray  of int                   (* 24 grayscale levels *)
;;
```

The definition of `color` uses more of the power of variants by
attaching data to the different tags.  In order to compute the color
code for a `color`, we use pattern matching to break down the `color`
variant into the appropriate cases.

```ocaml
# let color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
val color_to_int : color -> int = <fun>
```

<sidebar><title>Catch-all cases and refactoring</title>

OCaml's type system can act as a form of refactoring tool, where the
compiler warns you of places where your code needs to be adapted to
changes made elsewhere.  This is particularly valuable when working
with variant types.

Consider what would happen if we were to change the definition of
`color` to the following.

```ocaml
# type color =
  | Basic of basic_color     (* basic colors *)
  | Bold  of basic_color     (* bold basic colors *)
  | RGB   of int * int * int (* 6x6x6 color cube *)
  | Gray  of int             (* 24 grayscale levels *)
;;
```

We've essentially broken out the `Basic` case into two cases, `Basic`
and `Bold`, and `Basic` has changed from having two arguments to one.
`color_to_int` as we wrote it still expects the old structure of the
variant, and if we try to compile that same code again, the compiler
will notice the discrepancy.

```ocaml
# let color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
Characters 40-60:
Error: This pattern matches values of type 'a * 'b
       but a pattern was expected which matches values of type basic_color
```

Here, the compiler is complaining that the `Basic` tag is
assumed to have the wrong number of arguments.  If we fix that,
however, the compiler flag will flag a second problem, which is that
we haven't handled the new `Bold` tag.

```ocaml
# let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
Characters 19-154:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
Bold _
val color_to_int : color -> int = <fun>
```

Fixing this now leads us to the correct implementation.

```ocaml
# let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | Bold  basic_color -> 8 + basic_color_to_int basic_color
    | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | Gray i -> 232 + i ;;
val color_to_int : color -> int = <fun>
```

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

```ocaml
# let oldschool_color_to_int = function
    | Basic (basic_color,weight) ->
      let base = match weight with Bold -> 8 | Regular -> 0 in
      base + basic_color_to_int basic_color
    | _ -> basic_color_to_int White;;
val oldschool_color_to_int : color -> int = <fun>
```

But because the catch-all case encompasses all possibilities, the type
system will no longer warn us that we have missed the new `Bold` case
when we change the type to include it.  We can get this check back by
being more explicit about what we're ignoring.  We haven't changed the
behavior of the code, but we have improved our robustness to change.

</sidebar>

Using the above function, we can print text using the full set of
available colors.

```ocaml
# let color_print color s =
     printf "%s\n" (color_by_number (color_to_int color) s);;
val color_print : color -> string -> unit = <fun>
# color_print (Basic (Red,Bold)) "A bold red!";;
A bold red!
- : unit = ()
# color_print (Gray 4) "A muted gray...";;
A muted gray...
- : unit = ()
```

## Combining records and variants

Records and variants are most effective when used in concert.
Consider again the type `Log_entry.t` from section [[REUSING FIELD
NAMES]]:

```ocaml
module Log_entry = struct
  type t =
    { session_id: string;
      time: Time.t;
      important: bool;
      message: string;
    }
end
```

This record type combines multiple pieces of data into one value.  In
particular, a single `Log_entry.t` has a `session_id` _and_ a `time`
_and_ an `important` flag _and_ a `message`.  More generally, you can
think of record types as acting as conjunctions.  Variants, on the
other hand, are disjunctions, letting you represent multiple
possibilities, as in the following example.

```ocaml
type client_message = | Logon of Logon.t
                      | Heartbeat of Heartbeat.t
                      | Log_entry of Log_entry.t
```

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

```ocaml
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
```

There's one awkward bit about the code above, which is the calculation
of the session ids.  In particular, we have the following repetitive
snippet of code:

```ocaml
  let session_id = match message with
    | Logon     m -> m.Logon.session_id
    | Heartbeat m -> m.Heartbeat.session_id
    | Log_entry m -> m.Log_entry.session_id
  in
```

This code effectively computes the session id for each underlying
message type.  The repetition in this case isn't that bad, but would
become problematic in larger and more complicated examples.

We can improve the code by refactoring our types to explicitly
separate which parts are shared and which are common.  The first step
is to cut down the definitions of the per-message records to just
contain the unique components of each message.

```ocaml
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
```

We can then define a variant type that covers the different possible
unique components.

```ocaml
type details =
| Logon of Logon.t
| Heartbeat of Heartbeat.t
| Log_entry of Log_entry.t
```

Separately, we need a record that contains the fields that are common
across all messages.

```ocaml
module Common = struct
  type t = { session_id: string;
             time: Time.t;
           }
end
```

A full message can then represented as a pair of a `Common.t` and a
`details`.  Using this, we can rewrite our example above as follows:

```ocaml
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
```

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

```ocaml
let handle_message server_state (common,details) =
  match details with
  | Log_entry m -> handle_log_entry server_state (common,m)
  | Logon     m -> handle_logon     server_state (common,m)
  | Heartbeat m -> handle_heartbeat server_state (common,m)
```

And it's explicit at the type level that `handle_log_entry` sees only
`Log_entry` messages, `handle_logon` sees only `Logon` messages, etc.

## Variants and recursive data structures

Another common application of variants is to represent tree-like
recursive data-structures.  Let's see how this works by working
through a simple example: designing a Boolean expression evaluator.

Such a language can be useful anywhere you need to specify filters,
which are used in everything from packet analyzers to mail clients.
Below, we define a variant called `blang` (short for "binary
language") with one tag for each kind of expression we want to
support.

```ocaml
# type 'a blang =
  | Base  of 'a
  | Const of bool
  | And   of 'a blang list
  | Or    of 'a blang list
  | Not   of 'a blang
  ;;
```

Note that the definition of the type `blang` is recursive, meaning
that a `blang` may contain other `blang`s.

The only mysterious bit about `blang` is the role of `Base`.  The
`Base` tag is to let the language include a set of base
predicates.  These base predicates tie the expressions in question to
whatever our application is.  Thus, if you were writing a filter
language for an email processor, your base predicates might specify
the tests you would run against an email.  Here's a simple example of
how you might define a base predicate type.

```ocaml
# type mail_field = To | From | CC | Date | Subject
  type mail_predicate = { field: mail_field;
                          contains: string }
  ;;
```

And now, we can construct a simple expression that uses
`mail_predicate` for its base predicate.

```ocaml
# And [ Or [ Base { field = To; contains = "doligez" } ;
             Base { field = CC; contains = "doligez" } ];
        Base { field = Subject; contains = "runtime" } ];;
    - : mail_predicate blang =
And
 [Or
   [Base {field = To; contains = "doligez"};
    Base {field = CC; contains = "doligez"}];
  Base {field = Subject; contains = "runtime"}]
```

Being able to construct such expressions is all well and good, but to
do any real work, we need some way to evaluate an expression.  Here's
a piece of code to do just that.

```ocaml
# let rec eval blang base_eval =
    let eval' blang = eval blang base_eval in
    match blang with
    | Base  base   -> base_eval base
    | Const bool   -> bool
    | And   blangs -> List.for_all blangs ~f:eval'
    | Or    blangs -> List.exists  blangs ~f:eval'
    | Not   blang  -> not (eval' blang)
  ;;
val eval : 'a blang  -> ('a -> bool) -> bool = <fun>
```

The structure of the code is pretty straightforward --- we're just
walking over the structure of the data, doing the appropriate thing at
each state, which sometimes requires a recursive call and sometimes
doesn't.  We did define a helper function, `eval'`, which is just
`eval` specialized to use `base_eval`, and is there to remove some
boilerplate from the recursive calls to `eval`.

We can also write code to transform an expression, for example, by
simplifying it.  Here's a function to does just that.

```ocaml
# let rec simplify = function
    | Base _ | Const _ as x -> x
    | And blangs ->
      let blangs = List.map ~f:simplify blangs in
      if List.exists blangs ~f:(function Const false -> true | _ -> false)
      then Const false
      else And blangs
    | Or blangs ->
      let blangs = List.map ~f:simplify blangs in
      if List.exists blangs ~f:(function Const true -> true | _ -> false)
      then Const true else Or blangs
    | Not blang ->
      match simplify blang with
      | Const bool -> Const (not bool)
      | blang -> Not blang
  ;;
val simplify : 'a blang -> 'a blang = <fun>
```

One thing to notice about the above code is that it uses a catch-all
case in the very last line within the `Not` case.  It's generally
better to be explicit about the cases you're ignoring.  Indeed, if we
change this snippet of code to be more explicit:

```ocaml
    | Not blang ->
      match simplify blang with
      | Const bool -> Const (not bool)
      | (And _ | Or _ | Base _ | Not _) -> Not blang
```

we can immediately notice that we've missed an important
simplification.  Really, we should have simplified double negation.

```ocaml
    | Not blang ->
      match simplify blang with
      | Const b -> Const (not b)
      | Not blang -> blang
      | (And _ | Or _ | Base _ ) -> Not blang
```

This example is more than a toy.  There's a module very much in this
spirit already exists as part of Core, and gets a lot of practical use
in a variety of applications.  More generally, using variants to build
recursive data-structures is a common technique, and shows up
everywhere from designing little languages to building efficient
data-structures like red-black trees.

## Polymorphic variants

In addition to the ordinary variants we've seen so far, OCaml also
supports so-called _polymorphic variants_.  As we'll see, polymorphic
variants are more flexible and syntactically more lightweight than
ordinary variants, but that extra power comes at a cost, as we'll see.

Syntactically, polymorphic variants are distinguished from ordinary
variants by the leading backtick.  Pleasantly enough, you can create a
polymorphic variant without first writing an explicit type
declaration.

```ocaml
# let three = `Int 3;;
val three : [> `Int of int ] = `Int 3
# let four = `Float 4.;;
val four : [> `Float of float ] = `Float 4.
# let nan = `Not_a_number;;
val nan : [> `Not_a_number ] = `Not_a_number
# [three; four; nan];;
- : [> `Float of float | `Int of int | `Not_a_number ] list =
[`Int 3; `Float 4.; `Not_a_number]
```

Variant types are inferred automatically from their use, and when we
combine variants whose types contemplate different tags, the compiler
infers a new type that knows about both all those tags.

The type system will complain, however, if it sees incompatible uses
of the same tag:

```ocaml
# let five = `Int "five";;
val five : [> `Int of string ] = `Int "five"
# [three; four; five];;
Characters 14-18:
  [three; four; five];;
                ^^^^
Error: This expression has type [> `Int of string ]
       but an expression was expected of type
         [> `Float of float | `Int of int ]
       Types for tag `Int are incompatible
```

The `>` at the beginning of the variant types above is critical,
because it marks the types as being open to combination with other
variant types.  We can read the type ``[> `Int of string | `Float of
float]`` as describing a variant whose tags include `` `Int of string
`` and `` `Float of float ``, but may include more tags as well.  You
can roughly translate `>` to "these tags or more".

OCaml will in some cases infer a variant type with ` <`, to indicate
"these tags or less", as in the following example.

```ocaml
# let is_positive = function
     | `Int   x -> x > 0
     | `Float x -> x > 0.
  ;;
val is_positive : [< `Float of float | `Int of int ] -> bool = <fun>
```

The `<` is there because `is_positive` has no way of dealing with
values that have tags other than `` `Float of float`` or `` `Int of
int``.

We can think of these `<` and `>` markers as indications of upper and
lower bounds.  If the same type is both an upper and a lower bound, we
end up with an _exact_ polymorphic variant type, which has neither
marker.  For example:

```ocaml
# let exact = List.filter ~f:is_positive [three;four];;
val exact : [ `Float of float | `Int of int ] list
   = [`Int 3; `Float 4.]
```

<sidebar><title>Polymorphic variants and casts</title>

Most of the time, the inference system is able to infer polymorphic
variant types that work without any extra help from the user.  In some
cases, however, OCaml can't figure out how to make the types work on
its own, and requires some extra annotations.  

```ocaml
```

</sidebar>

Perhaps surprisingly, we can also create polymorphic variant types
that have different lower and upper bounds.

```ocaml
let is_positive = function
     | `Int   x -> Ok (x > 0)
     | `Float x -> Ok (x > 0.)
     | `Not_a_number -> Error "not a number";;
val is_positive :
  [< `Float of float | `Int of int | `Not_a_number ] ->
  (bool, string) Result.t = <fun>
# List.filter [three; four] ~f:(fun x ->
     match is_positive x with Error _ -> false | Ok b -> b);;
  - : [< `Float of float | `Int of int | `Not_a_number > `Float `Int ]
    List.t
= [`Int 3; `Float 4.]
```

Here, the inferred type states that the tags can be no more than ``
`Float``, `` `Int`` and `` `Not_a_number``, and must contain at least
`` `Float`` and `` `Int``.  As you can already start to see,
polymorphic variants can lead to fairly complex inferred types.



### Example: Terminal colors redux

To see how to use polymorphic variants in practice, let's go back to
the terminal color example that we discussed earlier.  Imagine that we
have a new terminal type that adds yet more colors, say, by adding an
alpha channel so you can specify translucent colors.  We could model
this extended set of colors as follows, using an ordinary variant.

```ocaml
# type extended_color =
  | Basic of basic_color * weight  (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color space *)
  | Gray  of int                   (* 24 grayscale levels *)
  | RGBA  of int * int * int * int (* 6x6x6x6 color space *)
  ;;
```

We want to write a function `extended_color_to_int`, that works like
`color_to_int` for all of the old kinds of colors, with new logic only
for handling colors that include an alpha channel.  We might think we
could write the function to do this as follows.

```ocaml
# let extended_color_to_int = function
    | RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
  ;;
```

This looks reasonable enough, but it leads to the following type
error.

```ocaml
Characters 93-98:
    | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
                                                          ^^^^^
Error: This expression has type extended_color
       but an expression was expected of type color
```

The problem is that `extended_color` and `color` are in the compiler's
view distinct and unrelated types.  The compiler doesn't, for example,
recognize any equality between the `Basic` tag in the two
types.

What we essentially want to do is to share tags between two
different types, and polymorphic variants let us do this.  First,
let's rewrite `basic_color_to_int` and `color_to_int` using
polymorphic variants.  The translation here is entirely
straightforward.

```ocaml
# let basic_color_to_int = function
    | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
    | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

  let color_to_int = function
    | `Basic (basic_color,weight) ->
      let base = match weight with `Bold -> 8 | `Regular -> 0 in
      base + basic_color_to_int basic_color
    | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | `Gray i -> 232 + i
 ;;
val basic_color_to_int :
  [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ] ->
  int = <fun>
val color_to_int :
  [< `Basic of
       [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red
        | `White | `Yellow ] *
       [< `Bold | `Regular ]
   | `Gray of int
   | `RGB of int * int * int ] ->
  int = <fun>
```

Now we can try writing `extended_color_to_int`.  The key issue with
this code is that `extended_color_to_int` needs to invoke
`color_to_int` with a narrower type, _i.e._, one that includes fewer
tags.  Written properly, this narrowing can be done via a pattern
match.  In particular, in the following code, the type of the variable
`color` includes only the tags `` `Basic``, `` `RGB`` and `` `Gray``,
and not `` `RGBA``.

```ocaml
# let extended_color_to_int = function
    | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color
  ;;
val extended_color_to_int :
  [< `Basic of
       [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red
        | `White | `Yellow ] *
       [< `Bold | `Regular ]
   | `Gray of int
   | `RGB of int * int * int
   | `RGBA of int * int * int * int ] ->
  int = <fun>
```

The above code is more delicately balanced than one might imagine.
In particular, if we use a catch-all case instead of an explicit
enumeration of the cases, the type is no longer narrowed, and so
compilation fails.

```ocaml
# let extended_color_to_int = function
    | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    | color -> color_to_int color
  ;;
      Characters 125-130:
      | color -> color_to_int color
                              ^^^^^
Error: This expression has type [> `RGBA of int * int * int * int ]
       but an expression was expected of type
         [< `Basic of
              [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red
               | `White | `Yellow ] *
              [< `Bold | `Regular ]
          | `Gray of int
          | `RGB of int * int * int ]
       The second variant type does not allow tag(s) `RGBA
```

<sidebar><title>Polymorphic variants and catch-all cases</title>

As we saw with the definition of `is_positive`, a match statement can
lead to the inference of an upper bound on a variant type, limiting
the possible tags to those that can be handled by the match.  If we
add a catch-all case to our match statement, we end up with a function
with a lower bound.

```ocaml
# let is_positive_permissive = function
     | `Int   x -> Ok (x > 0)
     | `Float x -> Ok (x > 0.)
     | _ -> Error "Unknown number type"
  ;;
val is_positive_permissive :
  [> `Float of float | `Int of int ] -> (bool, string) Core.Std._result =
  <fun>
# is_positive_permissive (`Int 0);;
- : (bool, string) Result.t = Ok false
# is_positive_permissive (`Ratio (3,4));;
- : (bool, string) Result.t = Error "Unknown number type"
```

Catch-all cases are error-prone even with ordinary variants, but they
are especially so with polymorphic variants.  That's because you have
no way of bounding what tags your function might have to deal with.
Such code is particularly vulnerable to typos.  For instance, if code
that uses `is_positive_permissive` passes in `Float` misspelled as
`Floot`, the erroneous code will compile without complaint.

```ocaml
# is_positive_permissive (`Floot 3.5);;
- : (bool, string) Result.t = Error "Unknown number type"
```

With ordinary variants, such a typo would have been caught as an
unknown tag.  As a general matter, one should be wary about
mixing catch-all cases and polymorphic variants.

</sidebar>

The code here is fragile in a different way, in that it's too
vulnerable to typos.  Let's consider how we might write this code as a
proper library, including a proper `mli`.  The interface might look
something like this:

```ocaml
(* file: terminal_color.mli *)

open Core.Std

type basic_color =
  [ `Black   | `Blue | `Cyan  | `Green
  | `Magenta | `Red  | `White | `Yellow ]

type color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB  of int * int * int ]

type extended_color =
  [ color
  | `RGBA of int * int * int * int ]

val color_to_int          : color -> int
val extended_color_to_int : extended_color -> int
```

Here, `extended_color` is defined as an explicit extension of
`color`.  Also, notice that we defined all of these types as exact
variants.   Now here's what the implementation might look like.

```ocaml
open Core.Std

type basic_color =
  [ `Black   | `Blue | `Cyan  | `Green
  | `Magenta | `Red  | `White | `Yellow ]

type color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB  of int * int * int ]

type extended_color =
  [ color
  | `RGBA of int * int * int * int ]

let basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

let color_to_int = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i

let extended_color_to_int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | `Grey x -> 2000 + x
  | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color
```

In this case, a change was made to `extended_color_to_int`, to add
special-case handling for the color gray, rather than using
`color_to_int`.  Unfortunately, `Gray` was misspelled as `Grey`, and
the compiler didn't complain.  It just inferred a bigger type for
`extended_color_to_int`, which happens to be compatible with the
`mli`, and so it compiles without incident.

If we add an explicit type annotation to the code itself (rather than
just in the mli), then the compiler has enough information to warn us.

```ocaml
let extended_color_to_int : extended_color -> int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | `Grey x -> 2000 + x
  | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color
```

In particular, the compiler will complain that the `` `Grey`` case as
unused.

```ocaml
File "terminal_color.ml", line 29, characters 4-11:
Warning 11: this match case is unused.
```

Once we have type definitions at our disposal, we can revisit the
question of how we write the pattern-match that narrows the type.  In
particular, we can explicitly use the type name as part of the pattern
match, by prefixing it with a `#`.

```ocaml
let extended_color_to_int : extended_color -> int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | #color as color -> color_to_int color
```

This is useful when you want to narrow down to a type whose definition
is long, and you don't want the verbosity of writing the tags down
explicitly in the match.

### When to use polymorphic variants

At first glance, polymorphic variants look like a strict improvement
over ordinary variants.  You can do everything that ordinary variants
can do, plus it's more flexible and more concise.  What's not to like?

In reality, regular variants are the more pragmatic choice most of the
time.  That's because the flexibility of polymorphic variants comes at
a price.  Here are some of the downsides.

- _Efficiency:_ This isn't a huge effect, but polymorphic variants are
  somewhat heavier than regular variants, and OCaml can't generate
  code for matching on polymorphic variants that is quite as efficient
  as what is generated for regular variants.
- _Error-finding:_ Polymorphic variants are type-safe, but the typing
  discipline that they impose is, by dint of its flexibility, less
  likely to catch bugs in your program.
- _Complexity:_ As we've seen, the typing rules for polymorphic
  variants are a lot more complicated than they are for regular
  variants.  This means that heavy use of polymorphic variants can
  leave you scratching your head trying to figure out why a given
  piece of code did or didn't compile.  It can also lead to absurdly
  long and hard to decode error messages.

All that said, polymorphic variants are still a useful and powerful
feature, but it's worth understanding their limitations, and how to
use them sensibly and modestly.

Probably the safest and most common use-case for polymorphic variants
is for cases where ordinary variants would be sufficient, but are
syntactically too heavyweight.  For example, you often want to create
a variant type for encoding the inputs or outputs to a function, where
it's not worth declaring a separate type for it.  Polymorphic variants
are very useful here, and as long as there are type annotations that
constrain these to have explicit, exact types, this tends to work
well.

Variants are most problematic exactly where you take full advantage of
their power; in particular, when you take advantage of the ability of
polymorphic variant types to overlap in the tags they support.  This
ties into OCaml's support for subtyping.  As we'll discuss further
when we cover objects in [xref](#object-oriented-programming),
subtyping brings in a lot of complexity, and most of the time, that's
complexity you want to avoid.

