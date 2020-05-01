/******************************************************************************/
/*                                                                            */
/*                                   Menhir                                   */
/*                                                                            */
/*                       François Pottier, Inria Paris                        */
/*              Yann Régis-Gianas, PPS, Université Paris Diderot              */
/*                                                                            */
/*  Copyright Inria. All rights reserved. This file is distributed under the  */
/*  terms of the GNU Library General Public License version 2, with a         */
/*  special exception on linking, as described in the file LICENSE.           */
/*                                                                            */
/******************************************************************************/

(* This is menhir's standard library. It offers a number of
   parameterized nonterminal definitions, such as options and lists,
   that should be useful in a number of circumstances. *)

%%

(* ------------------------------------------------------------------------- *)
(* The identity. *)

(* [endrule(X)] is the same as [X]. *)

(* This allows placing an anonymous subrule in the middle of a rule, as in:

     cat
     endrule(dog { action1 })
     cow
     { action2 }

   Because [endrule] is marked %inline, everything is expanded away. So,
   this is equivalent to:

     cat dog cow { action1; action2 }

   Note that [action1] moves to the end of the rule. The anonymous subrule
   can even have several branches, as in:

     cat
     endrule(dog { action1a } | fox { action1b })
     cow
     { action2 }

   This is expanded to:

     cat dog cow { action1a; action2 }
   | cat fox cow { action1b; action2 }

*)

%public %inline endrule(X):
x = X
    { x }

(* [anonymous(X)] is a deprecated synonym for [endrule(X)].
   It was never documented. *)

%public %inline anonymous(X):
x = X
    { x }

(* [midrule(X)] is the same as [X]. *)

(* This allows placing an anonymous subrule in the middle of a rule, as in:

     cat
     midrule(dog { action1 })
     cow
     { action2 }

   Because [midrule] is not marked %inline, this is equivalent to:

     cat xxx cow { action2 }

   where the fresh nonterminal symbol [xxx] is separately defined by:

     xxx: dog { action1 }

   In particular, if there is no [dog], what we get is a semantic action
   embedded in the middle of a rule. For instance,

     cat midrule({ action1 }) cow { action2 }

   is equivalent to:

     cat xxx cow { action2 }

   where [xxx] is separately defined by the rule:

     xxx: { action1 }

*)

%public midrule(X):
x = X
    { x }

(* [embedded(X)] is a deprecated synonym for [midrule(X)].
   It was never documented. *)

%public embedded(X):
x = X
    { x }

(* ------------------------------------------------------------------------- *)
(* Options. *)

(* [option(X)] recognizes either nothing or [X]. It produces a value
   of type ['a option] if [X] produces a value of type ['a]. *)

%public option(X):
  /* nothing */
    { None }
| x = X
    { Some x }

(* [ioption(X)] is identical to [option(X)], except its definition is
   inlined. This has the effect of duplicating the production that
   refers to it, possibly eliminating an LR(1) conflict. *)

%public %inline ioption(X):
  /* nothing */
    { None }
| x = X
    { Some x }

(* [boption(X)] recognizes either nothing or [X]. It produces a value
   of type [bool]. *)

%public boption(X):
  /* nothing */
    { false }
| X
    { true }

(* [loption(X)] recognizes either nothing or [X]. It produces a value
   of type ['a list] if [X] produces a value of type ['a list]. *)

%public loption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* ------------------------------------------------------------------------- *)
(* Sequences. *)

(* [epsilon] recognizes the empty word. It can be used instead of the
   traditional /* empty */ comment. *)

(* NOT YET ADDED because we first need to remove the limitation that
   every symbol must be reachable from the start symbol!

%public %inline epsilon:
  /* empty */
    { () }

 *)

(* [pair(X, Y)] recognizes the sequence [X Y]. It produces a value of
   type ['a * 'b] if [X] and [Y] produce values of type ['a] and ['b],
   respectively. *)

%public %inline pair(X, Y):
  x = X; y = Y
    { (x, y) }

(* [separated_pair(X, sep, Y)] recognizes the sequence [X sep Y]. It
   produces a value of type ['a * 'b] if [X] and [Y] produce values of
   type ['a] and ['b], respectively. *)

%public %inline separated_pair(X, sep, Y):
  x = X; sep; y = Y
    { (x, y) }

(* [preceded(opening, X)] recognizes the sequence [opening X]. It
   passes on the value produced by [X], so that it produces a value of
   type ['a] if [X] produces a value of type ['a]. *)

%public %inline preceded(opening, X):
  opening; x = X
    { x }

(* [terminated(X, closing)] recognizes the sequence [X closing]. It
   passes on the value produced by [X], so that it produces a value of
   type ['a] if [X] produces a value of type ['a]. *)

%public %inline terminated(X, closing):
  x = X; closing
    { x }

(* [delimited(opening, X, closing)] recognizes the sequence [opening X
   closing]. It passes on the value produced by [X], so that it
   produces a value of type ['a] if [X] produces a value of type
   ['a]. *)

%public %inline delimited(opening, X, closing):
  opening; x = X; closing
    { x }

(* ------------------------------------------------------------------------- *)
(* Lists. *)

(* [list(X)] recognizes a possibly empty list of [X]'s. It produces a
   value of type ['a list] if [X] produces a value of type ['a]. The
   front element of the list is the first element that was parsed. *)

%public list(X):
  /* nothing */
    { [] }
| x = X; xs = list(X)
    { x :: xs }

(* [nonempty_list(X)] recognizes a nonempty list of [X]'s. It produces
   a value of type ['a list] if [X] produces a value of type ['a]. The
   front element of the list is the first element that was parsed. *)

%public nonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = nonempty_list(X)
    { x :: xs }

(* [separated_list(separator, X)] recognizes a possibly empty list of
   [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public %inline separated_list(separator, X):
  xs = loption(separated_nonempty_list(separator, X))
    { xs }

(* [separated_nonempty_list(separator, X)] recognizes a nonempty list
   of [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = separated_nonempty_list(separator, X)
    { x :: xs }

(* ------------------------------------------------------------------------- *)
(* List manipulation and transformation. *)

(* [rev(XS)] recognizes the same language as [XS], but reverses the resulting
   OCaml list. (20181005) *)

%public %inline rev(XS):
  xs = XS
    { List.rev xs }

(* [flatten(XSS)] recognizes the same language as [XSS], and flattens the
   resulting OCaml list of lists. (20181005) *)

%public %inline flatten(XSS):
  xss = XSS
    { List.flatten xss }

(* [append(XS, YS)] recognizes [XS YS], and appends (concatenates) the
   resulting OCaml lists. (20181005) *)

%public %inline append(XS, YS):
  xs = XS ys = YS
    { xs @ ys }

%%
