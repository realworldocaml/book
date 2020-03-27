type filename = string

let has_suffix suffix name =
  Filename.check_suffix name suffix

(* [groups eq xs] segments the list [xs] into a list of groups, where several
   consecutive elements belong in the same group if they are equivalent in the
   sense of the function [eq]. *)

(* The auxiliary function [groups1] deals with the case where we have an open
   group [group] of which [x] is a member. The auxiliary function [group0]
   deals with the case where we have no open group. [groups] is the list of
   closed groups found so far, and [ys] is the list of elements that remain to
   be examined. *)

let rec groups1 eq groups x group ys =
  match ys with
  | [] ->
      group :: groups
  | y :: ys ->
      if eq x y then
        groups1 eq groups x (y :: group) ys
      else
        groups0 eq (List.rev group :: groups) (y :: ys)

and groups0 eq groups ys =
  match ys with
  | [] ->
      groups
  | y :: ys ->
      groups1 eq groups y [y] ys

let groups eq (xs : 'a list) : 'a list list =
  List.rev (groups0 eq [] xs)

(* [chop_numeric_suffix s] removes any numeric suffix off the string [s]. *)

let numeric_suffix =
  Str.regexp "[0-9]*$"

let chop_numeric_suffix s =
  let offset = Str.search_forward numeric_suffix s 0 in
  String.sub s 0 offset

let equal_up_to_numeric_suffix s1 s2 =
  chop_numeric_suffix s1 = chop_numeric_suffix s2

let dash_numeric_suffix =
  Str.regexp "-[0-9]*$"

let chop_dash_numeric_suffix s =
  let offset = Str.search_forward dash_numeric_suffix s 0 in
  String.sub s 0 offset
