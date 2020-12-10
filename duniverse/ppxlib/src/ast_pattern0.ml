open! Import

exception Expected of Location.t * string

let fail loc expected =
  raise (Expected (loc, expected))
;;

type context =
  { (* [matched] counts how many constructors have been matched. This is used to find what
       pattern matches the most some piece of ast in [Ast_pattern.alt]. In the case where
       all branches fail to match, we report the error from the one that matches the
       most.

       This is only incremented by combinators that can fail. *)
    mutable matched : int
  }

type ('matched_value, 'k, 'k_result) t =
    T of (context -> Location.t -> 'matched_value -> 'k -> 'k_result)
