(** Basic Myers diff algorithm, translated from GNU diff. **)

(** [iter_matches ?cutoff ~f ~hashable a b] diffs the arrays [a] and [b] (as in
    /usr/bin/diff), and calls [f] on each element of the longest common subsequence in
    increasing order. The arguments of [f] are the indices in [a] and [b], respectively,
    of that element.

    The [cutoff] is an upper bound on the minimum edit distance between [a] and [b]. When
    [cutoff] is exceeded, [iter_matches] returns a correct, but not necessarily minimal
    diff. It defaults to about [sqrt (Array.length a + Array.length b)]. *)
val iter_matches
  :  ?cutoff:int
  -> f:(int * int -> unit)
  -> hashable:'a Base.Hashtbl.Key.t
  -> 'a array
  -> 'a array
  -> unit
