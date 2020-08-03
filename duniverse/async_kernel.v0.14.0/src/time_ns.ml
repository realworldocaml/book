include Core_kernel.Core_kernel_private.Time_ns_alternate_sexp

(* [after] is like [add], but deals nicely with the case of overflow by instead returning
   [max_value].  Time-source functions use [after] to avoid immediately firing events that
   should never fire, due to the overflow leading to a negative time that appears to be in
   the past.  We don't check underflow because that is very unlikely, requiring both a
   negative time and a negative span. *)
let after t span =
  let result = add t span in
  if Span.( > ) span Span.zero && result < t then max_value_for_1us_rounding else result
;;
