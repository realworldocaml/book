(** {x This is bad markup} *)
val x : int

(** {9 Bad hading level} *)
val x : int

(* {4 Heading} this should be on it's own line *)
val x : int

(** {ul {limust be followed by whitespace}} *)
val x : int

(** {limust in a ul} *)
val x : int

(** {vmust be followed by whitespace v} *)
val x : int

(** {v must be preceded by whitespacev} *)
val x : int

(** @ stray *)
val x : int

(** Expect something on the same line: *)
val x : int

(** @before *)
val x : int

(** @param *)
val x : int

(** @raise *)
val x : int

(** @see *)
val x : int

(** @UnknownTag *)
val x : int

(** } unpaired *)
val x : int

(** ] unpaired *)
val x : int

(** {%invalid: raw markup target %} *)
val x : int

(** This comment has bad
    } markup on the second line. *)
val x : int

(** {x bad markup} in a standalone comment. *)
