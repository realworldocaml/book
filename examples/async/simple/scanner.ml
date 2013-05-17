module Scanner : sig
  type t
  val empty : t
  (* return a deferred that becomes determined as soon as a line is
     read that satisfied the condition in question *)
  val add_condition : t -> (string -> bool) -> t * string Deferred.t
  val run : t -> Reader.t -> unit Deferred.t
end = struct
  type t = (string -> bool * string Ivar.t) list

  let empty = []

  let add_condition t cond =
    let ivar = Ivar.create () in
    let t' = (cond,ivar) :: t in
    (t',Ivar.read ivar)

  let run t r =
    Pipe.fold (Reader.lines r) ~init:t ~f:(fun t line ->
      List.filter t ~f:(fun (cond,ivar) ->
        if cond line
        then (Ivar.fill ivar line; false)
        else true))
end
;;
