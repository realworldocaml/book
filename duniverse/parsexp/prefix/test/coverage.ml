open! Core
open! Import
include Coverage_intf

module type Seen = sig
  type t

  include Intable.S with type t := t

  (** [of_int_exn t < cardinality] for all [t]. *)
  val cardinality : int
end

module Seen : sig
  type 'a t

  val create : (module Seen with type t = 'a) -> 'a t
  val add : 'a t -> 'a -> unit
  val get_unseen : 'a t -> 'a list
end = struct
  type 'a t =
    { of_int_exn : int -> 'a
    ; seen : bool array
    ; to_int_exn : 'a -> int
    }

  let create (type a) (module M : Seen with type t = a) =
    { of_int_exn = M.of_int_exn
    ; seen = Array.create false ~len:M.cardinality
    ; to_int_exn = M.to_int_exn
    }
  ;;

  let add t a = t.seen.(t.to_int_exn a) <- true

  let get_unseen t =
    Array.filter_mapi t.seen ~f:(fun i was_seen ->
      match was_seen with
      | true -> None
      | false -> Some (t.of_int_exn i))
    |> Array.to_list
  ;;
end

let with_prefix_coverage full_string ~f =
  let module Prefixes_seen = struct
    type t = string

    let cardinality = String.length full_string + 1 (* "" is a prefix too *)

    let of_int_exn len = String.subo full_string ~len
    let to_int_exn = String.length
  end
  in
  let seen = Seen.create (module Prefixes_seen) in
  let saw_prefix ~prefix =
    match String.is_prefix full_string ~prefix with
    | false -> raise_s [%message "Not a prefix." prefix (full_string : string)]
    | true -> Seen.add seen prefix
  in
  let get_unseen () =
    List.filter (Seen.get_unseen seen) ~f:(function
      | "" ->
        (* For unquoted atoms, we never emit the empty prefix because we only know the
           atom has begun when we see the first character. *)
        false
      | unseen ->
        (match unseen.[String.length unseen - 1] with
         (* Some prefixes we don't emit because we don't know how to decode them until we
            see the next character. *)
         | '#' | '|' -> false (* block comments *)
         | _ -> true))
  in
  f ({ f = saw_prefix } : Saw_prefix.t);
  match get_unseen () with
  | [] -> ()
  | _ :: _ as unseen_prefixes ->
    raise_s [%message (unseen_prefixes : string list) (full_string : string)]
;;

let with_state_coverage ~f =
  let module State = Parsexp_symbolic_automaton.Automaton.State in
  let module States_seen = struct
    type t = int

    let cardinality = List.length [%all: State.t]
    let of_int_exn = Fn.id
    let to_int_exn = Fn.id
  end
  in
  let seen = Seen.create (module States_seen) in
  f ({ f = (fun state -> Seen.add seen state.automaton_state) } : Saw_state.t);
  match Seen.get_unseen seen with
  | [] -> ()
  | _ :: _ as unseen_states ->
    let unseen_states = List.map unseen_states ~f:State.of_int in
    raise_s [%message (unseen_states : State.t list)]
;;
