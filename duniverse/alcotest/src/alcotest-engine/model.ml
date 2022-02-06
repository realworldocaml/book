open! Import

type speed_level = [ `Quick | `Slow ]

module Test_name : sig
  type t

  val v : name:string -> index:int -> t
  val name : t -> Safe_string.t
  val index : t -> int

  val pp : t Fmt.t
  (** Pretty-print the unescaped test-case name *)

  val file : t -> string
  (** An escaped form of the test name with [.output] suffix. *)

  val length : t -> int
  (** The approximate number of terminal columns consumed by [pp_name]. *)

  val compare : t -> t -> int
  (** Order lexicographically by name, then by index. *)
end = struct
  type t = { name : Safe_string.t; file : string; index : int }

  let index { index; _ } = index

  let v ~name ~index =
    let name = Safe_string.v name in
    let file =
      let name =
        match Safe_string.to_string name with "" -> "" | n -> n ^ "."
      in
      Fmt.str "%s%03d.output" name index
    in
    { name; file; index }

  let pp = Fmt.using (fun { name; _ } -> name) Safe_string.pp
  let name { name; _ } = name
  let file { file; _ } = file
  let length = name >> Safe_string.length

  let compare t t' =
    match Safe_string.compare t.name t'.name with
    | 0 -> (compare : int -> int -> int) t.index t'.index
    | n -> n
end

module Run_result = struct
  type t =
    [ `Ok
    | `Exn of Test_name.t * string * unit Fmt.t
    | `Error of Test_name.t * unit Fmt.t
    | `Skip
    | `Todo of string ]

  (** [is_failure] holds for test results that are error states. *)
  let is_failure : t -> bool = function
    | `Ok | `Skip -> false
    | `Error _ | `Exn _ | `Todo _ -> true
end

module Suite (M : Monad.S) : sig
  type 'a t
  type 'a test_fn = [ `Skip | `Run of 'a -> Run_result.t M.t ]

  type 'a test_case = {
    name : Test_name.t;
    speed_level : speed_level;
    fn : 'a test_fn;
  }

  val v : name:string -> (_ t, [> `Empty_name ]) result
  (** Construct a new suite, given a non-empty [name]. Test cases must be added
      with {!add}. *)

  val name : _ t -> string
  (** An escaped form of the suite name. *)

  val pp_name : _ t Fmt.t
  (** Pretty-print the unescaped suite name. *)

  val add :
    'a t ->
    Test_name.t * string * speed_level * 'a test_fn ->
    ('a t, [ `Duplicate_test_path of string ]) result

  val tests : 'a t -> 'a test_case list
  val doc_of_test_name : 'a t -> Test_name.t -> string
end = struct
  module String_set = Set.Make (String)

  type 'a test_fn = [ `Skip | `Run of 'a -> Run_result.t M.t ]

  type 'a test_case = {
    name : Test_name.t;
    speed_level : speed_level;
    fn : 'a test_fn;
  }

  type 'a t = {
    name : Safe_string.t;
    tests : 'a test_case list;
    (* caches computed from the library values. *)
    filepaths : String_set.t;
    doc : (Test_name.t, string) Hashtbl.t;
  }

  let v ~name =
    match String.length name with
    | 0 -> Error `Empty_name
    | _ ->
        let name = Safe_string.v name in
        let tests = [] in
        let filepaths = String_set.empty in
        let doc = Hashtbl.create 0 in
        Ok { name; tests; filepaths; doc }

  let name { name; _ } = Safe_string.to_string name
  let pp_name ppf { name; _ } = Safe_string.pp ppf name

  let check_path_is_unique t tname =
    match String_set.mem (Test_name.file tname) t.filepaths with
    | false -> Ok ()
    | true -> Error (`Duplicate_test_path (Fmt.to_to_string Test_name.pp tname))

  let add t (tname, doc, speed_level, fn) =
    match check_path_is_unique t tname with
    | Error _ as e -> e
    | Ok () ->
        let tests = { name = tname; speed_level; fn } :: t.tests in
        let filepaths = String_set.add (Test_name.file tname) t.filepaths in
        Hashtbl.add t.doc tname doc;
        Ok { t with tests; filepaths }

  let tests t = List.rev t.tests

  let doc_of_test_name t path =
    try Hashtbl.find t.doc path with Not_found -> ""
end
