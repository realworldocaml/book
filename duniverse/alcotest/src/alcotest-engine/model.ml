open Utils

type speed_level = [ `Quick | `Slow ]

(** Given a UTF-8 encoded string, escape any characters not considered
    "filesystem safe" as their [U+XXXX] notation form. *)
let escape str =
  let add_codepoint buf uchar =
    Uchar.to_int uchar |> Fmt.str "U+%04X" |> Buffer.add_string buf
  in
  let buf = Buffer.create (String.length str * 2) in
  let get_normalized_char _ _ u =
    match u with
    | `Uchar u ->
        if Uchar.is_char u then
          match Uchar.to_char u with
          | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | ' ' | '.') as c
            ->
              Buffer.add_char buf c
          | _ -> add_codepoint buf u
        else add_codepoint buf u
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep
  in
  Uutf.String.fold_utf_8 get_normalized_char () str;
  Buffer.contents buf

module Test_name : sig
  type t

  val v : name:string -> index:int -> t
  val name : t -> string
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
  type t = { name : string; file : string; index : int }

  let index { index; _ } = index

  let v ~name ~index =
    let file =
      let name = name |> escape |> function "" -> "" | n -> n ^ "." in
      Fmt.str "%s%03d.output" name index
    in
    { name; file; index }

  let pp = Fmt.using (fun { name; _ } -> name) Fmt.string
  let name { name; _ } = name
  let file { file; _ } = file
  let length = name >> Uutf.String.fold_utf_8 (fun a _ _ -> a + 1) 0

  let compare t t' =
    match String.compare t.name t'.name with
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

  type 'a test_case = {
    name : Test_name.t;
    speed_level : speed_level;
    fn : 'a -> Run_result.t M.t;
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
    Test_name.t * string * speed_level * ('a -> Run_result.t M.t) ->
    ('a t, [ `Duplicate_test_path of string ]) result

  val tests : 'a t -> 'a test_case list
  val doc_of_test_name : 'a t -> Test_name.t -> string
end = struct
  module String_set = Set.Make (String)

  type 'a test_case = {
    name : Test_name.t;
    speed_level : speed_level;
    fn : 'a -> Run_result.t M.t;
  }

  type 'a t = {
    escaped_name : string;
    pp_name : unit Fmt.t;
    tests : 'a test_case list;
    (* caches computed from the library values. *)
    filepaths : String_set.t;
    doc : (Test_name.t, string) Hashtbl.t;
  }

  let v ~name =
    match String.length name with
    | 0 -> Error `Empty_name
    | _ ->
        let escaped_name = escape name in
        let pp_name = Fmt.(const string) name in
        let tests = [] in
        let filepaths = String_set.empty in
        let doc = Hashtbl.create 0 in
        Ok { escaped_name; pp_name; tests; filepaths; doc }

  let name { escaped_name; _ } = escaped_name
  let pp_name ppf { pp_name; _ } = pp_name ppf ()

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
