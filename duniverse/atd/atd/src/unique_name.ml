(*
   Ensure unique identifiers that don't conflict with predefined Python
   identifiers or prefixes reserved by atdpy.
*)

open Printf

type t = {
  reserved_identifiers: (string, unit) Hashtbl.t;
  reserved_prefixes: string list;
  safe_prefix: string;

  (* Translations are used to look up the translation of an already-registered
     identifier. *)
  translations: (string, string) Hashtbl.t;

  (* Reverse translations are needed when creating a new translation.
     They're for checking that the new translation doesn't conflict with
     an existing translation. *)
  reverse_translations: (string, string) Hashtbl.t;
}

let has_prefix ~prefix src =
  let len = String.length prefix in
  if String.length src < len then
    false
  else
    try
      for i = 0 to len - 1 do
        if prefix.[i] <> src.[i] then
          raise Exit
      done;
      true
    with Exit -> false

let init ~reserved_identifiers ~reserved_prefixes ~safe_prefix =
  let reserved_identifiers =
    let tbl = Hashtbl.create 100 in
    List.iter (fun id -> Hashtbl.replace tbl id ()) reserved_identifiers;
    tbl
  in
  List.iter (fun prefix ->
    if has_prefix ~prefix safe_prefix then
      invalid_arg
        (sprintf "Unique_name.init: safe_prefix %S is not safe as it \
                  conflicts with reserved prefix %S"
           safe_prefix prefix)
  ) reserved_prefixes;
  {
    reserved_identifiers;
    reserved_prefixes;
    safe_prefix;
    translations = Hashtbl.create 100;
    reverse_translations = Hashtbl.create 100;
  }

let is_reserved env src_or_dst =
  Hashtbl.mem env.reserved_identifiers src_or_dst

let conflicts_with_existing_translation env dst =
  Hashtbl.mem env.reverse_translations dst

let has_reserved_prefix env src =
  List.exists (fun prefix -> has_prefix ~prefix src) env.reserved_prefixes

let enumerate_suffixes () =
  let counter = ref 0 in
  let get_suffix () =
    let suf =
      match !counter with
      | 0 -> ""
      | 1 -> "_"
      | n -> string_of_int n
    in
    incr counter;
    suf
  in
  get_suffix

let register env src =
  let get_suffix = enumerate_suffixes () in
  let rec find_available_suffix () =
    let suffix = get_suffix () in
    let dst = src ^ suffix in
    let dst =
      (* assume that safe_prefix is not a prefix of a reserved prefix *)
      if has_reserved_prefix env dst then
        env.safe_prefix ^ dst
      else
        dst
    in
    if is_reserved env dst || conflicts_with_existing_translation env dst then
      find_available_suffix ()
    else
      dst
  in
  let dst = find_available_suffix () in
  Hashtbl.add env.translations src dst;
  Hashtbl.add env.reverse_translations dst src;
  dst

let translate_only env src =
  Hashtbl.find_opt env.translations src

let translate env src =
  match translate_only env src with
  | Some dst -> dst
  | None -> register env src

let reverse_translate env dst =
  Hashtbl.find_opt env.reverse_translations dst

let create env src =
  let get_suffix = enumerate_suffixes () in
  let rec find_available_suffix () =
    let suffix = get_suffix () in
    let src = src ^ suffix in
    if Hashtbl.mem env.translations src then
      find_available_suffix ()
    else
      src
  in
  let src = find_available_suffix () in
  ignore (register env src);
  src

let all env =
  Hashtbl.fold (fun src dst acc -> (src, dst) :: acc) env.translations []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
