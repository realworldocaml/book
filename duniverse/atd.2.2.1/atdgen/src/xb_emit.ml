(*
  Tools shared between code generators for the biniou serialization format.
  (xb means X-Biniou)
*)

open Atd.Import
open Mapping

type 'a expr = ('a, Biniou.biniou_repr) Mapping.mapping
type 'a def = ('a, Biniou.biniou_repr) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type name = (loc * string)

type names = {
  field_names : name list list;
  variant_names : name list list;
}

let rec extract_names_from_expr acc (x : 'a expr) =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float  _
  | String _ -> acc
  | Sum (_, va, _, _) ->
      let l, (fn, vn) =
        Array.fold_left extract_names_from_variant ([], acc) va
      in
      (fn, List.rev l :: vn)

  | Record (_, fa, _, _) ->
      let l, (fn, vn) =
        Array.fold_left extract_names_from_field ([], acc) fa
      in
      (List.rev l :: fn, vn)

  | Tuple (_, ca, _, _) ->
      Array.fold_left extract_names_from_cell acc ca

  | List (_, x, _, _)
  | Option (_, x, _, _)
  | Nullable (_, x, _, _)
  | Wrap (_, x, _, _) ->
      extract_names_from_expr acc x

  | Name (_, _, l, _, _) ->
      List.fold_left extract_names_from_expr acc l

  | External (_, _, l, _, _) ->
      List.fold_left extract_names_from_expr acc l

  | Tvar _ -> acc

and extract_names_from_variant (l, acc) x =
  let l = (x.var_loc, x.var_cons) :: l in
  match x.var_arg with
      None -> (l, acc)
    | Some x ->
        (l, extract_names_from_expr acc x)

and extract_names_from_field (l, acc) x =
  let l = (x.f_loc, x.f_name) :: l in
  (l, extract_names_from_expr acc x.f_value)

and extract_names_from_cell acc x =
  extract_names_from_expr acc x.cel_value


let extract_ocaml_names_from_defs l =
  let fn, vn =
    List.fold_left (
      fun acc def ->
        match def.def_value with
            None -> acc
          | Some x -> extract_names_from_expr acc x
    ) ([], []) l
  in
  {
    field_names = List.rev fn;
    variant_names = List.rev vn;
  }

let check_duplicate_hashes kind l =
  let tbl = Hashtbl.create 100 in
  List.iter (
    fun (loc, s) ->
      let h = Bi_io.hash_name s in
      try
        let loc0, s0 = Hashtbl.find tbl h in
        Error.error2
          loc0 (sprintf "Definition of %s %s." kind s0)
          loc (
            sprintf "\
Definition of %s %s.

Both %s and %s have the same hash %i which
makes them indistinguishable once in the Biniou format.
Use different names."
              kind s
              s0 s h
          )

      with Not_found ->
        Hashtbl.add tbl h (loc, s)
  ) l

let check_hashes x =
  List.iter (check_duplicate_hashes "record field name") x.field_names;
  List.iter (check_duplicate_hashes "variant name") x.variant_names

let check (l : 'a grouped_defs) =
  extract_ocaml_names_from_defs (List.concat_map snd l)
  |> check_hashes

(*
let find_clashes () =
  let l = Mikmatch.Text.lines_of_file "/tmp/dictionary.txt" in
(*
  let l1 = List.rev_map (fun s -> s ^ "1") l in
  let l2 = List.rev_map (fun s -> s ^ "2") l in
  let l3 = List.rev_map (fun s -> s ^ "3") l in
  let l4 = List.rev_map (fun s -> s ^ "4") l in
  let l = List.flatten [l; l1; l2; l3; l4] in
*)
  let tbl = Hashtbl.create (2 * List.length l) in
  List.iter (
    fun s ->
      let h = Bi_io.hash_name s in
      let r =
        try Hashtbl.find tbl h
        with Not_found ->
          let r = ref [] in
          Hashtbl.add tbl h r;
          r
      in
      r := s :: !r
  ) l;
  let clashes =
    Hashtbl.fold (
      fun h r acc ->
        let l = !r in
        if List.length l >= 2 then
        List.rev l :: acc
        else
          acc
    ) tbl []
  in
  let clashes = List.sort compare clashes in
  List.iter (fun l -> print_endline (String.concat " " l)) clashes
*)

(*
Groups of words with identical biniou hashes obtained with find_clashes:

bind1 classroom's3
bind2 classroom's4
commutes1 funerals4
expect1 tantalus4
idea chaw2
interval's1 middling2
interval's2 middling3
interval's3 middling4
militarily1 scheduled4
overviews neglects3
shea crew2
vacating maxine3
workshop1 examples3
workshop2 examples4

bevel reconveyed
cogitate jutties
premiums squigglier
representationalists supervene
*)
