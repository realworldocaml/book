open Printf
open Indent

type position = [ `Length | `Position of int | `End ]
type value = [ `Int of int | `Char of char ]

type 'a tree =
  [ `Node of (position * (value * 'a tree) list)
  | `Branch of ((position * value) list * 'a tree)
  | `Leaf of 'a ]

let group_by f l =
  let tbl = Hashtbl.create 20 in
  List.iter (
    fun x ->
      let k = f x in
      let r =
        try Hashtbl.find tbl k
        with Not_found ->
          let r = ref [] in
          Hashtbl.add tbl k r;
          r
      in
      r := x :: !r
  ) l;
  let l = Hashtbl.fold (fun k r l -> (k, List.rev !r) :: l) tbl [] in
  List.sort (fun (k1, _) (k2, _) -> compare k1 k2) l


let rec finish s pos =
  match pos with
    `End -> []
  | `Length ->
      (`Length, `Int (String.length s)) :: finish s (`Position 0)
  | `Position i ->
      if i < String.length s then
        (pos, `Char s.[i]) :: finish s (`Position (i+1))
      else
        finish s `End

let make_end_branch s pos x =
  match finish s pos with
    [] -> `Leaf x
  | l -> `Branch (l, `Leaf x)


(*
  Create branches where possible.
  As a result, all the nodes become part of a branch.
*)
let rec make_branches (x : 'a tree) : 'a tree =
  match x with
    `Leaf _ -> x
  | `Branch (l, x) ->
      (match make_branches x with
         `Branch (l2, x2) -> `Branch ((l @ l2), x2)
       | x -> `Branch (l, x))
  | `Node (pos, [ value, x ]) ->
      (match make_branches x with
         `Branch (l2, x2) -> `Branch (((pos, value) :: l2), x2)
       | x -> `Branch ([pos, value], x))
  | `Node (pos, l) ->
      `Node (pos, List.map (fun (value, x) -> (value, make_branches x)) l)


let make_initial_tree l : 'a tree =
  let rec aux i = function
      [] -> assert false
    | [ (s, x) ] ->
        let pos =
          if i < String.length s then `Position i
          else `End
        in
        make_end_branch s pos x

    | ((s, _) :: _) as l ->
        if i < String.length s then
          let groups = group_by (fun (s, _) -> `Char s.[i]) l in
          `Node (`Position i,
                 List.map (fun (k, l) -> (k, aux (i+1) l)) groups)
        else
          (* reached end of string but multiple strings remain *)
          invalid_arg (sprintf "String_match.make_tree: duplicate key %S" s)
  in
  match l with
    [] -> `Node (`Length, [])
  | [ (s, x) ] -> make_end_branch s `Length x
  | l ->
      let groups = group_by (fun (s, _) -> `Int (String.length s)) l in
      `Node (`Length,
             List.map (fun (k, l) -> (k, aux 0 l)) groups)

let make_tree l =
  make_branches (make_initial_tree l)



let test () =
  let l = [
    "abcdeg";
    "abcdef";
    "abdefh";
    "bcd";
    "";
  ]
  in
  make_tree (List.map (fun s -> (s, s)) l)


let get_value string_id pos_id pos =
  match pos with
    `Length -> "len"
  | `Position i ->
      if i = 0 then
        sprintf "String.unsafe_get %s %s" string_id pos_id
      else
        sprintf "String.unsafe_get %s (%s+%i)" string_id pos_id i
  | `End -> assert false

let make_pattern value =
  match value with
    `Int i -> string_of_int i
  | `Char c -> sprintf "%C" c

let cond test if_true if_false =
  [
    Line (sprintf "if %s then (" test);
    Block if_true;
    Line ")";
    Line "else (";
    Block if_false;
    Line ")";
  ]

let make_branch_test string_id pos_id = function
    (`Length, `Int n) -> sprintf "len = %i" n
  | (`Position i, `Char c) ->
      if i = 0 then
        sprintf "String.unsafe_get %s %s = %C" string_id pos_id c
      else
        sprintf "String.unsafe_get %s (%s+%i) = %C" string_id pos_id i c
  | _ -> assert false

let make_branch_tests string_id pos_id l =
  String.concat " && " (List.map (make_branch_test string_id pos_id) l)

let rec map_to_ocaml string_id pos_id e = function
    `Leaf expr -> expr

  | `Branch (l, x) ->
      cond (make_branch_tests string_id pos_id l)
        (map_to_ocaml string_id pos_id e x)
        e

  | `Node (pos, l) ->
      [
        Line (sprintf "match %s with" (get_value string_id pos_id pos));
        Block [
          Inline (List.map (make_case string_id pos_id e) l);
          Line "| _ -> (";
          Block [
            Block e;
            Line ")";
          ];
        ]
      ]

and make_case string_id pos_id e (value, tree) =
  Inline [
    Line (sprintf "| %s -> (" (make_pattern value));
    Block [
      Block (map_to_ocaml string_id pos_id e tree);
      Line ")";
    ];
  ]


type exit_with =
  [ `Exn of string
  | `Expr ]

let make_ocaml_expr_factored
    ?(string_id = "s")
    ?(pos_id = "pos")
    ?(exit_with = `Exn "Exit")
    ~error_expr
    cases : Indent.t list =

  let exit_expr, catch =
    match exit_with with
      `Expr -> error_expr, (fun x -> x)
    | `Exn error_exn ->
        let exit_expr = [ Line (sprintf "raise (%s)" error_exn) ] in
        let catch x =
          [
            Line "try";
            Block x;
            Line (sprintf "with %s -> (" error_exn);
            Block [
              Block error_expr;
              Line ")";
            ];
          ]
        in
        exit_expr, catch
  in
  match cases with
    [] -> error_expr
  | _ ->
      catch (map_to_ocaml string_id pos_id exit_expr (make_tree cases))

let test2 () =
  let l = [
    "abc";
    "abcd";
    "abde";
    "bcd";
    "";
  ]
  in
  let cases =
    List.map
      (fun s -> (s, [ Line (sprintf "Some `Case_%s" s) ]))
      l
  in
  let expr =
    make_ocaml_expr_factored
      ~error_expr:[ Line "None" ]
      cases
  in
  Indent.to_stdout (List.map Indent.strip expr)


let make_ocaml_expr_naive
    ?(string_id = "s")
    ~error_expr
    cases =
  let map (s, expr) =
    Inline [
      Line (sprintf "| %S ->" s);
      Block expr;
    ]
  in
  [
    Line (sprintf "match %s with" string_id);
    Block [
      Inline (List.map map cases);
      Line "| _ ->";
      Block error_expr;
    ]
  ]


let make_ocaml_expr
    ~optimized
    ?string_id
    ?pos_id
    ?exit_with
    ~error_expr
    cases : Indent.t list =

  if optimized then
    make_ocaml_expr_factored
      ?string_id ?pos_id ?exit_with ~error_expr cases
  else
    make_ocaml_expr_naive
      ?string_id ~error_expr cases


let make_ocaml_int_mapping
    ?(string_id = "s")
    ?(pos_id = "pos")
    ?(len_id = "len")
    ?exit_with
    ~error_expr1
    ?(error_expr2 = [ Line "assert false" ])
    ?(int_id = "i")
    cases : Indent.t list * Indent.t list =

  let a = Array.of_list cases in
  let int_cases =
    Array.mapi (fun i (s, _) -> (s, [ Line (string_of_int i) ])) a
  in
  let int_mapping_body =
    make_ocaml_expr_factored
      ~string_id
      ~pos_id
      ?exit_with
      ~error_expr: error_expr1
      (Array.to_list int_cases)
  in
  let int_mapping_function =
    [
      Line (sprintf "fun %s %s %s ->" string_id pos_id len_id);
      Block [
        Line (
          sprintf "if %s < 0 || %s < 0 || %s + %s > String.length %s then"
            pos_id len_id pos_id len_id string_id
        );
        Block [
          Line "invalid_arg \"out-of-bounds substring position or length\";";
        ];
        Inline int_mapping_body;
      ];
    ]
  in
  let int_matching_cases =
    Array.mapi (
      fun i (_, x) ->
        Inline [
          Line (sprintf "| %i ->" i);
          Block x;
        ]
    ) a
  in
  let int_matching =
    [
      Line (sprintf "match %s with" int_id);
      Block [
        Inline (Array.to_list int_matching_cases);
        Line "| _ -> (";
        Block [
          Block error_expr2;
          Line ")";
        ];
      ];
    ]
  in
  int_mapping_function, int_matching
