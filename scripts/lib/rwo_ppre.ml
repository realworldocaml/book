open Core
open Rwo_core2
open Async
module Html = Rwo_html
module Import = Rwo_import
module Lang = Rwo_lang
let (/) = Filename.concat

type code_item = [
| `Output of string
| `Prompt of string
| `Input of string
| `Data of string
]

type code_block = code_item list

type pre = {
  data_type : string;
  data_code_language : string option;
  code_block : code_block;
}

type p = {
  a_href : string;
  a_class : string;
  em_data : string;
  data1 : string option;
  part : Import.part option;
  a2 : Rwo_html.item option;
  a3 : Rwo_html.item option;
}

type t = {p:p; pre:pre}

(* Replace ampersand encoded HTML sub-strings with their ASCII
    counterparts. *)
let decode_html_string (s:string) : string =
  s
  |> String.substr_replace_all ~pattern:"&amp;" ~with_:"&"
  |> String.substr_replace_all ~pattern:"&lt;" ~with_:"<"
  |> String.substr_replace_all ~pattern:"&gt;" ~with_:">"

(* Extract data from an HTML node. Be tolerant and accept some
    non-DATA nodes. *)
let rec item_to_string (item:Html.item) : string Or_error.t =
  let open Result.Monad_infix in
  match item with
  | `Data x -> Ok x
  | `Element {name="em"; attrs=[]; childs} -> (
    Result.List.map childs ~f:item_to_string
    >>| String.concat ~sep:""
  )
  | `Element _ ->
    error "cannot treat node as data" item Html.sexp_of_item

(* Like [item_to_string but for list of items. *)
let items_to_string (items : Html.item list) : string Or_error.t =
  let open Result.Monad_infix in
  Result.List.map items ~f:item_to_string
  >>| String.concat ~sep:""

let parse_p_before_pre (item:Html.item) : p Or_error.t =
  let open Result.Monad_infix in

  (* Return DATA under <em>. *)
  let parse_em item : string Or_error.t =
    match item with
    | `Element {
      Html.name="em";
      attrs=["class","hyperlink"];
      childs=[`Data em_data]}
      ->
      Ok em_data
    | _ ->
      error "unexpected form for <em> node" item Html.sexp_of_item
  in

  (* Return href and class attribute values of main <a>, and the data
      under the <em> child of it. *)
  let parse_main_a item : (string * string * string) Or_error.t =
    match item with
    | `Element {Html.name="a"; attrs; childs=[em]} -> (
      let find x = Option.value_exn (List.Assoc.find ~equal:String.equal attrs x) in
      Html.check_attrs attrs ~required:["href";"class"] ~allowed:(`Some [])
      >>= fun () ->
      parse_em em >>= fun em_data ->
      Ok (find "href", find "class", em_data)
    )
    | _ ->
      error "unexpected form for main <a> node" item Html.sexp_of_item
  in

  (* The 2nd data field, i.e. the one after the main <a>, optionally
      contains the part number. Parse it out. *)
  let parse_data2 (x : string option) : string option Or_error.t =
    match x with
    | None -> Ok None
    | Some x -> (
      try
        Ok (Some (Scanf.sscanf (String.strip x) "(part %s)" ident))
      with
        exn ->
          error "error parsing part name from data2 node"
            (x, exn) [%sexp_of: string * exn]
    )
  in

  let values = match item with
    | `Element {Html.name="p"; attrs=[]; childs=main_a::[]} ->
      Ok (main_a, None, None, None, None)
    | `Element {Html.name="p"; attrs=[]; childs=main_a::(`Data data2)::[]} ->
      Ok (main_a, None, Some data2, None, None)
    | `Element {Html.name="p"; attrs=[]; childs=(`Data data1)::main_a::[]} ->
      Ok (main_a, Some data1, None, None, None)
    | `Element {
      Html.name="p";
      attrs=[];
      childs=(`Data data1)::main_a::(`Data data2)::[];
    } ->
      Ok (main_a, Some data1, Some data2, None, None)
    | `Element {
      Html.name="p";
      attrs=[];
      childs=main_a::(`Data data2)::((`Element{name="a";_}) as a2)::[];
    } ->
      Ok (main_a, None, Some data2, Some a2, None)
    | `Element {
      Html.name="p";
      attrs=[];
      childs =
        main_a
        ::(`Data data2)
        ::((`Element{name="a";_}) as a2)
        ::((`Element{name="a";_}) as a3)
        ::[]
      ;
    } ->
      Ok (main_a, None, Some data2, Some a2, Some a3)
    | _ ->
      error "unexpected format of <p> before <pre>"
        item Html.sexp_of_item
  in
  values >>= fun (main_a,data1,data2,a2,a3) ->
  parse_data2 data2 >>= fun part ->
  parse_main_a main_a >>= fun (a_href,a_class,em_data) ->
  Ok {a_href;a_class;em_data;data1;part;a2;a3}


(* Parse <code> element, requiring exactly the given attributes. *)
let parse_code_helper expected_attrs item : string Or_error.t =
  match item with
  | `Data _ ->
    error "expected <code> but got DATA"
      (expected_attrs,item) [%sexp_of: Html.attributes * Html.item]
  | `Element {Html.name="code"; attrs; childs} -> (
    if attrs = expected_attrs then
      Result.map (items_to_string childs) ~f:decode_html_string
    else
      error "expected attributes differ from the ones found"
        (expected_attrs, item)
        [%sexp_of:Html.attributes * Html.item]
  )
  | `Element _ ->
    error "expected <code> but got other type of node"
      (expected_attrs,item) [%sexp_of: Html.attributes * Html.item]

(* Parse <code> element. *)
let parse_code item : string Or_error.t =
  parse_code_helper [] item

(* Parse <code class="prompt"> element. *)
let parse_code_prompt item : [> `Prompt of string] Or_error.t =
  parse_code_helper ["class","prompt"] item
  |> Result.map ~f:(fun x -> `Prompt x)

(* Parse <code class="computeroutput"> element. *)
let parse_code_computeroutput item : [> `Output of string] Or_error.t =
  parse_code_helper ["class","computeroutput"] item
  |> Result.map ~f:(fun x -> `Output x)

(* Parse <strong><code>DATA</code></strong> element.*)
let parse_strong_code item : [> `Input of string] Or_error.t =
  match item with
  | `Data x ->
    error "expected <strong> but got DATA" x sexp_of_string
  | `Element {Html.name="strong"; attrs=[]; childs=[elem]} -> (
    match parse_code elem with
    | Ok x -> Ok (`Input x)
    | Error _ as e -> Or_error.tag e ~tag:"within <strong>"
  )
  | `Element {Html.name="strong"; attrs=[]; childs=_} ->
    Or_error.error_string "expected exactly one child of <strong>"
  | `Element {Html.name="strong"; attrs; childs=_::[]} ->
    error "unexpected attributes in <strong>" attrs Html.sexp_of_attributes
  | `Element {Html.name; _} ->
    error "expected <strong> but got other type of node" name sexp_of_string

let parse_data item : [> `Data of string] Or_error.t =
  let open Result.Monad_infix in
  item_to_string item
  >>| decode_html_string
  >>| fun x -> `Data x

let code_items_to_code_block code_items =
  let open Result.Monad_infix in
  let rec loop (b:code_block) = function
    | [] -> Ok b
    | ((`Input _ as x) | (`Output _ as x) | (`Data _ as x))::l ->
      loop (x::b) l
    | (`Prompt _ as x)::(`Input _ as y)::l ->
      loop (y::x::b) l
    | (`Prompt _)::(`Output x)::_ ->
      error "prompt followed by output" x sexp_of_string
    | (`Prompt _)::(`Data x)::_ ->
      error "prompt followed by data" x sexp_of_string
    | (`Prompt x)::(`Prompt y)::_ ->
      error "two successive code prompts"
        (x,y) [%sexp_of: string * string]
    | (`Prompt _)::[] ->
      Or_error.error_string "prompt not followed by anything"
  in
  loop [] code_items
  >>| List.rev

let parse_code_block (items : Html.item list) : code_block Or_error.t =
  let open Result.Monad_infix in
  (Result.List.fold items ~init:[] ~f:(fun accum item ->
    match parse_code_prompt item with
    | Ok x -> Ok (x::accum)
    | Error _ as e1 ->
      match parse_strong_code item with
      | Ok x -> Ok (x::accum)
      | Error _ as e2 ->
        match parse_code_computeroutput item with
        | Ok x -> Ok (x::accum)
        | Error _ as e3 ->
          match parse_data item with
          | Ok x -> Ok (x::accum)
          | Error _ as e4 ->
            Or_error.(
              tag (combine_errors [e1;e2;e3;e4])
                ~tag:"expected one of these to not happen but all happened"
            )
   )
  )
  >>| List.rev
  >>= code_items_to_code_block


(* Parse <pre> element. *)
let parse_pre item =
  let open Result.Monad_infix in
  let required = ["data-type"] in
  let allowed = `Some ["data-code-language"] in
  match item with
  | `Data x ->
    error "expected <pre> but got DATA" x sexp_of_string
  | `Element {Html.name="pre"; attrs; childs} -> (
    let find x = List.Assoc.find ~equal:String.equal attrs x in
    Html.check_attrs attrs ~required ~allowed >>= fun () ->
    parse_code_block childs >>| fun code_block ->
    {
      data_type = Option.value_exn (find "data-type");
      data_code_language = find "data-code-language";
      code_block;
    }
  )
  | `Element {Html.name; _} ->
    error "expected <pre> but got other type of node" name sexp_of_string


(* Find all <pre> nodes in [html]. Also find filename to which code
    should be extracted by searching for sibling <p> node that has
    this information. *)
let map (html:Html.t) ~f =
  let open Result.Monad_infix in
  let rec loop = function
    | ((`Element {Html.name="p";_}) as p_node)
      ::((`Element {Html.name="pre";_}) as pre_node)
      ::rest
       -> (
         parse_p_before_pre p_node >>= fun p ->
         parse_pre pre_node >>= fun pre ->
         loop rest >>= fun rest ->
         Ok ((f {p;pre})::rest)
       )
    | [] ->
      Ok []
    | (`Data _ as x)::rest -> (
      loop rest >>= fun rest ->
      Ok (x::rest)
    )
    | (`Element {Html.name;attrs;childs})::rest -> (
      loop childs >>= fun childs ->
      loop rest >>= fun rest ->
      Ok ((`Element{Html.name;attrs;childs})::rest)
    )
  in
  loop html

(* Notes:

   - x.pre.data_type ignored because it is always "programlisting"

   - x.pre.code_block ignored because it is not part of [import]
   type. Must be printed to external file referred to in [href].
*)
let to_import (x:t) : Import.t Or_error.t =
  Ok {
    Import.href = String.chop_prefix_exn x.p.a_href
      ~prefix:"https://github.com/realworldocaml/examples/tree/v1/code/"
    ;
    part = x.p.part;
    alt = None;
    childs = List.filter_map ~f:ident [x.p.a2; x.p.a3];
  }

let extract_code_from_1e_exn chapter =
  let base = sprintf "ch%02d" chapter in
  let in_file = "book"/(base ^ ".html") in
  let out_file = "tmp"/"book"/(base ^ ".html") in
  let code_dir = "tmp"/"book"/"code" in
  let imports : code_block Import.Map.t ref = ref Import.Map.empty in

  let code_block_to_string code_block (lang:Lang.t) part : string =
    let part = match part with
      | None | Some "" -> ""
      | Some part -> (match (lang :> string) with
        | "topscript" -> sprintf "\n[@@@part %S]\n" part
        | "ml" -> sprintf "\n\n[@@@part %S]\n" part
        | _ ->
          ok_exn (error "unexpected part number with this language"
                    (part, lang) [%sexp_of: string * Lang.t]
          )
      )
    in

    let f = function
      | `Output x | `Prompt x -> (match (lang :> string) with
        | "topscript" -> ""
        | _ -> x
      )
      | `Input x | `Data x -> x
    in
    List.map code_block ~f
    |> String.concat ~sep:""
    |> fun x -> part ^ x ^ "\n"
  in

  let write_imports () : unit Deferred.t =
    Import.Map.to_alist !imports
    |> Deferred.List.iter ~f:(fun (i,blk) ->
      let out_file = code_dir/i.Import.href in
      let contents =
        code_block_to_string blk (Import.lang_of i |> ok_exn) i.Import.part
      in
      Unix.mkdir ~p:() (Filename.dirname out_file) >>= fun () ->
      Writer.with_file ~append:true out_file ~f:(fun wrtr ->
        return (Writer.write wrtr contents))
    )
  in

  let f (x:t) : Html.item =
    let i:Import.t = ok_exn (to_import x) in
    (match Import.Map.add !imports ~key:i ~data:x.pre.code_block with
    | `Duplicate -> printf "WARNING: duplicate import for file %s part %S\n"
          i.href (match i.part with Some x -> x | None -> "")
    | `Ok imports' -> imports := imports'
    );
    Import.to_html i
  in

  Html.of_file in_file >>= fun t ->
  return (ok_exn (map t ~f)) >>= fun t ->
  write_imports () >>= fun () ->
  Writer.save out_file ~contents:(Html.to_string t)
