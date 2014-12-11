open Core.Std
open Rwo_core2
open Async.Std

type item = Nethtml.document

type t = item list

type attributes = (string * string) list
with sexp

let of_string s =
  Netchannels.with_in_obj_channel
    (new Netchannels.input_string s)
    Nethtml.parse

let of_file file =
  Reader.file_contents file >>| of_string

let item_of_string_helper ?filename s = match of_string s with
  | x::[] -> Ok x
  | l -> (
    let msg = "expected single HTML item but got" in
    let n = List.length l in
    match filename with
    | None -> error msg n sexp_of_int
    | Some filename -> error msg (filename,n) <:sexp_of< string * int >>
  )

let item_of_string s = item_of_string_helper s

let item_of_file filename =
  Reader.file_contents filename
  >>| item_of_string_helper ~filename

let to_string docs =
  let buf = Buffer.create 2048 in
  Netchannels.with_out_obj_channel (new Netchannels.output_buffer buf)
    (Fn.flip Nethtml.write docs)
  ;
  Buffer.contents buf


let has_html_extension file =
  Filename.split_extension file
  |> snd
  |> function Some _ -> true | None -> false

let html_files_of_dir dir =
  Sys.readdir dir
  >>| Array.to_list
  >>| List.filter ~f:has_html_extension
  >>| List.map ~f:(Filename.concat dir)

let get_all_nodes tag t =
  let rec helper t =
    List.fold t ~init:[] ~f:(fun accum item ->
      match item with
      | Nethtml.Element (name,_,childs) ->
        if name = tag then
          item::accum
        else
          (helper childs)@accum
      | Nethtml.Data _ -> accum
    )
  in
  helper t |> List.rev


let is_nested name t =
  let rec loop have_seen = function
    | Nethtml.Data _ -> false
    | Nethtml.Element (name', _, childs) ->
      if have_seen && (name = name') then
        true
      else
        let have_seen = have_seen || (name = name') in
        List.exists childs ~f:(loop have_seen)
  in
  List.exists t ~f:(loop false)


let print_elements_only ?(exclude_elements=[]) ?(keep_attrs=[]) t =
  let rec print_item depth = function
    | Nethtml.Data _ -> ()
    | Nethtml.Element (name, attrs, childs) ->
      if List.mem exclude_elements name then
        ()
      else (
        let padding = String.init (2*depth) ~f:(fun _ -> ' ') in
        let attrs =
          List.filter_map attrs ~f:(fun (attr,value) ->
            if List.mem keep_attrs attr then
              Some (sprintf "%s=%s" attr value)
            else
              None
          )
          |> String.concat ~sep:" "
        in
        printf "%s%s %s\n" padding name attrs;
        List.iter childs ~f:(print_item (depth+1))
      )
  in
  List.iter t ~f:(print_item 0)


(******************************************************************************)
(* Attributes                                                                 *)
(******************************************************************************)
let get_all_attributes t =
  let rec helper t =
    List.fold t ~init:String.Set.empty ~f:(fun accum item -> match item with
    | Nethtml.Data _ -> accum
    | Nethtml.Element (_, attrs, childs) -> (
      List.fold attrs ~init:accum ~f:(fun accum (name,_) -> Set.add accum name)
      |> Set.union (helper childs)
    ) )
  in
  helper t |> Set.to_list

let check_attrs ?(required=[]) ?(allowed=`Any) attrs_list =
  let attrs_list = List.map attrs_list ~f:fst in
  let attrs = String.Set.of_list attrs_list in
  let required = String.Set.of_list required in
  match List.find_a_dup attrs_list with
  | Some x ->
    error "attribute repeated" x sexp_of_string
  | None ->
    if not (Set.subset required attrs) then
      error "expected attributes not present"
        (Set.diff required attrs) String.Set.sexp_of_t
    else
      match allowed with
      | `Any -> Ok ()
      | `Some allowed ->
        let allowed = String.Set.of_list allowed in
        let remaining = Set.diff attrs required in
        if Set.subset remaining allowed then
          Ok ()
        else
          error "unexpected attributes present"
            (Set.diff remaining allowed)
            String.Set.sexp_of_t


(******************************************************************************)
(* HTMLBook Code Blocks                                                       *)
(******************************************************************************)
type code_item = [
| `Output of string
| `Prompt of string
| `Input of string
| `Data of string
]

type code_block = code_item list

type pre = {
  data_type : string;
  class_ : string option;
  data_code_language : string option;
  code_block : code_block;
}

(** Types of code elements within a code block. Needed only as an
    intermediate type to parse code blocks. *)

(** Parse <code> element, requiring exactly the given attributes. *)
let parse_code_helper expected_attrs item : string Or_error.t =
  match item with
  | Nethtml.Data x ->
    error "expected <code> but got DATA" x sexp_of_string
  | Nethtml.Element ("code", attrs, [Nethtml.Data data]) -> (
    if attrs = expected_attrs then
      Ok data
    else
      error "expected attributes differ from the ones found"
        (expected_attrs, attrs)
        <:sexp_of< attributes * attributes >>
  )
  | Nethtml.Element ("code", [], _) ->
    Or_error.error_string "<code> expected to have single DATA child node"
  | Nethtml.Element (name, _, _) ->
    error "expected <code> but got other type of node" name sexp_of_string

(** Parse <code> element. *)
let parse_code item : string Or_error.t =
  parse_code_helper [] item

(** Parse <code class="prompt"> element. *)
let parse_code_prompt item : [> `Prompt of string] Or_error.t =
  parse_code_helper ["class","prompt"] item
  |> Result.map ~f:(fun x -> `Prompt x)

let parse_code_computeroutput item : [> `Output of string] Or_error.t =
  parse_code_helper ["class","computeroutput"] item
  |> Result.map ~f:(fun x -> `Output x)

(** Parse <strong><code>DATA</code></strong> element.*)
let parse_strong_code item : [> `Input of string] Or_error.t =
  match item with
  | Nethtml.Data x ->
    error "expected <strong> but got DATA" x sexp_of_string
  | Nethtml.Element ("strong", [], [elem]) -> (
    match parse_code elem with
    | Ok x -> Ok (`Input x)
    | Error _ as e -> Or_error.tag e "within <strong>"
  )
  | Nethtml.Element ("strong", [], _) ->
    Or_error.error_string "expected exactly one child of <strong>"
  | Nethtml.Element ("strong", attrs, _::[]) ->
    error "unexpected attributes in <strong>" attrs sexp_of_attributes
  | Nethtml.Element (name, _, _) ->
    error "expected <strong> but got other type of node" name sexp_of_string

let parse_data item : [> `Data of string] Or_error.t =
  match item with
  | Nethtml.Data x -> Ok (`Data x)
  | Nethtml.Element (name, _, _) ->
    error "expected DATA node but got element" name sexp_of_string

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
        (x,y) <:sexp_of< string * string >>
    | (`Prompt _)::[] ->
      Or_error.error_string "prompt not followed by anything"
  in
  loop [] code_items
  >>| List.rev

let parse_code_block (items : item list) : code_block Or_error.t =
  let open Result.Monad_infix in
  let elems : code_item list Or_error.t =
    Result.List.fold items ~init:[] ~f:(fun accum item ->
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
              Or_error.all [e1;e2;e3;e4]
    ) >>| List.rev
  in
  elems >>= code_items_to_code_block

(** Parse <pre> element. *)
let parse_pre item =
  let open Result.Monad_infix in
  let required = ["data-type"] in
  let allowed = `Some ["class"; "data-code-language"] in
  match item with
  | Nethtml.Data x ->
    error "expected <pre> but got DATA" x sexp_of_string
  | Nethtml.Element ("pre", attrs, childs) -> (
    let find x = List.Assoc.find attrs x in
    check_attrs attrs ~required ~allowed >>= fun () ->
    parse_code_block childs >>| fun code_block ->
    {
      data_type = Option.value_exn (find "data-type");
      class_ = find "class";
      data_code_language = find "data-code-language";
      code_block;
    }
  )
  | Nethtml.Element (name, _, _) ->
    error "expected <pre> but got other type of node" name sexp_of_string


(******************************************************************************)
(* Import Nodes                                                               *)
(******************************************************************************)
type import = {
  data_code_language : string;
  href : string;
}

let parse_import item =
  let open Result.Monad_infix in
  match item with
  | Nethtml.Data x ->
    error "expected <link> but got DATA" x sexp_of_string
  | Nethtml.Element ("link", attrs, []) -> (
    let find x = List.Assoc.find attrs x in
    check_attrs attrs
      ~required:["data-code-language"; "href"]
      ~allowed:(`Some [])
    >>| fun () ->
    {
      data_code_language = Option.value_exn (find "data-code-language");
      href = Option.value_exn (find "href");
    }
  )
  | Nethtml.Element ("link", _, _::_) ->
    Or_error.error_string "<link> node cannot have children"
  | Nethtml.Element (name, _, _) ->
    error "expected <link> but got other type of node" name sexp_of_string
