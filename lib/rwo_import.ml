open Core.Std
module Lang = Rwo_lang
module Html = Rwo_html

module T = struct
  type t = {
    data_code_language : Rwo_lang.t;
    href : string;
    part : float option;
    childs : Rwo_html.item list;
  } with sexp

  (* Ignore [data_code_language] and [childs]. *)
  let compare (x:t) (y:t) =
    compare (x.href, x.part) (y.href, y.part)

end
include T
include Comparable.Make(T)

let is_import_html = function
  | `Data _ -> false
  | `Element {Html.name="link"; attrs; _} -> (
    match List.Assoc.find attrs "rel" with
    | Some "import" -> true
    | Some _
    | None -> false
  )
  | `Element _ -> false

let of_html item =
  let open Result.Monad_infix in
  if not (is_import_html item) then
    error "attempting to parse non-import node as an import node"
      item Html.sexp_of_item
  else (
    match item with
    | `Element {name="link"; attrs; childs} -> (
      let find x = List.Assoc.find attrs x in
      Html.check_attrs attrs
        ~required:["data-code-language"; "href"; "rel"]
        ~allowed:(`Some ["part"])
      >>= fun () ->

      (
        try Ok (find "part" |> Option.map ~f:Float.of_string)
        with exn -> error "invalid part" exn sexp_of_exn
      ) >>= fun part ->

      Lang.of_string (Option.value_exn (find "data-code-language"))
      >>= fun data_code_language ->

      Ok {
        data_code_language;
        href = Option.value_exn (find "href");
        part;
        childs;
      }
    )
    | `Element _
    | `Data _ ->
      assert false
  )
;;

let to_html x =
  [
    Some ("rel", "import");
    Some ("data-code-language", Lang.to_string x.data_code_language);
    Some ("href", x.href);
    (Option.map x.part ~f:(fun x -> "part", Float.to_string x));
  ]
  |> List.filter_map ~f:ident
  |> fun a -> Html.link ~a []

let find_all html =
  let rec loop accum = function
    | [] -> accum
    | (`Data _)::rest ->
      loop accum rest
    | (`Element {Html.childs;_} as item)::rest ->
      let accum =
        if is_import_html item
        then (ok_exn (of_html item))::accum
        else accum
      in
      loop (loop accum childs) rest
  in
  loop [] html
