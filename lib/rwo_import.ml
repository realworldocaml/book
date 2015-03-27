open Core.Std
module Code = Rwo_code
module Html = Rwo_html

module T = struct
  type t = {
    data_code_language : Rwo_code.lang;
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
  | Nethtml.Data _ -> false
  | Nethtml.Element ("link", attrs, _) -> (
    match List.Assoc.find attrs "rel" with
    | Some "import" -> true
    | Some _
    | None -> false
  )
  | Nethtml.Element (_,_,_) -> false

let of_html item =
  let open Result.Monad_infix in
  if not (is_import_html item) then
    error "attempting to parse non-import node as an import node"
      item Html.sexp_of_item
  else (
    match item with
    | Nethtml.Element ("link", attrs, childs) -> (
      let find x = List.Assoc.find attrs x in
      Html.check_attrs attrs
        ~required:["data-code-language"; "href"; "rel"]
        ~allowed:(`Some ["part"])
      >>= fun () ->

      (
        try Ok (find "part" |> Option.map ~f:Float.of_string)
        with exn -> error "invalid part" exn sexp_of_exn
      ) >>= fun part ->

      Code.lang_of_string (Option.value_exn (find "data-code-language"))
      >>= fun data_code_language ->

      Ok {
        data_code_language;
        href = Option.value_exn (find "href");
        part;
        childs;
      }
    )
    | Nethtml.Element (_,_,_)
    | Nethtml.Data _ ->
      assert false
  )
;;

let to_html x =
  [
    Some ("rel", "import");
    Some ("data-code-language", Code.lang_to_string x.data_code_language);
    Some ("href", x.href);
    (Option.map x.part ~f:(fun x -> "part", Float.to_string x));
  ]
  |> List.filter_map ~f:ident
  |> fun a -> Html.link ~a []
