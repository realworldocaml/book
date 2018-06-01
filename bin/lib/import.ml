open Core

module T = struct

  type part = string
    [@@deriving sexp]

  type t = {
    href : string;
    part : part option;
    alt : string option;
    childs : Html.item list;
  } [@@deriving sexp]

  (* Ignore [childs]. *)
  let compare (x:t) (y:t) =
    compare (x.href, x.part) (y.href, y.part)

end
include T
include Comparable.Make(T)

let lang_of t = Lang.of_filename t.href

let is_import_html = function
  | `Data _ -> false
  | `Element {Html.name="link"; attrs; _} -> (
    match List.Assoc.find ~equal:String.equal attrs "rel" with
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
      let find x = List.Assoc.find ~equal:String.equal attrs x in
      Html.check_attrs attrs
        ~required:["href"; "rel"]
        ~allowed:(`Some ["part"; "alt"])
      >>= fun () ->

      (
        try Ok (find "part")
        with exn -> error "invalid part" exn sexp_of_exn
      ) >>= fun part ->

      let ans =
      {
        href = Option.value_exn (find "href");
        part;
	alt = find "alt";
        childs;
      }
      in

      lang_of ans >>= fun _ -> (* validate language *)

      Ok ans
    )
    | `Element _
    | `Data _ ->
      assert false
  )
;;

let to_html x =
  [
    Some ("rel", "import");
    Some ("href", x.href);
    (Option.map x.part ~f:(fun x -> "part", x));
    (Option.map x.alt ~f:(fun x -> "alt", x));
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
