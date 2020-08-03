open Tyxml

(* Basic alcotest machinery *)

let to_string = Format.asprintf "%a" (Html.pp_elt ())

let tyxml_tests l =
  let f (name, (ty : Html_types.body_content Html.elt), s) =
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l


(* Boilerplate to make writing the PPX and JSX tests easier *)

module type LANGUAGE = sig
  include Xml_sigs.Typed_pp
  type 'a wrap
  type 'a list_wrap
  val pp_wrap :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a wrap -> unit
  val pp_wrap_list :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a list_wrap -> unit
  val totl : Xml.elt list_wrap -> ('a elt) list_wrap
  val toeltl : ('a elt) list_wrap -> Xml.elt list_wrap
end

module TyTests (Language : LANGUAGE) = struct
  module Testable = struct
    type t = Xml.elt Language.list_wrap
    let pp fmt x =
      Language.pp_wrap_list
        (Language.pp_elt ())
        fmt (Language.totl x)
    let equal = (=)
  end

  let make l =
    let f (name, ty1, ty2) =
      name, `Quick, fun () ->
        Alcotest.(check (module Testable)) name
          (Language.toeltl ty1) (Language.toeltl ty2)
    in
    List.map f l
end

module Html = struct
  include Tyxml.Html
  let pp_wrap pp = pp
  let pp_wrap_list pp = Format.pp_print_list ~pp_sep:(fun _ () -> ()) pp
end
module Svg = struct
  include Tyxml.Svg
  let pp_wrap pp = pp
  let pp_wrap_list pp = Format.pp_print_list ~pp_sep:(fun _ () -> ()) pp
end
module HtmlTests = TyTests (Html)
module SvgTests = TyTests (Svg)


(* The regular HTML module, but with most type equality hidden.
   This forces the use of the wrapping functions provided in Xml.W.
*)
module HtmlWrapped : sig
  include Html_sigs.T
    with type Xml.elt = Tyxml.Xml.elt
     and type 'a elt = 'a Html.elt
  include LANGUAGE
    with type 'a elt := 'a elt
     and type 'a wrap := 'a wrap
     and type 'a list_wrap := 'a list_wrap
     and type doc := doc
end = struct
  include Html
  module Svg = Svg
end
module HtmlWrappedTests = TyTests(HtmlWrapped)

let (@:) h t =  HtmlWrapped.Xml.W.(cons (return h) t)
let (@-) =  HtmlWrapped.Xml.W.append
let nil = HtmlWrapped.Xml.W.nil
let (!) = HtmlWrapped.Xml.W.return
let (!:) x = x @: nil ()
