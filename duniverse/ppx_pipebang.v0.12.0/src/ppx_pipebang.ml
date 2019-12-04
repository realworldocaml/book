open! Ppxlib

let expand (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_apply (_, [(Nolabel, x); (Nolabel, y)]) ->
      Some (
        match y with
        | { pexp_desc = Pexp_construct (id, None); _ } ->
          { y with pexp_desc = Pexp_construct (id, Some x) }
        | { pexp_desc = Pexp_apply (f, args); pexp_attributes = []; _ }
          when (match f.pexp_desc with
            (* Do not inline |> as this would create applications with too many
               arguments *)
            | Pexp_ident { txt = Lident "|>"; _ } -> false
            | _ -> true) ->
          { e with pexp_desc = Pexp_apply (f, args @ [(Nolabel, x)]) }
        | _ ->
          { e with pexp_desc = Pexp_apply (y, [(Nolabel, x)]) }
      )
  | Pexp_ident { txt = Lident s; _ }
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ }, _) ->
    Location.raise_errorf ~loc:e.pexp_loc "%s must be applied to two arguments" s
  | _ -> None
;;

let () =
  Driver.register_transformation "pipebang"
    ~rules:[ Context_free.Rule.special_function "|>" expand ]
;;
