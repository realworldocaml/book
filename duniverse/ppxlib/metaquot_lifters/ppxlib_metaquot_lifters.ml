open Stdppx
open Ppxlib
open Ast_builder.Default

class expression_lifters loc =
  let loc = { loc with loc_ghost = true } in
  object
    inherit [expression] Ppxlib_traverse_builtins.lift
    method record flds =
      pexp_record ~loc
        (List.map flds ~f:(fun (lab, e) ->
           ({ loc; txt = Lident lab }, e)))
        None
    method constr id args =
      pexp_construct ~loc { loc; txt = Lident id }
        (match args with
         | [] -> None
         | l  -> Some (pexp_tuple ~loc l))
    method tuple     l = pexp_tuple ~loc l
    method int       i = eint       ~loc i
    method int32     i = eint32     ~loc i
    method int64     i = eint64     ~loc i
    method nativeint i = enativeint ~loc i
    method float     f = efloat     ~loc (Float.to_string f)
    method string    s = estring    ~loc s
    method char      c = echar      ~loc c
    method bool      b = ebool      ~loc b
    method array : 'a. ('a -> expression) -> 'a array -> expression =
      fun f a -> pexp_array ~loc (List.map (Array.to_list a) ~f)
    method unit () = eunit ~loc
    method other : 'a. 'a -> expression = fun _ -> failwith "not supported"
  end

class pattern_lifters loc =
  let loc = { loc with loc_ghost = true } in
  object
    inherit [pattern] Ppxlib_traverse_builtins.lift
    method record flds =
      ppat_record ~loc
        (List.map flds ~f:(fun (lab, e) ->
           ({ loc; txt = Lident lab }, e)))
        Closed
    method constr id args =
      ppat_construct ~loc { loc; txt = Lident id }
        (match args with
         | [] -> None
         | l  -> Some (ppat_tuple ~loc l))
    method tuple     l = ppat_tuple ~loc l
    method int       i = pint       ~loc i
    method int32     i = pint32     ~loc i
    method int64     i = pint64     ~loc i
    method nativeint i = pnativeint ~loc i
    method float     f = pfloat     ~loc (Float.to_string f)
    method string    s = pstring    ~loc s
    method char      c = pchar      ~loc c
    method bool      b = pbool      ~loc b
    method array : 'a. ('a -> pattern) -> 'a array -> pattern =
      fun f a -> ppat_array ~loc (List.map (Array.to_list a) ~f)
    method unit () = punit ~loc
    method other : 'a. 'a -> pattern = fun _ -> failwith "not supported"
  end
