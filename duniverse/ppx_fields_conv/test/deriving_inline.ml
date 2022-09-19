type t =
  { x : int
  ; mutable y : bool
  }
[@@deriving_inline fields ~fold_right]

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : t) -> ()
  let y _r__ = _r__.y
  let _ = y
  let set_y _r__ v__ = _r__.y <- v__
  let _ = set_y
  let x _r__ = _r__.x
  let _ = x

  module Fields = struct
    let names = [ "x"; "y" ]
    let _ = names

    let y =
      (Fieldslib.Field.Field
         { Fieldslib.Field.For_generated_code.force_variance =
             (fun (_ : [< `Read | `Set_and_create ]) -> ())
         ; name = "y"
         ; getter = y
         ; setter = Some set_y
         ; fset = (fun _r__ v__ -> { _r__ with y = v__ })
         }
       : ([< `Read | `Set_and_create ], _, bool) Fieldslib.Field.t_with_perm)
    ;;

    let _ = y

    let x =
      (Fieldslib.Field.Field
         { Fieldslib.Field.For_generated_code.force_variance =
             (fun (_ : [< `Read | `Set_and_create ]) -> ())
         ; name = "x"
         ; getter = x
         ; setter = None
         ; fset = (fun _r__ v__ -> { _r__ with x = v__ })
         }
       : ([< `Read | `Set_and_create ], _, int) Fieldslib.Field.t_with_perm)
    ;;

    let _ = x

    let make_creator ~x:x_fun__ ~y:y_fun__ compile_acc__ =
      let x_gen__, compile_acc__ = x_fun__ x compile_acc__ in
      let y_gen__, compile_acc__ = y_fun__ y compile_acc__ in
      ( (fun acc__ ->
          let x = x_gen__ acc__ in
          let y = y_gen__ acc__ in
          { x; y })
      , compile_acc__ )
    ;;

    let _ = make_creator
    let create ~x ~y = { x; y }
    let _ = create
    let map ~x:x_fun__ ~y:y_fun__ = { x = x_fun__ x; y = y_fun__ y }
    let _ = map

    let iter ~x:x_fun__ ~y:y_fun__ =
      (x_fun__ x : unit);
      (y_fun__ y : unit)
    ;;

    let _ = iter
    let fold ~init:init__ ~x:x_fun__ ~y:y_fun__ = y_fun__ (x_fun__ init__ x) y
    let _ = fold
    let map_poly record__ = [ record__.Fieldslib.Field.f x; record__.Fieldslib.Field.f y ]
    let _ = map_poly
    let for_all ~x:x_fun__ ~y:y_fun__ = (true && x_fun__ x) && y_fun__ y
    let _ = for_all
    let exists ~x:x_fun__ ~y:y_fun__ = (false || x_fun__ x) || y_fun__ y
    let _ = exists
    let to_list ~x:x_fun__ ~y:y_fun__ = [ x_fun__ x; y_fun__ y ]
    let _ = to_list
    let fold_right ~x:x_fun__ ~y:y_fun__ ~init:init__ = x_fun__ x (y_fun__ y init__)
    let _ = fold_right

    module Direct = struct
      let iter record__ ~x:x_fun__ ~y:y_fun__ =
        x_fun__ x record__ record__.x;
        y_fun__ y record__ record__.y
      ;;

      let _ = iter

      let fold record__ ~init:init__ ~x:x_fun__ ~y:y_fun__ =
        y_fun__ (x_fun__ init__ x record__ record__.x) y record__ record__.y
      ;;

      let _ = fold

      let for_all record__ ~x:x_fun__ ~y:y_fun__ =
        (true && x_fun__ x record__ record__.x) && y_fun__ y record__ record__.y
      ;;

      let _ = for_all

      let exists record__ ~x:x_fun__ ~y:y_fun__ =
        (false || x_fun__ x record__ record__.x) || y_fun__ y record__ record__.y
      ;;

      let _ = exists

      let to_list record__ ~x:x_fun__ ~y:y_fun__ =
        [ x_fun__ x record__ record__.x; y_fun__ y record__ record__.y ]
      ;;

      let _ = to_list

      let fold_right record__ ~x:x_fun__ ~y:y_fun__ ~init:init__ =
        x_fun__ x record__ record__.x (y_fun__ y record__ record__.y init__)
      ;;

      let _ = fold_right

      let map record__ ~x:x_fun__ ~y:y_fun__ =
        { x = x_fun__ x record__ record__.x; y = y_fun__ y record__ record__.y }
      ;;

      let _ = map

      let set_all_mutable_fields _record__ ~y =
        let _record__ = Fieldslib.Field.For_generated_code.opaque_identity _record__ in
        _record__.y <- y
      [@@inline always]
      ;;

      let _ = set_all_mutable_fields
    end
  end

  let _ = Fields.Direct.iter
end [@@ocaml.doc "@inline"]

[@@@end]
