open! Base

module Abstract = struct
  type t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__002_ = "expansion.ml.Abstract.t" in
     fun x__003_ -> Sexplib0.Sexp_conv_error.empty_type error_source__002_ x__003_
                    : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp
  let sexp_of_t = (fun _ -> assert false : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Tuple = struct
  type t = int * int * int [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__012_ = "expansion.ml.Tuple.t" in
     function
     | Sexplib0.Sexp.List [ arg0__005_; arg1__006_; arg2__007_ ] ->
       let res0__008_ = int_of_sexp arg0__005_
       and res1__009_ = int_of_sexp arg1__006_
       and res2__010_ = int_of_sexp arg2__007_ in
       res0__008_, res1__009_, res2__010_
     | sexp__011_ ->
       Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__012_ 3 sexp__011_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (arg0__013_, arg1__014_, arg2__015_) ->
       let res0__016_ = sexp_of_int arg0__013_
       and res1__017_ = sexp_of_int arg1__014_
       and res2__018_ = sexp_of_int arg2__015_ in
       Sexplib0.Sexp.List [ res0__016_; res1__017_; res2__018_ ]
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__036_ = "expansion.ml.Record.t" in
     function
     | Sexplib0.Sexp.List field_sexps__021_ as sexp__020_ ->
       let a__022_ = Stdlib.ref Stdlib.Option.None
       and b__024_ = Stdlib.ref Stdlib.Option.None
       and c__026_ = Stdlib.ref Stdlib.Option.None
       and duplicates__028_ = Stdlib.ref []
       and extra__029_ = Stdlib.ref [] in
       let rec iter__037_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__030_ :: (([] | [ _ ]) as _field_sexps__032_))
           :: tail__038_ ->
           let _field_sexp__031_ () =
             match _field_sexps__032_ with
             | [ x__039_ ] -> x__039_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__036_
                 sexp__020_
             | _ -> assert false
           in
           (match field_name__030_ with
            | "a" ->
              (match Stdlib.( ! ) a__022_ with
               | Stdlib.Option.None ->
                 let _field_sexp__031_ = _field_sexp__031_ () in
                 let fvalue__035_ = int_of_sexp _field_sexp__031_ in
                 Stdlib.( := ) a__022_ (Stdlib.Option.Some fvalue__035_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__028_
                   (field_name__030_ :: Stdlib.( ! ) duplicates__028_))
            | "b" ->
              (match Stdlib.( ! ) b__024_ with
               | Stdlib.Option.None ->
                 let _field_sexp__031_ = _field_sexp__031_ () in
                 let fvalue__034_ = int_of_sexp _field_sexp__031_ in
                 Stdlib.( := ) b__024_ (Stdlib.Option.Some fvalue__034_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__028_
                   (field_name__030_ :: Stdlib.( ! ) duplicates__028_))
            | "c" ->
              (match Stdlib.( ! ) c__026_ with
               | Stdlib.Option.None ->
                 let _field_sexp__031_ = _field_sexp__031_ () in
                 let fvalue__033_ = int_of_sexp _field_sexp__031_ in
                 Stdlib.( := ) c__026_ (Stdlib.Option.Some fvalue__033_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__028_
                   (field_name__030_ :: Stdlib.( ! ) duplicates__028_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__029_ (field_name__030_ :: Stdlib.( ! ) extra__029_)
              else ());
           iter__037_ tail__038_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__020_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__036_
             sexp__020_
         | [] -> ()
       in
       iter__037_ field_sexps__021_;
       (match Stdlib.( ! ) duplicates__028_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__036_
            (Stdlib.( ! ) duplicates__028_)
            sexp__020_
        | [] ->
          (match Stdlib.( ! ) extra__029_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__036_
               (Stdlib.( ! ) extra__029_)
               sexp__020_
           | [] ->
             (match Stdlib.( ! ) a__022_, Stdlib.( ! ) b__024_, Stdlib.( ! ) c__026_ with
              | ( Stdlib.Option.Some a__023_
                , Stdlib.Option.Some b__025_
                , Stdlib.Option.Some c__027_ ) -> { a = a__023_; b = b__025_; c = c__027_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__036_
                  sexp__020_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__022_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__024_) Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) c__026_) Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp__020_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__036_ sexp__020_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__041_; b = b__043_; c = c__045_ } ->
       let bnds__040_ = [] in
       let bnds__040_ =
         let arg__046_ = sexp_of_int c__045_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__046_ ] :: bnds__040_
       in
       let bnds__040_ =
         let arg__044_ = sexp_of_int b__043_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__044_ ] :: bnds__040_
       in
       let bnds__040_ =
         let arg__042_ = sexp_of_int a__041_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__042_ ] :: bnds__040_
       in
       Sexplib0.Sexp.List bnds__040_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Mutable_record = struct
  type t =
    { mutable a : int
    ; mutable b : int
    ; mutable c : int
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__064_ = "expansion.ml.Mutable_record.t" in
     function
     | Sexplib0.Sexp.List field_sexps__049_ as sexp__048_ ->
       let a__050_ = Stdlib.ref Stdlib.Option.None
       and b__052_ = Stdlib.ref Stdlib.Option.None
       and c__054_ = Stdlib.ref Stdlib.Option.None
       and duplicates__056_ = Stdlib.ref []
       and extra__057_ = Stdlib.ref [] in
       let rec iter__065_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__058_ :: (([] | [ _ ]) as _field_sexps__060_))
           :: tail__066_ ->
           let _field_sexp__059_ () =
             match _field_sexps__060_ with
             | [ x__067_ ] -> x__067_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__064_
                 sexp__048_
             | _ -> assert false
           in
           (match field_name__058_ with
            | "a" ->
              (match Stdlib.( ! ) a__050_ with
               | Stdlib.Option.None ->
                 let _field_sexp__059_ = _field_sexp__059_ () in
                 let fvalue__063_ = int_of_sexp _field_sexp__059_ in
                 Stdlib.( := ) a__050_ (Stdlib.Option.Some fvalue__063_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__056_
                   (field_name__058_ :: Stdlib.( ! ) duplicates__056_))
            | "b" ->
              (match Stdlib.( ! ) b__052_ with
               | Stdlib.Option.None ->
                 let _field_sexp__059_ = _field_sexp__059_ () in
                 let fvalue__062_ = int_of_sexp _field_sexp__059_ in
                 Stdlib.( := ) b__052_ (Stdlib.Option.Some fvalue__062_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__056_
                   (field_name__058_ :: Stdlib.( ! ) duplicates__056_))
            | "c" ->
              (match Stdlib.( ! ) c__054_ with
               | Stdlib.Option.None ->
                 let _field_sexp__059_ = _field_sexp__059_ () in
                 let fvalue__061_ = int_of_sexp _field_sexp__059_ in
                 Stdlib.( := ) c__054_ (Stdlib.Option.Some fvalue__061_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__056_
                   (field_name__058_ :: Stdlib.( ! ) duplicates__056_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__057_ (field_name__058_ :: Stdlib.( ! ) extra__057_)
              else ());
           iter__065_ tail__066_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__048_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__064_
             sexp__048_
         | [] -> ()
       in
       iter__065_ field_sexps__049_;
       (match Stdlib.( ! ) duplicates__056_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__064_
            (Stdlib.( ! ) duplicates__056_)
            sexp__048_
        | [] ->
          (match Stdlib.( ! ) extra__057_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__064_
               (Stdlib.( ! ) extra__057_)
               sexp__048_
           | [] ->
             (match Stdlib.( ! ) a__050_, Stdlib.( ! ) b__052_, Stdlib.( ! ) c__054_ with
              | ( Stdlib.Option.Some a__051_
                , Stdlib.Option.Some b__053_
                , Stdlib.Option.Some c__055_ ) -> { a = a__051_; b = b__053_; c = c__055_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__064_
                  sexp__048_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__050_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__052_) Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) c__054_) Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp__048_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__064_ sexp__048_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__069_; b = b__071_; c = c__073_ } ->
       let bnds__068_ = [] in
       let bnds__068_ =
         let arg__074_ = sexp_of_int c__073_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__074_ ] :: bnds__068_
       in
       let bnds__068_ =
         let arg__072_ = sexp_of_int b__071_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__072_ ] :: bnds__068_
       in
       let bnds__068_ =
         let arg__070_ = sexp_of_int a__069_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__070_ ] :: bnds__068_
       in
       Sexplib0.Sexp.List bnds__068_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant = struct
  type t =
    | A
    | B of int * int
    | C of
        { a : int
        ; b : int
        ; d : int
        }
    | D of
        { mutable a : int
        ; mutable b : int
        ; mutable t : int
        }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__077_ = "expansion.ml.Variant.t" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__080_) :: sexp_args__081_) as
       _sexp__079_ ->
       (match sexp_args__081_ with
        | [ arg0__082_; arg1__083_ ] ->
          let res0__084_ = int_of_sexp arg0__082_
          and res1__085_ = int_of_sexp arg1__083_ in
          B (res0__084_, res1__085_)
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__077_
            _tag__080_
            _sexp__079_)
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("c" | "C") as _tag__088_) :: field_sexps__086_) as
       sexp__087_ ->
       let a__089_ = Stdlib.ref Stdlib.Option.None
       and b__091_ = Stdlib.ref Stdlib.Option.None
       and d__093_ = Stdlib.ref Stdlib.Option.None
       and duplicates__095_ = Stdlib.ref []
       and extra__096_ = Stdlib.ref [] in
       let rec iter__103_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__097_ :: (([] | [ _ ]) as _field_sexps__099_))
           :: tail__104_ ->
           let _field_sexp__098_ () =
             match _field_sexps__099_ with
             | [ x__105_ ] -> x__105_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__077_
                 sexp__087_
             | _ -> assert false
           in
           (match field_name__097_ with
            | "a" ->
              (match Stdlib.( ! ) a__089_ with
               | Stdlib.Option.None ->
                 let _field_sexp__098_ = _field_sexp__098_ () in
                 let fvalue__102_ = int_of_sexp _field_sexp__098_ in
                 Stdlib.( := ) a__089_ (Stdlib.Option.Some fvalue__102_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__095_
                   (field_name__097_ :: Stdlib.( ! ) duplicates__095_))
            | "b" ->
              (match Stdlib.( ! ) b__091_ with
               | Stdlib.Option.None ->
                 let _field_sexp__098_ = _field_sexp__098_ () in
                 let fvalue__101_ = int_of_sexp _field_sexp__098_ in
                 Stdlib.( := ) b__091_ (Stdlib.Option.Some fvalue__101_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__095_
                   (field_name__097_ :: Stdlib.( ! ) duplicates__095_))
            | "d" ->
              (match Stdlib.( ! ) d__093_ with
               | Stdlib.Option.None ->
                 let _field_sexp__098_ = _field_sexp__098_ () in
                 let fvalue__100_ = int_of_sexp _field_sexp__098_ in
                 Stdlib.( := ) d__093_ (Stdlib.Option.Some fvalue__100_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__095_
                   (field_name__097_ :: Stdlib.( ! ) duplicates__095_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__096_ (field_name__097_ :: Stdlib.( ! ) extra__096_)
              else ());
           iter__103_ tail__104_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__087_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__077_
             sexp__087_
         | [] -> ()
       in
       iter__103_ field_sexps__086_;
       (match Stdlib.( ! ) duplicates__095_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__077_
            (Stdlib.( ! ) duplicates__095_)
            sexp__087_
        | [] ->
          (match Stdlib.( ! ) extra__096_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__077_
               (Stdlib.( ! ) extra__096_)
               sexp__087_
           | [] ->
             (match Stdlib.( ! ) a__089_, Stdlib.( ! ) b__091_, Stdlib.( ! ) d__093_ with
              | ( Stdlib.Option.Some a__090_
                , Stdlib.Option.Some b__092_
                , Stdlib.Option.Some d__094_ ) -> C { a = a__090_; b = b__092_; d = d__094_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__077_
                  sexp__087_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__089_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__091_) Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) d__093_) Stdlib.Option.None, "d"
                  ])))
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("d" | "D") as _tag__108_) :: field_sexps__106_) as
       sexp__107_ ->
       let a__109_ = Stdlib.ref Stdlib.Option.None
       and b__111_ = Stdlib.ref Stdlib.Option.None
       and t__113_ = Stdlib.ref Stdlib.Option.None
       and duplicates__115_ = Stdlib.ref []
       and extra__116_ = Stdlib.ref [] in
       let rec iter__123_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__117_ :: (([] | [ _ ]) as _field_sexps__119_))
           :: tail__124_ ->
           let _field_sexp__118_ () =
             match _field_sexps__119_ with
             | [ x__125_ ] -> x__125_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__077_
                 sexp__107_
             | _ -> assert false
           in
           (match field_name__117_ with
            | "a" ->
              (match Stdlib.( ! ) a__109_ with
               | Stdlib.Option.None ->
                 let _field_sexp__118_ = _field_sexp__118_ () in
                 let fvalue__122_ = int_of_sexp _field_sexp__118_ in
                 Stdlib.( := ) a__109_ (Stdlib.Option.Some fvalue__122_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__115_
                   (field_name__117_ :: Stdlib.( ! ) duplicates__115_))
            | "b" ->
              (match Stdlib.( ! ) b__111_ with
               | Stdlib.Option.None ->
                 let _field_sexp__118_ = _field_sexp__118_ () in
                 let fvalue__121_ = int_of_sexp _field_sexp__118_ in
                 Stdlib.( := ) b__111_ (Stdlib.Option.Some fvalue__121_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__115_
                   (field_name__117_ :: Stdlib.( ! ) duplicates__115_))
            | "t" ->
              (match Stdlib.( ! ) t__113_ with
               | Stdlib.Option.None ->
                 let _field_sexp__118_ = _field_sexp__118_ () in
                 let fvalue__120_ = int_of_sexp _field_sexp__118_ in
                 Stdlib.( := ) t__113_ (Stdlib.Option.Some fvalue__120_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__115_
                   (field_name__117_ :: Stdlib.( ! ) duplicates__115_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__116_ (field_name__117_ :: Stdlib.( ! ) extra__116_)
              else ());
           iter__123_ tail__124_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__107_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__077_
             sexp__107_
         | [] -> ()
       in
       iter__123_ field_sexps__106_;
       (match Stdlib.( ! ) duplicates__115_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__077_
            (Stdlib.( ! ) duplicates__115_)
            sexp__107_
        | [] ->
          (match Stdlib.( ! ) extra__116_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__077_
               (Stdlib.( ! ) extra__116_)
               sexp__107_
           | [] ->
             (match Stdlib.( ! ) a__109_, Stdlib.( ! ) b__111_, Stdlib.( ! ) t__113_ with
              | ( Stdlib.Option.Some a__110_
                , Stdlib.Option.Some b__112_
                , Stdlib.Option.Some t__114_ ) -> D { a = a__110_; b = b__112_; t = t__114_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__077_
                  sexp__107_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__109_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__111_) Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) t__113_) Stdlib.Option.None, "t"
                  ])))
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__078_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__077_ sexp__078_
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp__078_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__077_ sexp__078_
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp__078_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__077_ sexp__078_
     | Sexplib0.Sexp.Atom ("d" | "D") as sexp__078_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__077_ sexp__078_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__076_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__077_ sexp__076_
     | Sexplib0.Sexp.List [] as sexp__076_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__077_ sexp__076_
     | sexp__076_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__077_ sexp__076_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | A -> Sexplib0.Sexp.Atom "A"
      | B (arg0__126_, arg1__127_) ->
        let res0__128_ = sexp_of_int arg0__126_
        and res1__129_ = sexp_of_int arg1__127_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__128_; res1__129_ ]
      | C { a = a__131_; b = b__133_; d = d__135_ } ->
        let bnds__130_ = [] in
        let bnds__130_ =
          let arg__136_ = sexp_of_int d__135_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__136_ ] :: bnds__130_
        in
        let bnds__130_ =
          let arg__134_ = sexp_of_int b__133_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__134_ ] :: bnds__130_
        in
        let bnds__130_ =
          let arg__132_ = sexp_of_int a__131_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__132_ ] :: bnds__130_
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__130_)
      | D { a = a__138_; b = b__140_; t = t__142_ } ->
        let bnds__137_ = [] in
        let bnds__137_ =
          let arg__143_ = sexp_of_int t__142_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__143_ ] :: bnds__137_
        in
        let bnds__137_ =
          let arg__141_ = sexp_of_int b__140_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__141_ ] :: bnds__137_
        in
        let bnds__137_ =
          let arg__139_ = sexp_of_int a__138_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__139_ ] :: bnds__137_
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__137_)
        : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant = struct
  type t =
    [ `A
    | `B of int
    ]
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__149_ = "expansion.ml.Poly_variant.t" in
     function
     | Sexplib0.Sexp.Atom atom__145_ as _sexp__147_ ->
       (match atom__145_ with
        | "A" -> `A
        | "B" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__149_ _sexp__147_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__145_ :: sexp_args__148_) as
       _sexp__147_ ->
       (match atom__145_ with
        | "B" as _tag__150_ ->
          (match sexp_args__148_ with
           | [ arg0__151_ ] ->
             let res0__152_ = int_of_sexp arg0__151_ in
             `B res0__152_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__149_
               _tag__150_
               _sexp__147_)
        | "A" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__149_ _sexp__147_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__146_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__149_ sexp__146_
     | Sexplib0.Sexp.List [] as sexp__146_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__149_ sexp__146_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__154_ = "expansion.ml.Poly_variant.t" in
     fun sexp__153_ ->
       try __t_of_sexp__ sexp__153_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__154_ sexp__153_
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | `A -> Sexplib0.Sexp.Atom "A"
      | `B v__155_ -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int v__155_ ]
                      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Inline_poly_variant = struct
  type t =
    [ Poly_variant.t
    | `C of int * int
    ]
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__167_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__156_ ->
       try (Poly_variant.__t_of_sexp__ sexp__156_ :> t) with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         (match sexp__156_ with
          | Sexplib0.Sexp.Atom atom__157_ as _sexp__159_ ->
            (match atom__157_ with
             | "C" ->
               Sexplib0.Sexp_conv_error.ptag_takes_args error_source__167_ _sexp__159_
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__157_ :: sexp_args__160_) as
            _sexp__159_ ->
            (match atom__157_ with
             | "C" as _tag__161_ ->
               (match sexp_args__160_ with
                | [ arg0__168_ ] ->
                  let res0__169_ =
                    match arg0__168_ with
                    | Sexplib0.Sexp.List [ arg0__162_; arg1__163_ ] ->
                      let res0__164_ = int_of_sexp arg0__162_
                      and res1__165_ = int_of_sexp arg1__163_ in
                      res0__164_, res1__165_
                    | sexp__166_ ->
                      Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                        error_source__167_
                        2
                        sexp__166_
                  in
                  `C res0__169_
                | _ ->
                  Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                    error_source__167_
                    _tag__161_
                    _sexp__159_)
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__158_ ->
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
              error_source__167_
              sexp__158_
          | Sexplib0.Sexp.List [] as sexp__158_ ->
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
              error_source__167_
              sexp__158_)
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__171_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__170_ ->
       try __t_of_sexp__ sexp__170_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__171_ sexp__170_
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | #Poly_variant.t as v__172_ -> Poly_variant.sexp_of_t v__172_
      | `C v__173_ ->
        Sexplib0.Sexp.List
          [ Sexplib0.Sexp.Atom "C"
          ; (let arg0__174_, arg1__175_ = v__173_ in
             let res0__176_ = sexp_of_int arg0__174_
             and res1__177_ = sexp_of_int arg1__175_ in
             Sexplib0.Sexp.List [ res0__176_; res1__177_ ])
          ]
        : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Recursive = struct
  type t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let error_source__180_ = "expansion.ml.Recursive.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__183_) :: sexp_args__184_) as
       _sexp__182_ ->
       (match sexp_args__184_ with
        | [ arg0__185_ ] ->
          let res0__186_ = t_of_sexp arg0__185_ in
          Banana res0__186_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__180_
            _tag__183_
            _sexp__182_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__181_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__180_ sexp__181_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__181_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__180_ sexp__181_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__179_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__180_ sexp__179_
     | Sexplib0.Sexp.List [] as sexp__179_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__180_ sexp__179_
     | sexp__179_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__180_ sexp__179_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
      | Banana arg0__187_ ->
        let res0__188_ = sexp_of_t arg0__187_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__188_ ]
      | Orange -> Sexplib0.Sexp.Atom "Orange"
                  : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Nonrecursive = struct
  open Recursive

  type nonrec t = t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (t_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (sexp_of_t : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Mutually_recursive = struct
  type a =
    | A
    | B of b
    | C of
        { a : a
        ; b : b
        ; c : c
        }

  and b =
    { a : a
    ; b : b
    }

  and c = int [@@deriving_inline sexp]

  let _ = fun (_ : a) -> ()
  let _ = fun (_ : b) -> ()
  let _ = fun (_ : c) -> ()

  let rec a_of_sexp =
    (let error_source__192_ = "expansion.ml.Mutually_recursive.a" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__195_) :: sexp_args__196_) as
       _sexp__194_ ->
       (match sexp_args__196_ with
        | [ arg0__197_ ] ->
          let res0__198_ = b_of_sexp arg0__197_ in
          B res0__198_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__192_
            _tag__195_
            _sexp__194_)
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("c" | "C") as _tag__201_) :: field_sexps__199_) as
       sexp__200_ ->
       let a__202_ = Stdlib.ref Stdlib.Option.None
       and b__204_ = Stdlib.ref Stdlib.Option.None
       and c__206_ = Stdlib.ref Stdlib.Option.None
       and duplicates__208_ = Stdlib.ref []
       and extra__209_ = Stdlib.ref [] in
       let rec iter__216_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__210_ :: (([] | [ _ ]) as _field_sexps__212_))
           :: tail__217_ ->
           let _field_sexp__211_ () =
             match _field_sexps__212_ with
             | [ x__218_ ] -> x__218_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__192_
                 sexp__200_
             | _ -> assert false
           in
           (match field_name__210_ with
            | "a" ->
              (match Stdlib.( ! ) a__202_ with
               | Stdlib.Option.None ->
                 let _field_sexp__211_ = _field_sexp__211_ () in
                 let fvalue__215_ = a_of_sexp _field_sexp__211_ in
                 Stdlib.( := ) a__202_ (Stdlib.Option.Some fvalue__215_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__208_
                   (field_name__210_ :: Stdlib.( ! ) duplicates__208_))
            | "b" ->
              (match Stdlib.( ! ) b__204_ with
               | Stdlib.Option.None ->
                 let _field_sexp__211_ = _field_sexp__211_ () in
                 let fvalue__214_ = b_of_sexp _field_sexp__211_ in
                 Stdlib.( := ) b__204_ (Stdlib.Option.Some fvalue__214_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__208_
                   (field_name__210_ :: Stdlib.( ! ) duplicates__208_))
            | "c" ->
              (match Stdlib.( ! ) c__206_ with
               | Stdlib.Option.None ->
                 let _field_sexp__211_ = _field_sexp__211_ () in
                 let fvalue__213_ = c_of_sexp _field_sexp__211_ in
                 Stdlib.( := ) c__206_ (Stdlib.Option.Some fvalue__213_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__208_
                   (field_name__210_ :: Stdlib.( ! ) duplicates__208_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__209_ (field_name__210_ :: Stdlib.( ! ) extra__209_)
              else ());
           iter__216_ tail__217_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__200_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__192_
             sexp__200_
         | [] -> ()
       in
       iter__216_ field_sexps__199_;
       (match Stdlib.( ! ) duplicates__208_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__192_
            (Stdlib.( ! ) duplicates__208_)
            sexp__200_
        | [] ->
          (match Stdlib.( ! ) extra__209_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__192_
               (Stdlib.( ! ) extra__209_)
               sexp__200_
           | [] ->
             (match Stdlib.( ! ) a__202_, Stdlib.( ! ) b__204_, Stdlib.( ! ) c__206_ with
              | ( Stdlib.Option.Some a__203_
                , Stdlib.Option.Some b__205_
                , Stdlib.Option.Some c__207_ ) -> C { a = a__203_; b = b__205_; c = c__207_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__192_
                  sexp__200_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__202_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__204_) Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) c__206_) Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__193_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__192_ sexp__193_
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp__193_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__192_ sexp__193_
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp__193_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__192_ sexp__193_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__191_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__192_ sexp__191_
     | Sexplib0.Sexp.List [] as sexp__191_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__192_ sexp__191_
     | sexp__191_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__192_ sexp__191_
       : Sexplib0.Sexp.t -> a)

  and b_of_sexp =
    (let error_source__233_ = "expansion.ml.Mutually_recursive.b" in
     function
     | Sexplib0.Sexp.List field_sexps__221_ as sexp__220_ ->
       let a__222_ = Stdlib.ref Stdlib.Option.None
       and b__224_ = Stdlib.ref Stdlib.Option.None
       and duplicates__226_ = Stdlib.ref []
       and extra__227_ = Stdlib.ref [] in
       let rec iter__234_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__228_ :: (([] | [ _ ]) as _field_sexps__230_))
           :: tail__235_ ->
           let _field_sexp__229_ () =
             match _field_sexps__230_ with
             | [ x__236_ ] -> x__236_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__233_
                 sexp__220_
             | _ -> assert false
           in
           (match field_name__228_ with
            | "a" ->
              (match Stdlib.( ! ) a__222_ with
               | Stdlib.Option.None ->
                 let _field_sexp__229_ = _field_sexp__229_ () in
                 let fvalue__232_ = a_of_sexp _field_sexp__229_ in
                 Stdlib.( := ) a__222_ (Stdlib.Option.Some fvalue__232_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__226_
                   (field_name__228_ :: Stdlib.( ! ) duplicates__226_))
            | "b" ->
              (match Stdlib.( ! ) b__224_ with
               | Stdlib.Option.None ->
                 let _field_sexp__229_ = _field_sexp__229_ () in
                 let fvalue__231_ = b_of_sexp _field_sexp__229_ in
                 Stdlib.( := ) b__224_ (Stdlib.Option.Some fvalue__231_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__226_
                   (field_name__228_ :: Stdlib.( ! ) duplicates__226_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__227_ (field_name__228_ :: Stdlib.( ! ) extra__227_)
              else ());
           iter__234_ tail__235_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__220_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__233_
             sexp__220_
         | [] -> ()
       in
       iter__234_ field_sexps__221_;
       (match Stdlib.( ! ) duplicates__226_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__233_
            (Stdlib.( ! ) duplicates__226_)
            sexp__220_
        | [] ->
          (match Stdlib.( ! ) extra__227_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__233_
               (Stdlib.( ! ) extra__227_)
               sexp__220_
           | [] ->
             (match Stdlib.( ! ) a__222_, Stdlib.( ! ) b__224_ with
              | Stdlib.Option.Some a__223_, Stdlib.Option.Some b__225_ ->
                { a = a__223_; b = b__225_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__233_
                  sexp__220_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__222_) Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__224_) Stdlib.Option.None, "b"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp__220_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__233_ sexp__220_
       : Sexplib0.Sexp.t -> b)

  and c_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> c)

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
      | A -> Sexplib0.Sexp.Atom "A"
      | B arg0__238_ ->
        let res0__239_ = sexp_of_b arg0__238_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__239_ ]
      | C { a = a__241_; b = b__243_; c = c__245_ } ->
        let bnds__240_ = [] in
        let bnds__240_ =
          let arg__246_ = sexp_of_c c__245_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__246_ ] :: bnds__240_
        in
        let bnds__240_ =
          let arg__244_ = sexp_of_b b__243_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__244_ ] :: bnds__240_
        in
        let bnds__240_ =
          let arg__242_ = sexp_of_a a__241_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__242_ ] :: bnds__240_
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__240_)
        : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = a__248_; b = b__250_ } ->
       let bnds__247_ = [] in
       let bnds__247_ =
         let arg__251_ = sexp_of_b b__250_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__251_ ] :: bnds__247_
       in
       let bnds__247_ =
         let arg__249_ = sexp_of_a a__248_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__249_ ] :: bnds__247_
       in
       Sexplib0.Sexp.List bnds__247_
       : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (sexp_of_int : c -> Sexplib0.Sexp.t)

  let _ = sexp_of_a
  and _ = sexp_of_b
  and _ = sexp_of_c

  [@@@end]
end

module Alias = struct
  type t = Recursive.t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (Recursive.t_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (Recursive.sexp_of_t : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Re_export = struct
  type t = Recursive.t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let error_source__255_ = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__258_) :: sexp_args__259_) as
       _sexp__257_ ->
       (match sexp_args__259_ with
        | [ arg0__260_ ] ->
          let res0__261_ = t_of_sexp arg0__260_ in
          Banana res0__261_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__255_
            _tag__258_
            _sexp__257_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__256_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__255_ sexp__256_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__256_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__255_ sexp__256_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__254_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__255_ sexp__254_
     | Sexplib0.Sexp.List [] as sexp__254_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__255_ sexp__254_
     | sexp__254_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__255_ sexp__254_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
      | Banana arg0__262_ ->
        let res0__263_ = sexp_of_t arg0__262_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__263_ ]
      | Orange -> Sexplib0.Sexp.Atom "Orange"
                  : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Unary = struct
  type 'a t = 'a list option [@@deriving_inline sexp]

  let _ = fun (_ : 'a t) -> ()

  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
    fun _of_a__264_ x__266_ -> option_of_sexp (list_of_sexp _of_a__264_) x__266_
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__267_ x__268_ -> sexp_of_option (sexp_of_list _of_a__267_) x__268_
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Binary = struct
  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
    'a 'b.
    (Sexplib0.Sexp.t -> 'a)
    -> (Sexplib0.Sexp.t -> 'b)
    -> Sexplib0.Sexp.t
    -> ('a, 'b) t
    =
    Either.t_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
    'a 'b.
    ('a -> Sexplib0.Sexp.t)
    -> ('b -> Sexplib0.Sexp.t)
    -> ('a, 'b) t
    -> Sexplib0.Sexp.t
    =
    Either.sexp_of_t
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module First_order = struct
  type 'a t = 'a -> 'a [@@deriving_inline sexp]

  let _ = fun (_ : 'a t) -> ()

  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
    fun _of_a__276_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__278_ _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Second_order = struct
  type ('a, 'b) t = ('a -> 'a) -> ('a -> 'b) -> ('b -> 'b) -> 'a -> 'b
  [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
    'a 'b.
    (Sexplib0.Sexp.t -> 'a)
    -> (Sexplib0.Sexp.t -> 'b)
    -> Sexplib0.Sexp.t
    -> ('a, 'b) t
    =
    fun _of_a__279_ _of_b__280_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
    'a 'b.
    ('a -> Sexplib0.Sexp.t)
    -> ('b -> Sexplib0.Sexp.t)
    -> ('a, 'b) t
    -> Sexplib0.Sexp.t
    =
    fun _of_a__282_ _of_b__283_ _ ->
    Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Named_arguments = struct
  type t = ?a:int -> b:int -> int -> int [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (Sexplib0.Sexp_conv.fun_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp

  let sexp_of_t =
    (fun _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
              : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Gadt = struct
  type _ t =
    | A : _ option t
    | B : int -> int t
    | C : 'a list -> unit t
  [@@deriving_inline sexp_of]

  let _ = fun (_ : _ t) -> ()

  let sexp_of_t : 'a__285_. ('a__285_ -> Sexplib0.Sexp.t) -> 'a__285_ t -> Sexplib0.Sexp.t
    =
    fun (type a__291_) : ((a__291_ -> Sexplib0.Sexp.t) -> a__291_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__286_ -> function
      | A -> Sexplib0.Sexp.Atom "A"
      | B arg0__287_ ->
        let res0__288_ = sexp_of_int arg0__287_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__288_ ]
      | C arg0__289_ ->
        let res0__290_ = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") arg0__289_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; res0__290_ ]
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Recursive_record_containing_variant = struct
  type t =
    { a : [ `A of t ]
    ; b : [ `B ] [@sexp_drop_default Poly.equal] [@default `B]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let (default__324_ : [ `B ]) = `B in
     let error_source__310_ = "expansion.ml.Recursive_record_containing_variant.t" in
     function
     | Sexplib0.Sexp.List field_sexps__294_ as sexp__293_ ->
       let a__295_ = Stdlib.ref Stdlib.Option.None
       and b__297_ = Stdlib.ref Stdlib.Option.None
       and duplicates__299_ = Stdlib.ref []
       and extra__300_ = Stdlib.ref [] in
       let rec iter__326_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__301_ :: (([] | [ _ ]) as _field_sexps__303_))
           :: tail__327_ ->
           let _field_sexp__302_ () =
             match _field_sexps__303_ with
             | [ x__328_ ] -> x__328_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__310_
                 sexp__293_
             | _ -> assert false
           in
           (match field_name__301_ with
            | "a" ->
              (match Stdlib.( ! ) a__295_ with
               | Stdlib.Option.None ->
                 let _field_sexp__302_ = _field_sexp__302_ () in
                 let fvalue__322_ =
                   let sexp__321_ = _field_sexp__302_ in
                   try
                     match sexp__321_ with
                     | Sexplib0.Sexp.Atom atom__314_ as _sexp__316_ ->
                       (match atom__314_ with
                        | "A" ->
                          Sexplib0.Sexp_conv_error.ptag_takes_args
                            error_source__310_
                            _sexp__316_
                        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__314_ :: sexp_args__317_)
                       as _sexp__316_ ->
                       (match atom__314_ with
                        | "A" as _tag__318_ ->
                          (match sexp_args__317_ with
                           | [ arg0__319_ ] ->
                             let res0__320_ = t_of_sexp arg0__319_ in
                             `A res0__320_
                           | _ ->
                             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                               error_source__310_
                               _tag__318_
                               _sexp__316_)
                        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__315_ ->
                       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                         error_source__310_
                         sexp__315_
                     | Sexplib0.Sexp.List [] as sexp__315_ ->
                       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                         error_source__310_
                         sexp__315_
                   with
                   | Sexplib0.Sexp_conv_error.No_variant_match ->
                     Sexplib0.Sexp_conv_error.no_matching_variant_found
                       error_source__310_
                       sexp__321_
                 in
                 Stdlib.( := ) a__295_ (Stdlib.Option.Some fvalue__322_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__299_
                   (field_name__301_ :: Stdlib.( ! ) duplicates__299_))
            | "b" ->
              (match Stdlib.( ! ) b__297_ with
               | Stdlib.Option.None ->
                 let _field_sexp__302_ = _field_sexp__302_ () in
                 let fvalue__312_ =
                   let sexp__311_ = _field_sexp__302_ in
                   try
                     match sexp__311_ with
                     | Sexplib0.Sexp.Atom atom__306_ as _sexp__308_ ->
                       (match atom__306_ with
                        | "B" -> `B
                        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__306_ :: _) as
                       _sexp__308_ ->
                       (match atom__306_ with
                        | "B" ->
                          Sexplib0.Sexp_conv_error.ptag_no_args
                            error_source__310_
                            _sexp__308_
                        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__307_ ->
                       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                         error_source__310_
                         sexp__307_
                     | Sexplib0.Sexp.List [] as sexp__307_ ->
                       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                         error_source__310_
                         sexp__307_
                   with
                   | Sexplib0.Sexp_conv_error.No_variant_match ->
                     Sexplib0.Sexp_conv_error.no_matching_variant_found
                       error_source__310_
                       sexp__311_
                 in
                 Stdlib.( := ) b__297_ (Stdlib.Option.Some fvalue__312_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__299_
                   (field_name__301_ :: Stdlib.( ! ) duplicates__299_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__300_ (field_name__301_ :: Stdlib.( ! ) extra__300_)
              else ());
           iter__326_ tail__327_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__293_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__310_
             sexp__293_
         | [] -> ()
       in
       iter__326_ field_sexps__294_;
       (match Stdlib.( ! ) duplicates__299_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__310_
            (Stdlib.( ! ) duplicates__299_)
            sexp__293_
        | [] ->
          (match Stdlib.( ! ) extra__300_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__310_
               (Stdlib.( ! ) extra__300_)
               sexp__293_
           | [] ->
             (match Stdlib.( ! ) a__295_, Stdlib.( ! ) b__297_ with
              | Stdlib.Option.Some a__296_, b__298_ ->
                { a = a__296_
                ; b =
                    (match b__298_ with
                     | Stdlib.Option.None -> default__324_
                     | Stdlib.Option.Some v__325_ -> v__325_)
                }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__310_
                  sexp__293_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__295_) Stdlib.Option.None, "a" ])))
     | Sexplib0.Sexp.Atom _ as sexp__293_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__310_ sexp__293_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (let (default__335_ : [ `B ]) = `B
     and (drop_default__334_ : [ `B ] -> [ `B ] -> Stdlib.Bool.t) = Poly.equal in
     fun { a = a__330_; b = b__336_ } ->
       let bnds__329_ = [] in
       let bnds__329_ =
         if drop_default__334_ default__335_ b__336_
         then bnds__329_
         else (
           let arg__338_ = (fun `B -> Sexplib0.Sexp.Atom "B") b__336_ in
           let bnd__337_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__338_ ] in
           bnd__337_ :: bnds__329_)
       in
       let bnds__329_ =
         let arg__331_ =
           let (`A v__332_) = a__330_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v__332_ ]
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__331_ ] :: bnds__329_
       in
       Sexplib0.Sexp.List bnds__329_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_record = struct
  type t =
    { a : 'a. 'a list
    ; b : 'b. 'b option
    ; c : 'c. 'c
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__355_ = "expansion.ml.Poly_record.t" in
     function
     | Sexplib0.Sexp.List field_sexps__341_ as sexp__340_ ->
       let a__366_, b__367_, c__368_ =
         let a__342_ = Stdlib.ref Stdlib.Option.None
         and b__344_ = Stdlib.ref Stdlib.Option.None
         and c__346_ = Stdlib.ref Stdlib.Option.None
         and duplicates__348_ = Stdlib.ref []
         and extra__349_ = Stdlib.ref [] in
         let rec iter__363_ = function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom field_name__350_
                :: (([] | [ _ ]) as _field_sexps__352_))
             :: tail__364_ ->
             let _field_sexp__351_ () =
               match _field_sexps__352_ with
               | [ x__365_ ] -> x__365_
               | [] ->
                 Sexplib0.Sexp_conv_error.record_only_pairs_expected
                   error_source__355_
                   sexp__340_
               | _ -> assert false
             in
             (match field_name__350_ with
              | "a" ->
                (match Stdlib.( ! ) a__342_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__351_ = _field_sexp__351_ () in
                   let fvalue__362_ =
                     let _of_a__360_ sexp__361_ =
                       Sexplib0.Sexp_conv_error.record_poly_field_value
                         error_source__355_
                         sexp__361_
                     in
                     list_of_sexp _of_a__360_ _field_sexp__351_
                   in
                   Stdlib.( := ) a__342_ (Stdlib.Option.Some fvalue__362_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__348_
                     (field_name__350_ :: Stdlib.( ! ) duplicates__348_))
              | "b" ->
                (match Stdlib.( ! ) b__344_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__351_ = _field_sexp__351_ () in
                   let fvalue__359_ =
                     let _of_b__357_ sexp__358_ =
                       Sexplib0.Sexp_conv_error.record_poly_field_value
                         error_source__355_
                         sexp__358_
                     in
                     option_of_sexp _of_b__357_ _field_sexp__351_
                   in
                   Stdlib.( := ) b__344_ (Stdlib.Option.Some fvalue__359_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__348_
                     (field_name__350_ :: Stdlib.( ! ) duplicates__348_))
              | "c" ->
                (match Stdlib.( ! ) c__346_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__351_ = _field_sexp__351_ () in
                   let fvalue__356_ =
                     let _of_c__353_ sexp__354_ =
                       Sexplib0.Sexp_conv_error.record_poly_field_value
                         error_source__355_
                         sexp__354_
                     in
                     _of_c__353_ _field_sexp__351_
                   in
                   Stdlib.( := ) c__346_ (Stdlib.Option.Some fvalue__356_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__348_
                     (field_name__350_ :: Stdlib.( ! ) duplicates__348_))
              | _ ->
                if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
                then
                  Stdlib.( := ) extra__349_ (field_name__350_ :: Stdlib.( ! ) extra__349_)
                else ());
             iter__363_ tail__364_
           | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__340_) :: _ ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected
               error_source__355_
               sexp__340_
           | [] -> ()
         in
         iter__363_ field_sexps__341_;
         match Stdlib.( ! ) duplicates__348_ with
         | _ :: _ ->
           Sexplib0.Sexp_conv_error.record_duplicate_fields
             error_source__355_
             (Stdlib.( ! ) duplicates__348_)
             sexp__340_
         | [] ->
           (match Stdlib.( ! ) extra__349_ with
            | _ :: _ ->
              Sexplib0.Sexp_conv_error.record_extra_fields
                error_source__355_
                (Stdlib.( ! ) extra__349_)
                sexp__340_
            | [] ->
              (match Stdlib.( ! ) a__342_, Stdlib.( ! ) b__344_, Stdlib.( ! ) c__346_ with
               | ( Stdlib.Option.Some a__343_
                 , Stdlib.Option.Some b__345_
                 , Stdlib.Option.Some c__347_ ) -> a__343_, b__345_, c__347_
               | _ ->
                 Sexplib0.Sexp_conv_error.record_undefined_elements
                   error_source__355_
                   sexp__340_
                   [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__342_) Stdlib.Option.None, "a"
                   ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) b__344_) Stdlib.Option.None, "b"
                   ; Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) c__346_) Stdlib.Option.None, "c"
                   ]))
       in
       { a = a__366_; b = b__367_; c = c__368_ }
     | Sexplib0.Sexp.Atom _ as sexp__340_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__355_ sexp__340_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__370_; b = b__373_; c = c__376_ } ->
       let bnds__369_ = [] in
       let bnds__369_ =
         let arg__377_ =
           let _of_c__378_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           _of_c__378_ c__376_
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__377_ ] :: bnds__369_
       in
       let bnds__369_ =
         let arg__374_ =
           let _of_b__375_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_option _of_b__375_ b__373_
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__374_ ] :: bnds__369_
       in
       let bnds__369_ =
         let arg__371_ =
           let _of_a__372_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_list _of_a__372_ a__370_
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__371_ ] :: bnds__369_
       in
       Sexplib0.Sexp.List bnds__369_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_defaults = struct
  type t =
    { a : int [@default 0]
    ; b : int [@default 0] [@sexp_drop_default.compare]
    ; c : int [@default 0] [@sexp_drop_default.equal]
    ; d : int [@default 0] [@sexp_drop_default.sexp]
    ; e : int [@default 0] [@sexp_drop_default ( = )]
    ; f : int [@sexp_drop_if ( = ) 0]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let (default__415_ : int) = 0
     and (default__417_ : int) = 0
     and (default__419_ : int) = 0
     and (default__421_ : int) = 0
     and (default__423_ : int) = 0 in
     let error_source__425_ = "expansion.ml.Record_with_defaults.t" in
     function
     | Sexplib0.Sexp.List field_sexps__381_ as sexp__380_ ->
       let a__382_ = Stdlib.ref Stdlib.Option.None
       and b__384_ = Stdlib.ref Stdlib.Option.None
       and c__386_ = Stdlib.ref Stdlib.Option.None
       and d__388_ = Stdlib.ref Stdlib.Option.None
       and e__390_ = Stdlib.ref Stdlib.Option.None
       and f__392_ = Stdlib.ref Stdlib.Option.None
       and duplicates__394_ = Stdlib.ref []
       and extra__395_ = Stdlib.ref [] in
       let rec iter__426_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__396_ :: (([] | [ _ ]) as _field_sexps__398_))
           :: tail__427_ ->
           let _field_sexp__397_ () =
             match _field_sexps__398_ with
             | [ x__428_ ] -> x__428_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__425_
                 sexp__380_
             | _ -> assert false
           in
           (match field_name__396_ with
            | "a" ->
              (match Stdlib.( ! ) a__382_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__409_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) a__382_ (Stdlib.Option.Some fvalue__409_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | "b" ->
              (match Stdlib.( ! ) b__384_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__407_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) b__384_ (Stdlib.Option.Some fvalue__407_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | "c" ->
              (match Stdlib.( ! ) c__386_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__405_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) c__386_ (Stdlib.Option.Some fvalue__405_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | "d" ->
              (match Stdlib.( ! ) d__388_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__403_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) d__388_ (Stdlib.Option.Some fvalue__403_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | "e" ->
              (match Stdlib.( ! ) e__390_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__401_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) e__390_ (Stdlib.Option.Some fvalue__401_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | "f" ->
              (match Stdlib.( ! ) f__392_ with
               | Stdlib.Option.None ->
                 let _field_sexp__397_ = _field_sexp__397_ () in
                 let fvalue__399_ = int_of_sexp _field_sexp__397_ in
                 Stdlib.( := ) f__392_ (Stdlib.Option.Some fvalue__399_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__394_
                   (field_name__396_ :: Stdlib.( ! ) duplicates__394_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__395_ (field_name__396_ :: Stdlib.( ! ) extra__395_)
              else ());
           iter__426_ tail__427_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__380_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__425_
             sexp__380_
         | [] -> ()
       in
       iter__426_ field_sexps__381_;
       (match Stdlib.( ! ) duplicates__394_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__425_
            (Stdlib.( ! ) duplicates__394_)
            sexp__380_
        | [] ->
          (match Stdlib.( ! ) extra__395_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__425_
               (Stdlib.( ! ) extra__395_)
               sexp__380_
           | [] ->
             (match
                ( Stdlib.( ! ) a__382_
                , Stdlib.( ! ) b__384_
                , Stdlib.( ! ) c__386_
                , Stdlib.( ! ) d__388_
                , Stdlib.( ! ) e__390_
                , Stdlib.( ! ) f__392_ )
              with
              | a__383_, b__385_, c__387_, d__389_, e__391_, Stdlib.Option.Some f__393_ ->
                { a =
                    (match a__383_ with
                     | Stdlib.Option.None -> default__415_
                     | Stdlib.Option.Some v__416_ -> v__416_)
                ; b =
                    (match b__385_ with
                     | Stdlib.Option.None -> default__417_
                     | Stdlib.Option.Some v__418_ -> v__418_)
                ; c =
                    (match c__387_ with
                     | Stdlib.Option.None -> default__419_
                     | Stdlib.Option.Some v__420_ -> v__420_)
                ; d =
                    (match d__389_ with
                     | Stdlib.Option.None -> default__421_
                     | Stdlib.Option.Some v__422_ -> v__422_)
                ; e =
                    (match e__391_ with
                     | Stdlib.Option.None -> default__423_
                     | Stdlib.Option.Some v__424_ -> v__424_)
                ; f = f__393_
                }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__425_
                  sexp__380_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) f__392_) Stdlib.Option.None, "f" ])))
     | Sexplib0.Sexp.Atom _ as sexp__380_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__425_ sexp__380_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let (default__433_ : int) = 0
     and (default__438_ : int) = 0
     and (default__443_ : int) = 0
     and (default__449_ : int) = 0
     and (drop_default__448_ : int -> int -> Stdlib.Bool.t) = ( = )
     and (drop_if__454_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t) = fun () -> ( = ) 0 in
     fun { a = a__430_; b = b__434_; c = c__439_; d = d__444_; e = e__450_; f = f__455_ } ->
       let bnds__429_ = [] in
       let bnds__429_ =
         if (drop_if__454_ ()) f__455_
         then bnds__429_
         else (
           let arg__457_ = sexp_of_int f__455_ in
           let bnd__456_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__457_ ] in
           bnd__456_ :: bnds__429_)
       in
       let bnds__429_ =
         if drop_default__448_ default__449_ e__450_
         then bnds__429_
         else (
           let arg__452_ = sexp_of_int e__450_ in
           let bnd__451_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__452_ ] in
           bnd__451_ :: bnds__429_)
       in
       let bnds__429_ =
         let arg__446_ = sexp_of_int d__444_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__443_) arg__446_
         then bnds__429_
         else (
           let bnd__445_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__446_ ] in
           bnd__445_ :: bnds__429_)
       in
       let bnds__429_ =
         if [%equal: int] default__438_ c__439_
         then bnds__429_
         else (
           let arg__441_ = sexp_of_int c__439_ in
           let bnd__440_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__441_ ] in
           bnd__440_ :: bnds__429_)
       in
       let bnds__429_ =
         if [%compare.equal: int] default__433_ b__434_
         then bnds__429_
         else (
           let arg__436_ = sexp_of_int b__434_ in
           let bnd__435_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__436_ ] in
           bnd__435_ :: bnds__429_)
       in
       let bnds__429_ =
         let arg__431_ = sexp_of_int a__430_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__431_ ] :: bnds__429_
       in
       Sexplib0.Sexp.List bnds__429_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_special_types = struct
  type t =
    { a : int option [@sexp.option]
    ; b : int list [@sexp.list]
    ; c : int array [@sexp.array]
    ; d : bool [@sexp.bool]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__480_ = "expansion.ml.Record_with_special_types.t" in
     function
     | Sexplib0.Sexp.List field_sexps__466_ as sexp__465_ ->
       let a__467_ = Stdlib.ref Stdlib.Option.None
       and b__469_ = Stdlib.ref Stdlib.Option.None
       and c__471_ = Stdlib.ref Stdlib.Option.None
       and d__473_ = Stdlib.ref false
       and duplicates__475_ = Stdlib.ref []
       and extra__476_ = Stdlib.ref [] in
       let rec iter__486_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__477_ :: (([] | [ _ ]) as _field_sexps__479_))
           :: tail__487_ ->
           let _field_sexp__478_ () =
             match _field_sexps__479_ with
             | [ x__488_ ] -> x__488_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__480_
                 sexp__465_
             | _ -> assert false
           in
           (match field_name__477_ with
            | "a" ->
              (match Stdlib.( ! ) a__467_ with
               | Stdlib.Option.None ->
                 let _field_sexp__478_ = _field_sexp__478_ () in
                 let fvalue__483_ = int_of_sexp _field_sexp__478_ in
                 Stdlib.( := ) a__467_ (Stdlib.Option.Some fvalue__483_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__475_
                   (field_name__477_ :: Stdlib.( ! ) duplicates__475_))
            | "b" ->
              (match Stdlib.( ! ) b__469_ with
               | Stdlib.Option.None ->
                 let _field_sexp__478_ = _field_sexp__478_ () in
                 let fvalue__482_ = list_of_sexp int_of_sexp _field_sexp__478_ in
                 Stdlib.( := ) b__469_ (Stdlib.Option.Some fvalue__482_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__475_
                   (field_name__477_ :: Stdlib.( ! ) duplicates__475_))
            | "c" ->
              (match Stdlib.( ! ) c__471_ with
               | Stdlib.Option.None ->
                 let _field_sexp__478_ = _field_sexp__478_ () in
                 let fvalue__481_ = array_of_sexp int_of_sexp _field_sexp__478_ in
                 Stdlib.( := ) c__471_ (Stdlib.Option.Some fvalue__481_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__475_
                   (field_name__477_ :: Stdlib.( ! ) duplicates__475_))
            | "d" ->
              if Stdlib.( ! ) d__473_
              then
                Stdlib.( := )
                  duplicates__475_
                  (field_name__477_ :: Stdlib.( ! ) duplicates__475_)
              else (
                match _field_sexps__479_ with
                | [] -> Stdlib.( := ) d__473_ true
                | _ :: _ ->
                  Sexplib0.Sexp_conv_error.record_sexp_bool_with_payload
                    error_source__480_
                    sexp__465_)
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__476_ (field_name__477_ :: Stdlib.( ! ) extra__476_)
              else ());
           iter__486_ tail__487_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__465_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__480_
             sexp__465_
         | [] -> ()
       in
       iter__486_ field_sexps__466_;
       (match Stdlib.( ! ) duplicates__475_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__480_
            (Stdlib.( ! ) duplicates__475_)
            sexp__465_
        | [] ->
          (match Stdlib.( ! ) extra__476_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__480_
               (Stdlib.( ! ) extra__476_)
               sexp__465_
           | [] ->
             (match
                ( Stdlib.( ! ) a__467_
                , Stdlib.( ! ) b__469_
                , Stdlib.( ! ) c__471_
                , Stdlib.( ! ) d__473_ )
              with
              | a__468_, b__470_, c__472_, d__474_ ->
                { a = a__468_
                ; b =
                    (match b__470_ with
                     | Stdlib.Option.None -> []
                     | Stdlib.Option.Some v__484_ -> v__484_)
                ; c =
                    (match c__472_ with
                     | Stdlib.Option.None -> [||]
                     | Stdlib.Option.Some v__485_ -> v__485_)
                ; d = d__474_
                })))
     | Sexplib0.Sexp.Atom _ as sexp__465_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__480_ sexp__465_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__490_; b = b__495_; c = c__499_; d = d__502_ } ->
       let bnds__489_ = [] in
       let bnds__489_ =
         if d__502_
         then (
           let bnd__503_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           bnd__503_ :: bnds__489_)
         else bnds__489_
       in
       let bnds__489_ =
         if match c__499_ with
           | [||] -> true
           | _ -> false
         then bnds__489_
         else (
           let arg__501_ = (sexp_of_array sexp_of_int) c__499_ in
           let bnd__500_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__501_ ] in
           bnd__500_ :: bnds__489_)
       in
       let bnds__489_ =
         if match b__495_ with
           | [] -> true
           | _ -> false
         then bnds__489_
         else (
           let arg__497_ = (sexp_of_list sexp_of_int) b__495_ in
           let bnd__496_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__497_ ] in
           bnd__496_ :: bnds__489_)
       in
       let bnds__489_ =
         match a__490_ with
         | Stdlib.Option.None -> bnds__489_
         | Stdlib.Option.Some v__491_ ->
           let arg__493_ = sexp_of_int v__491_ in
           let bnd__492_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__493_ ] in
           bnd__492_ :: bnds__489_
       in
       Sexplib0.Sexp.List bnds__489_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_omit_nil = struct
  type t =
    { a : int option [@sexp.omit_nil]
    ; b : int list [@sexp.omit_nil]
    ; c : unit [@sexp.omit_nil]
    ; d : int [@sexp.omit_nil]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__535_ = "expansion.ml.Record_with_omit_nil.t" in
     function
     | Sexplib0.Sexp.List field_sexps__506_ as sexp__505_ ->
       let a__507_ = Stdlib.ref Stdlib.Option.None
       and b__509_ = Stdlib.ref Stdlib.Option.None
       and c__511_ = Stdlib.ref Stdlib.Option.None
       and d__513_ = Stdlib.ref Stdlib.Option.None
       and duplicates__515_ = Stdlib.ref []
       and extra__516_ = Stdlib.ref [] in
       let rec iter__532_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__517_ :: (([] | [ _ ]) as _field_sexps__519_))
           :: tail__533_ ->
           let _field_sexp__518_ () =
             match _field_sexps__519_ with
             | [ x__534_ ] -> x__534_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__535_
                 sexp__505_
             | _ -> assert false
           in
           (match field_name__517_ with
            | "a" ->
              (match Stdlib.( ! ) a__507_ with
               | Stdlib.Option.None ->
                 let _field_sexp__518_ = _field_sexp__518_ () in
                 let fvalue__523_ = option_of_sexp int_of_sexp _field_sexp__518_ in
                 Stdlib.( := ) a__507_ (Stdlib.Option.Some fvalue__523_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__515_
                   (field_name__517_ :: Stdlib.( ! ) duplicates__515_))
            | "b" ->
              (match Stdlib.( ! ) b__509_ with
               | Stdlib.Option.None ->
                 let _field_sexp__518_ = _field_sexp__518_ () in
                 let fvalue__522_ = list_of_sexp int_of_sexp _field_sexp__518_ in
                 Stdlib.( := ) b__509_ (Stdlib.Option.Some fvalue__522_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__515_
                   (field_name__517_ :: Stdlib.( ! ) duplicates__515_))
            | "c" ->
              (match Stdlib.( ! ) c__511_ with
               | Stdlib.Option.None ->
                 let _field_sexp__518_ = _field_sexp__518_ () in
                 let fvalue__521_ = unit_of_sexp _field_sexp__518_ in
                 Stdlib.( := ) c__511_ (Stdlib.Option.Some fvalue__521_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__515_
                   (field_name__517_ :: Stdlib.( ! ) duplicates__515_))
            | "d" ->
              (match Stdlib.( ! ) d__513_ with
               | Stdlib.Option.None ->
                 let _field_sexp__518_ = _field_sexp__518_ () in
                 let fvalue__520_ = int_of_sexp _field_sexp__518_ in
                 Stdlib.( := ) d__513_ (Stdlib.Option.Some fvalue__520_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__515_
                   (field_name__517_ :: Stdlib.( ! ) duplicates__515_))
            | _ ->
              if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
              then Stdlib.( := ) extra__516_ (field_name__517_ :: Stdlib.( ! ) extra__516_)
              else ());
           iter__532_ tail__533_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__505_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__535_
             sexp__505_
         | [] -> ()
       in
       iter__532_ field_sexps__506_;
       (match Stdlib.( ! ) duplicates__515_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__535_
            (Stdlib.( ! ) duplicates__515_)
            sexp__505_
        | [] ->
          (match Stdlib.( ! ) extra__516_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__535_
               (Stdlib.( ! ) extra__516_)
               sexp__505_
           | [] ->
             (match
                ( Stdlib.( ! ) a__507_
                , Stdlib.( ! ) b__509_
                , Stdlib.( ! ) c__511_
                , Stdlib.( ! ) d__513_ )
              with
              | a__508_, b__510_, c__512_, d__514_ ->
                { a =
                    (match a__508_ with
                     | Stdlib.Option.Some v__525_ -> v__525_
                     | Stdlib.Option.None ->
                       (try option_of_sexp int_of_sexp (Sexplib0.Sexp.List []) with
                        | Sexplib0.Sexp_conv_error.Of_sexp_error (e__524_, _) ->
                          Stdlib.raise
                            (Sexplib0.Sexp_conv_error.Of_sexp_error (e__524_, sexp__505_))))
                ; b =
                    (match b__510_ with
                     | Stdlib.Option.Some v__527_ -> v__527_
                     | Stdlib.Option.None ->
                       (try list_of_sexp int_of_sexp (Sexplib0.Sexp.List []) with
                        | Sexplib0.Sexp_conv_error.Of_sexp_error (e__526_, _) ->
                          Stdlib.raise
                            (Sexplib0.Sexp_conv_error.Of_sexp_error (e__526_, sexp__505_))))
                ; c =
                    (match c__512_ with
                     | Stdlib.Option.Some v__529_ -> v__529_
                     | Stdlib.Option.None ->
                       (try unit_of_sexp (Sexplib0.Sexp.List []) with
                        | Sexplib0.Sexp_conv_error.Of_sexp_error (e__528_, _) ->
                          Stdlib.raise
                            (Sexplib0.Sexp_conv_error.Of_sexp_error (e__528_, sexp__505_))))
                ; d =
                    (match d__514_ with
                     | Stdlib.Option.Some v__531_ -> v__531_
                     | Stdlib.Option.None ->
                       (try int_of_sexp (Sexplib0.Sexp.List []) with
                        | Sexplib0.Sexp_conv_error.Of_sexp_error (e__530_, _) ->
                          Stdlib.raise
                            (Sexplib0.Sexp_conv_error.Of_sexp_error (e__530_, sexp__505_))))
                })))
     | Sexplib0.Sexp.Atom _ as sexp__505_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__535_ sexp__505_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__537_; b = b__539_; c = c__541_; d = d__543_ } ->
       let bnds__536_ = [] in
       let bnds__536_ =
         match sexp_of_int d__543_ with
         | Sexplib0.Sexp.List [] -> bnds__536_
         | arg__544_ ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__544_ ] :: bnds__536_
       in
       let bnds__536_ =
         match sexp_of_unit c__541_ with
         | Sexplib0.Sexp.List [] -> bnds__536_
         | arg__542_ ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__542_ ] :: bnds__536_
       in
       let bnds__536_ =
         match sexp_of_list sexp_of_int b__539_ with
         | Sexplib0.Sexp.List [] -> bnds__536_
         | arg__540_ ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__540_ ] :: bnds__536_
       in
       let bnds__536_ =
         match sexp_of_option sexp_of_int a__537_ with
         | Sexplib0.Sexp.List [] -> bnds__536_
         | arg__538_ ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__538_ ] :: bnds__536_
       in
       Sexplib0.Sexp.List bnds__536_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__547_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__550_) :: sexp_args__551_) as
       _sexp__549_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__551_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__548_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__547_ sexp__548_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__546_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__547_ sexp__546_
     | Sexplib0.Sexp.List [] as sexp__546_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__547_ sexp__546_
     | sexp__546_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__547_ sexp__546_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__552_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__552_)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__559_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__554_ as _sexp__556_ ->
       (match atom__554_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__559_ _sexp__556_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__554_ :: sexp_args__557_) as
       _sexp__556_ ->
       (match atom__554_ with
        | "A" as _tag__558_ -> `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__557_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__555_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__559_ sexp__555_
     | Sexplib0.Sexp.List [] as sexp__555_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__559_ sexp__555_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__561_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__560_ ->
       try __t_of_sexp__ sexp__560_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__561_ sexp__560_
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__562_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__562_)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__574_ = "expansion.ml.Record_allowing_extra_fields.t" in
     function
     | Sexplib0.Sexp.List field_sexps__565_ as sexp__564_ ->
       let a__566_ = Stdlib.ref Stdlib.Option.None
       and duplicates__568_ = Stdlib.ref []
       and extra__569_ = Stdlib.ref [] in
       let rec iter__575_ = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name__570_ :: (([] | [ _ ]) as _field_sexps__572_))
           :: tail__576_ ->
           let _field_sexp__571_ () =
             match _field_sexps__572_ with
             | [ x__577_ ] -> x__577_
             | [] ->
               Sexplib0.Sexp_conv_error.record_only_pairs_expected
                 error_source__574_
                 sexp__564_
             | _ -> assert false
           in
           (match field_name__570_ with
            | "a" ->
              (match Stdlib.( ! ) a__566_ with
               | Stdlib.Option.None ->
                 let _field_sexp__571_ = _field_sexp__571_ () in
                 let fvalue__573_ = int_of_sexp _field_sexp__571_ in
                 Stdlib.( := ) a__566_ (Stdlib.Option.Some fvalue__573_)
               | Stdlib.Option.Some _ ->
                 Stdlib.( := )
                   duplicates__568_
                   (field_name__570_ :: Stdlib.( ! ) duplicates__568_))
            | _ -> ());
           iter__575_ tail__576_
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__564_) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected
             error_source__574_
             sexp__564_
         | [] -> ()
       in
       iter__575_ field_sexps__565_;
       (match Stdlib.( ! ) duplicates__568_ with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            error_source__574_
            (Stdlib.( ! ) duplicates__568_)
            sexp__564_
        | [] ->
          (match Stdlib.( ! ) extra__569_ with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               error_source__574_
               (Stdlib.( ! ) extra__569_)
               sexp__564_
           | [] ->
             (match Stdlib.( ! ) a__566_ with
              | Stdlib.Option.Some a__567_ -> { a = a__567_ }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  error_source__574_
                  sexp__564_
                  [ Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) a__566_) Stdlib.Option.None, "a" ])))
     | Sexplib0.Sexp.Atom _ as sexp__564_ ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__574_ sexp__564_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__579_ } ->
       let bnds__578_ = [] in
       let bnds__578_ =
         let arg__580_ = sexp_of_int a__579_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__580_ ] :: bnds__578_
       in
       Sexplib0.Sexp.List bnds__578_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__582_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__582_
                    : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__583_ -> sexp_of_list Sexplib0.Sexp_conv.sexp_of_opaque x__583_
                    : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end
