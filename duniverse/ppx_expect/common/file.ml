open! Base
open Import

module Name : sig
  type t [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val relative_to : dir:string -> t -> string

  include Identifiable.S with type t := t
end = struct
  include String

  let relative_to ~dir t =
    if not (Stdlib.Filename.is_relative t) then t else Stdlib.Filename.concat dir t
  ;;
end

let initial_dir =
  let dir_or_error =
    match Stdlib.Sys.getcwd () with
    | v -> `Ok v
    | exception exn -> `Exn exn
  in
  fun () ->
    match dir_or_error with
    | `Ok v -> v
    | `Exn exn -> raise exn
;;

module Location = struct
  module T = struct
    type t =
      { filename : Name.t
      ; line_number : int
      ; line_start : int
      ; start_pos : int
      ; end_pos : int
      }
    [@@deriving_inline sexp, compare]

    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__024_ = "file.ml.Location.T.t" in
       function
       | Sexplib0.Sexp.List field_sexps__003_ as sexp__002_ ->
         let filename__004_ = Stdlib.ref Stdlib.Option.None
         and line_number__006_ = Stdlib.ref Stdlib.Option.None
         and line_start__008_ = Stdlib.ref Stdlib.Option.None
         and start_pos__010_ = Stdlib.ref Stdlib.Option.None
         and end_pos__012_ = Stdlib.ref Stdlib.Option.None
         and duplicates__014_ = Stdlib.ref []
         and extra__015_ = Stdlib.ref [] in
         let rec iter__025_ = function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom field_name__016_
                :: (([] | [ _ ]) as _field_sexps__018_))
             :: tail__026_ ->
             let _field_sexp__017_ () =
               match _field_sexps__018_ with
               | [ x__027_ ] -> x__027_
               | [] ->
                 Sexplib0.Sexp_conv_error.record_only_pairs_expected
                   error_source__024_
                   sexp__002_
               | _ -> assert false
             in
             (match field_name__016_ with
              | "filename" ->
                (match Stdlib.( ! ) filename__004_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__017_ = _field_sexp__017_ () in
                   let fvalue__023_ = Name.t_of_sexp _field_sexp__017_ in
                   Stdlib.( := ) filename__004_ (Stdlib.Option.Some fvalue__023_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__014_
                     (field_name__016_ :: Stdlib.( ! ) duplicates__014_))
              | "line_number" ->
                (match Stdlib.( ! ) line_number__006_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__017_ = _field_sexp__017_ () in
                   let fvalue__022_ = int_of_sexp _field_sexp__017_ in
                   Stdlib.( := ) line_number__006_ (Stdlib.Option.Some fvalue__022_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__014_
                     (field_name__016_ :: Stdlib.( ! ) duplicates__014_))
              | "line_start" ->
                (match Stdlib.( ! ) line_start__008_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__017_ = _field_sexp__017_ () in
                   let fvalue__021_ = int_of_sexp _field_sexp__017_ in
                   Stdlib.( := ) line_start__008_ (Stdlib.Option.Some fvalue__021_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__014_
                     (field_name__016_ :: Stdlib.( ! ) duplicates__014_))
              | "start_pos" ->
                (match Stdlib.( ! ) start_pos__010_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__017_ = _field_sexp__017_ () in
                   let fvalue__020_ = int_of_sexp _field_sexp__017_ in
                   Stdlib.( := ) start_pos__010_ (Stdlib.Option.Some fvalue__020_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__014_
                     (field_name__016_ :: Stdlib.( ! ) duplicates__014_))
              | "end_pos" ->
                (match Stdlib.( ! ) end_pos__012_ with
                 | Stdlib.Option.None ->
                   let _field_sexp__017_ = _field_sexp__017_ () in
                   let fvalue__019_ = int_of_sexp _field_sexp__017_ in
                   Stdlib.( := ) end_pos__012_ (Stdlib.Option.Some fvalue__019_)
                 | Stdlib.Option.Some _ ->
                   Stdlib.( := )
                     duplicates__014_
                     (field_name__016_ :: Stdlib.( ! ) duplicates__014_))
              | _ ->
                if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
                then
                  Stdlib.( := ) extra__015_ (field_name__016_ :: Stdlib.( ! ) extra__015_)
                else ());
             iter__025_ tail__026_
           | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__002_) :: _ ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected
               error_source__024_
               sexp__002_
           | [] -> ()
         in
         iter__025_ field_sexps__003_;
         (match Stdlib.( ! ) duplicates__014_ with
          | _ :: _ ->
            Sexplib0.Sexp_conv_error.record_duplicate_fields
              error_source__024_
              (Stdlib.( ! ) duplicates__014_)
              sexp__002_
          | [] ->
            (match Stdlib.( ! ) extra__015_ with
             | _ :: _ ->
               Sexplib0.Sexp_conv_error.record_extra_fields
                 error_source__024_
                 (Stdlib.( ! ) extra__015_)
                 sexp__002_
             | [] ->
               (match
                  ( Stdlib.( ! ) filename__004_
                  , Stdlib.( ! ) line_number__006_
                  , Stdlib.( ! ) line_start__008_
                  , Stdlib.( ! ) start_pos__010_
                  , Stdlib.( ! ) end_pos__012_ )
                with
                | ( Stdlib.Option.Some filename__005_
                  , Stdlib.Option.Some line_number__007_
                  , Stdlib.Option.Some line_start__009_
                  , Stdlib.Option.Some start_pos__011_
                  , Stdlib.Option.Some end_pos__013_ ) ->
                  { filename = filename__005_
                  ; line_number = line_number__007_
                  ; line_start = line_start__009_
                  ; start_pos = start_pos__011_
                  ; end_pos = end_pos__013_
                  }
                | _ ->
                  Sexplib0.Sexp_conv_error.record_undefined_elements
                    error_source__024_
                    sexp__002_
                    [ ( Sexplib0.Sexp_conv.( = )
                          (Stdlib.( ! ) filename__004_)
                          Stdlib.Option.None
                      , "filename" )
                    ; ( Sexplib0.Sexp_conv.( = )
                          (Stdlib.( ! ) line_number__006_)
                          Stdlib.Option.None
                      , "line_number" )
                    ; ( Sexplib0.Sexp_conv.( = )
                          (Stdlib.( ! ) line_start__008_)
                          Stdlib.Option.None
                      , "line_start" )
                    ; ( Sexplib0.Sexp_conv.( = )
                          (Stdlib.( ! ) start_pos__010_)
                          Stdlib.Option.None
                      , "start_pos" )
                    ; ( Sexplib0.Sexp_conv.( = )
                          (Stdlib.( ! ) end_pos__012_)
                          Stdlib.Option.None
                      , "end_pos" )
                    ])))
       | Sexplib0.Sexp.Atom _ as sexp__002_ ->
         Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__024_ sexp__002_
         : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { filename = filename__029_
           ; line_number = line_number__031_
           ; line_start = line_start__033_
           ; start_pos = start_pos__035_
           ; end_pos = end_pos__037_
           } ->
        let bnds__028_ = [] in
        let bnds__028_ =
          let arg__038_ = sexp_of_int end_pos__037_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "end_pos"; arg__038_ ] :: bnds__028_
        in
        let bnds__028_ =
          let arg__036_ = sexp_of_int start_pos__035_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "start_pos"; arg__036_ ] :: bnds__028_
        in
        let bnds__028_ =
          let arg__034_ = sexp_of_int line_start__033_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_start"; arg__034_ ] :: bnds__028_
        in
        let bnds__028_ =
          let arg__032_ = sexp_of_int line_number__031_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_number"; arg__032_ ]
          :: bnds__028_
        in
        let bnds__028_ =
          let arg__030_ = Name.sexp_of_t filename__029_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "filename"; arg__030_ ] :: bnds__028_
        in
        Sexplib0.Sexp.List bnds__028_
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    let compare =
      (fun a__039_ b__040_ ->
         if Ppx_compare_lib.phys_equal a__039_ b__040_
         then 0
         else (
           match Name.compare a__039_.filename b__040_.filename with
           | 0 ->
             (match compare_int a__039_.line_number b__040_.line_number with
              | 0 ->
                (match compare_int a__039_.line_start b__040_.line_start with
                 | 0 ->
                   (match compare_int a__039_.start_pos b__040_.start_pos with
                    | 0 -> compare_int a__039_.end_pos b__040_.end_pos
                    | n -> n)
                 | n -> n)
              | n -> n)
           | n -> n)
           : t -> t -> int)
    ;;

    let _ = compare

    [@@@end]
  end

  include T
  include Comparable.Make (T)

  let beginning_of_file filename =
    { filename; line_number = 1; line_start = 0; start_pos = 0; end_pos = 0 }
  ;;

  let of_source_code_position (pos : Source_code_position.t) =
    { filename = Name.of_string (Caml.Filename.basename pos.pos_fname)
    ; line_number = pos.pos_lnum
    ; line_start = pos.pos_bol
    ; start_pos = pos.pos_cnum
    ; end_pos = pos.pos_cnum
    }
  ;;
end

module Digest : sig
  type t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : t) -> ()
  let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t
  let compare = (compare_string : t -> t -> int)
  let _ = compare

  [@@@end]

  let to_string t = t

  let of_string s =
    let expected_length = 32 in
    if String.length s <> expected_length
    then invalid_arg "Expect_test_collector.File.Digest.of_string, unexpected length";
    for i = 0 to expected_length - 1 do
      match s.[i] with
      | '0' .. '9' | 'a' .. 'f' -> ()
      | _ -> invalid_arg "Expect_test_collector.File.Digest.of_string"
    done;
    s
  ;;
end
