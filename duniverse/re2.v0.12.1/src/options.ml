open! Core_kernel

module Encoding = struct

  type t =
    | Latin1
    | Utf8
  [@@deriving compare, equal, sexp_of]

  module C_repr = struct
    type t = int [@@deriving compare, sexp_of]
    let equal = Int.(=) (* would use [@@deriving equal], but equal_int is not in scope *)

    external get_latin1 : unit -> int = "mlre2__options__encoding__get_latin1" [@@noalloc]
    external get_utf8 : unit -> int = "mlre2__options__encoding__get_utf8" [@@noalloc]

    let utf8 = get_utf8 ()
    let latin1 = get_latin1 ()

  end

  let to_c_repr = function
    | Latin1 -> C_repr.latin1
    | Utf8 -> C_repr.utf8
  ;;

  let of_c_repr c_repr =
    if C_repr.equal c_repr C_repr.utf8
    then Utf8
    else if C_repr.equal c_repr C_repr.latin1
    then Latin1
    else raise_s [%message "Unexpected Encoding.C_repr" ~_:(c_repr : C_repr.t)]
  ;;

end

type t =
  { case_sensitive : bool
  ; dot_nl         : bool
  ; encoding       : Encoding.t
  ; literal        : bool
  ; log_errors     : bool
  ; longest_match  : bool
  ; max_mem        : int
  ; never_capture  : bool
  ; never_nl       : bool
  ; one_line       : bool
  ; perl_classes   : bool
  ; posix_syntax   : bool
  ; word_boundary  : bool
  }
[@@deriving compare, fields, sexp_of]

module C_repr = struct
  type t

  (*$ #use "options.cinaps";;
    List.iter all ~f:(fun { name; type_ = { ocaml_type; _} } ->
    printf "\n  external %s : t -> %s = \"mlre2__options__%s\" [@@noalloc]\n"
    name ocaml_type name;
    printf "  external set_%s : t -> %s -> unit = \"mlre2__options__set_%s\" [@@noalloc]\n"
    name ocaml_type name);

    printf "  "
  *)
  external case_sensitive : t -> bool = "mlre2__options__case_sensitive" [@@noalloc]
  external set_case_sensitive : t -> bool -> unit = "mlre2__options__set_case_sensitive" [@@noalloc]

  external dot_nl : t -> bool = "mlre2__options__dot_nl" [@@noalloc]
  external set_dot_nl : t -> bool -> unit = "mlre2__options__set_dot_nl" [@@noalloc]

  external encoding : t -> Encoding.C_repr.t = "mlre2__options__encoding" [@@noalloc]
  external set_encoding : t -> Encoding.C_repr.t -> unit = "mlre2__options__set_encoding" [@@noalloc]

  external literal : t -> bool = "mlre2__options__literal" [@@noalloc]
  external set_literal : t -> bool -> unit = "mlre2__options__set_literal" [@@noalloc]

  external log_errors : t -> bool = "mlre2__options__log_errors" [@@noalloc]
  external set_log_errors : t -> bool -> unit = "mlre2__options__set_log_errors" [@@noalloc]

  external longest_match : t -> bool = "mlre2__options__longest_match" [@@noalloc]
  external set_longest_match : t -> bool -> unit = "mlre2__options__set_longest_match" [@@noalloc]

  external max_mem : t -> int = "mlre2__options__max_mem" [@@noalloc]
  external set_max_mem : t -> int -> unit = "mlre2__options__set_max_mem" [@@noalloc]

  external never_capture : t -> bool = "mlre2__options__never_capture" [@@noalloc]
  external set_never_capture : t -> bool -> unit = "mlre2__options__set_never_capture" [@@noalloc]

  external never_nl : t -> bool = "mlre2__options__never_nl" [@@noalloc]
  external set_never_nl : t -> bool -> unit = "mlre2__options__set_never_nl" [@@noalloc]

  external one_line : t -> bool = "mlre2__options__one_line" [@@noalloc]
  external set_one_line : t -> bool -> unit = "mlre2__options__set_one_line" [@@noalloc]

  external perl_classes : t -> bool = "mlre2__options__perl_classes" [@@noalloc]
  external set_perl_classes : t -> bool -> unit = "mlre2__options__set_perl_classes" [@@noalloc]

  external posix_syntax : t -> bool = "mlre2__options__posix_syntax" [@@noalloc]
  external set_posix_syntax : t -> bool -> unit = "mlre2__options__set_posix_syntax" [@@noalloc]

  external word_boundary : t -> bool = "mlre2__options__word_boundary" [@@noalloc]
  external set_word_boundary : t -> bool -> unit = "mlre2__options__set_word_boundary" [@@noalloc]
  (*$*)

  external create_quiet : unit -> t = "mlre2__options__create_quiet"

end

let to_c_repr t =
  let c_repr = C_repr.create_quiet () in
  let f set _field _t value = set c_repr value in
  Fields.Direct.iter t
    (*$ List.iter all ~f:(fun { name; type_ = _} ->
      if String.equal "encoding" name
      then printf "\n\
      ~encoding:(f (fun c_repr value -> \
      C_repr.set_encoding c_repr (Encoding.to_c_repr value)))"
      else printf "\n\
      ~%s:(f C_repr.set_%s)" name name) *)
    ~case_sensitive:(f C_repr.set_case_sensitive)
    ~dot_nl:(f C_repr.set_dot_nl)
    ~encoding:(f (fun c_repr value -> C_repr.set_encoding c_repr (Encoding.to_c_repr value)))
    ~literal:(f C_repr.set_literal)
    ~log_errors:(f C_repr.set_log_errors)
    ~longest_match:(f C_repr.set_longest_match)
    ~max_mem:(f C_repr.set_max_mem)
    ~never_capture:(f C_repr.set_never_capture)
    ~never_nl:(f C_repr.set_never_nl)
    ~one_line:(f C_repr.set_one_line)
    ~perl_classes:(f C_repr.set_perl_classes)
    ~posix_syntax:(f C_repr.set_posix_syntax)
    ~word_boundary:(f C_repr.set_word_boundary)(*$*);
  c_repr
;;

let of_c_repr =
  let f get _field () = get, () in
  Fields.make_creator
    (*$ List.iter all ~f:(fun {name; type_ = _} ->
      if String.equal "encoding" name
      then printf "\n\
      ~encoding:(f (fun c_repr -> Encoding.of_c_repr (C_repr.encoding c_repr)))"
      else printf "\n\
      ~%s:(f C_repr.%s)" name name) *)
    ~case_sensitive:(f C_repr.case_sensitive)
    ~dot_nl:(f C_repr.dot_nl)
    ~encoding:(f (fun c_repr -> Encoding.of_c_repr (C_repr.encoding c_repr)))
    ~literal:(f C_repr.literal)
    ~log_errors:(f C_repr.log_errors)
    ~longest_match:(f C_repr.longest_match)
    ~max_mem:(f C_repr.max_mem)
    ~never_capture:(f C_repr.never_capture)
    ~never_nl:(f C_repr.never_nl)
    ~one_line:(f C_repr.one_line)
    ~perl_classes:(f C_repr.perl_classes)
    ~posix_syntax:(f C_repr.posix_syntax)
    ~word_boundary:(f C_repr.word_boundary)(*$*)
    ()
  |> fst
;;

let default = C_repr.create_quiet () |> of_c_repr

let latin1 = { default with encoding = Latin1 }

let noisy = { default with log_errors = true }

let posix = { default with longest_match = true; posix_syntax = true }

module Private = struct
  module C_repr = C_repr

  let of_c_repr = of_c_repr
  let to_c_repr = to_c_repr
end
