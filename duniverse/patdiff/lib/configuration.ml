open Core
module P = Patdiff_core

let warn_if_no_trailing_newline_in_both_default = true

type t =
  { output : P.Output.t
  ; rules : P.Format.Rules.t
  ; ext_cmp : string option
  ; float_tolerance : Percent.t option
  ; produce_unified_lines : bool
  ; unrefined : bool
  ; keep_ws : bool
  ; split_long_lines : bool
  ; interleave : bool
  ; assume_text : bool
  ; context : int
  ; line_big_enough : int
  ; word_big_enough : int
  ; shallow : bool
  ; quiet : bool
  ; double_check : bool
  ; mask_uniques : bool
  ; prev_alt : string option
  ; next_alt : string option
  ; location_style : P.Format.Location_style.t
  ; warn_if_no_trailing_newline_in_both : bool
                                          [@default warn_if_no_trailing_newline_in_both_default] [@sexp_drop_default.equal]
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f field = Invariant.check_field t f field in
    Fields.iter
      ~output:
        (check (fun output ->
           if P.Output.implies_unrefined output
           then [%test_eq: bool] t.unrefined true ~message:"output implies unrefined"))
      ~rules:ignore
      ~ext_cmp:
        (check (fun ext_cmp ->
           Option.iter ext_cmp ~f:(fun (_ : string) ->
             [%test_eq: bool] t.unrefined true ~message:"ext_cmp implies unrefined")))
      ~float_tolerance:
        (check (fun float_tolerance ->
           if Option.is_some float_tolerance
           then
             [%test_eq: string option]
               t.ext_cmp
               None
               ~message:"ext_cmp and float_tolerance cannot both be some"))
      ~produce_unified_lines:ignore
      ~unrefined:ignore
      ~keep_ws:ignore
      ~interleave:ignore
      ~assume_text:ignore
      ~split_long_lines:ignore
      ~context:
        (check (fun context ->
           [%test_pred: int]
             Int.is_non_negative
             context
             ~message:"context cannot be negative"))
      ~line_big_enough:
        (check (fun line_big_enough ->
           [%test_pred: int]
             Int.is_positive
             line_big_enough
             ~message:"line_big_enough must be positive"))
      ~word_big_enough:
        (check (fun word_big_enough ->
           [%test_pred: int]
             Int.is_positive
             word_big_enough
             ~message:"word_big_enough must be positive"))
      ~shallow:ignore
      ~quiet:ignore
      ~double_check:ignore
      ~mask_uniques:ignore
      ~prev_alt:ignore
      ~next_alt:ignore
      ~location_style:ignore
      ~warn_if_no_trailing_newline_in_both:ignore)
;;

let override
      ?output
      ?rules
      ?ext_cmp
      ?float_tolerance
      ?produce_unified_lines
      ?unrefined
      ?keep_ws
      ?split_long_lines
      ?interleave
      ?assume_text
      ?context
      ?line_big_enough
      ?word_big_enough
      ?shallow
      ?quiet
      ?double_check
      ?mask_uniques
      ?prev_alt
      ?next_alt
      ?location_style
      ?warn_if_no_trailing_newline_in_both
      t
  =
  let output = Option.value ~default:t.output output in
  let ext_cmp = Option.value ~default:t.ext_cmp ext_cmp in
  let unrefined =
    Option.value ~default:t.unrefined unrefined
    || is_some ext_cmp
    || P.Output.implies_unrefined output
  in
  let t =
    let value value field = Option.value value ~default:(Field.get field t) in
    Fields.map
      ~output:(const output)
      ~rules:(value rules)
      ~ext_cmp:(const ext_cmp)
      ~float_tolerance:(value float_tolerance)
      ~produce_unified_lines:(value produce_unified_lines)
      ~unrefined:(const unrefined)
      ~keep_ws:(value keep_ws)
      ~interleave:(value interleave)
      ~assume_text:(value assume_text)
      ~split_long_lines:(value split_long_lines)
      ~context:(value context)
      ~line_big_enough:(value line_big_enough)
      ~word_big_enough:(value word_big_enough)
      ~shallow:(value shallow)
      ~quiet:(value quiet)
      ~double_check:(value double_check)
      ~mask_uniques:(value mask_uniques)
      ~prev_alt:(value prev_alt)
      ~next_alt:(value next_alt)
      ~location_style:(value location_style)
      ~warn_if_no_trailing_newline_in_both:(value warn_if_no_trailing_newline_in_both)
  in
  invariant t;
  t
;;

module Style = P.Format.Style

module Affix = struct
  (* Simply a prefix or a suffix.  Might come with an associated style. *)

  type t =
    { text : string option [@sexp.option]
    ; style : Style.t list option [@sexp.option]
    }
  [@@deriving sexp]

  let blank = { text = None; style = None }
  let get_text t = Option.value t.text ~default:""

  let length t_opt =
    let f t = String.length (get_text t) in
    Option.value_map t_opt ~default:0 ~f
  ;;

  let to_internal t ~min_width =
    let text = sprintf "%*s" min_width (get_text t) in
    let styles = Option.value ~default:[] t.style in
    P.Format.Rule.Affix.create ~styles text
  ;;
end

module Rule = struct
  type t =
    { prefix : Affix.t option [@sexp.option]
    ; suffix : Affix.t option [@sexp.option]
    ; style : Style.t list option [@sexp.option]
    }
  [@@deriving sexp]

  let blank = { prefix = None; suffix = None; style = None }

  let to_internal t =
    let f = Affix.to_internal ~min_width:0 in
    let default = P.Format.Rule.Affix.blank in
    let affix opt = Option.value_map ~default ~f opt in
    let pre = affix t.prefix in
    let suf = affix t.suffix in
    let style = Option.value ~default:[] t.style in
    P.Format.Rule.create ~pre ~suf style
  ;;
end

module Hunk = struct
  type t = Rule.t [@@deriving sexp]

  let to_internal t =
    let get_affix a = Option.value a ~default:Affix.blank in
    let prefix = get_affix t.Rule.prefix in
    let suffix = get_affix t.Rule.suffix in
    let prefix_text = Option.value prefix.Affix.text ~default:"@@ " in
    let suffix_text = Option.value suffix.Affix.text ~default:" @@" in
    let t =
      { t with
        Rule.prefix = Some { prefix with Affix.text = Some prefix_text }
      ; Rule.suffix = Some { suffix with Affix.text = Some suffix_text }
      }
    in
    Rule.to_internal t
  ;;
end

module Header = struct
  type t = Rule.t [@@deriving sexp]

  let to_internal t ~default =
    let get_affix a = Option.value a ~default:Affix.blank in
    let prefix = get_affix t.Rule.prefix in
    let prefix_text = Option.value prefix.Affix.text ~default in
    let t =
      { t with Rule.prefix = Some { prefix with Affix.text = Some prefix_text } }
    in
    Rule.to_internal t
  ;;
end

module Line_rule = struct
  type t =
    { prefix : Affix.t option [@sexp.option]
    ; suffix : Affix.t option [@sexp.option]
    ; style : Style.t list option [@sexp.option]
    ; word_same : Style.t list option [@sexp.option]
    }
  [@@deriving sexp]

  let default = { prefix = None; suffix = None; style = None; word_same = None }
end

module Output = struct
  type t = P.Output.t [@@deriving sexp]
end

module Config = struct
  module V1 = struct
    type t =
      { dont_produce_unified_lines : bool option [@sexp.option]
      ; dont_overwrite_word_old_word_new : bool option [@sexp.option]
      ; config_path : string option [@sexp.option]
      ; context : int option [@sexp.option]
      ; line_big_enough : int option [@sexp.option]
      ; word_big_enough : int option [@sexp.option]
      ; unrefined : bool option [@sexp.option]
      ; keep_whitespace : bool option [@sexp.option]
      ; split_long_lines : bool option [@sexp.option]
      ; interleave : bool option [@sexp.option]
      ; assume_text : bool option [@sexp.option]
      ; quiet : bool option [@sexp.option]
      ; shallow : bool option [@sexp.option]
      ; double_check : bool option [@sexp.option]
      ; mask_uniques : bool option [@sexp.option]
      ; html : bool option [@sexp.option]
      ; alt_old : string option [@sexp.option]
      ; alt_new : string option [@sexp.option]
      ; ext_cmp : string option [@sexp.option]
      ; float_tolerance : Percent.t option [@sexp.option]
      ; header_old : Header.t option [@sexp.option]
      ; header_new : Header.t option [@sexp.option]
      ; hunk : Hunk.t option [@sexp.option]
      ; line_same : Line_rule.t option [@sexp.option]
      ; line_old : Line_rule.t option [@sexp.option]
      ; line_new : Line_rule.t option [@sexp.option]
      ; line_unified : Line_rule.t option [@sexp.option]
      ; word_old : Rule.t option [@sexp.option]
      ; word_new : Rule.t option [@sexp.option]
      ; location_style : P.Format.Location_style.t
                         [@default P.Format.Location_style.Diff] [@sexp_drop_default.equal]
      ; warn_if_no_trailing_newline_in_both : bool
                                              [@default warn_if_no_trailing_newline_in_both_default]
                                              [@sexp_drop_default.equal]
      }
    [@@deriving sexp]
  end

  module V0 = struct
    module Line_changed = struct
      type t =
        { prefix_old : Affix.t
        ; prefix_new : Affix.t
        }
      [@@deriving sexp]
    end

    module Word_same = struct
      type t =
        { style_old : Style.t list
        ; style_new : Style.t list
        }
      [@@deriving sexp]
    end

    module Word_changed = struct
      type t =
        { style_old : Style.t list
        ; style_new : Style.t list
        ; prefix_old : Affix.t option [@sexp.option]
        ; suffix_old : Affix.t option [@sexp.option]
        ; prefix_new : Affix.t option [@sexp.option]
        ; suffix_new : Affix.t option [@sexp.option]
        }
      [@@deriving sexp]
    end

    module Old_header = struct
      type t =
        { style_old : Style.t list option [@sexp.option]
        ; style_new : Style.t list option [@sexp.option]
        ; prefix_old : Affix.t option [@sexp.option]
        ; suffix_old : Affix.t option [@sexp.option]
        ; prefix_new : Affix.t option [@sexp.option]
        ; suffix_new : Affix.t option [@sexp.option]
        }
      [@@deriving sexp]
    end

    type t =
      { config_path : string option [@sexp.option]
      ; context : int option [@sexp.option]
      ; line_big_enough : int option [@sexp.option]
      ; word_big_enough : int option [@sexp.option]
      ; unrefined : bool option [@sexp.option]
      ; external_compare : string option [@sexp.option]
      ; float_tolerance : Percent.t option [@sexp.option]
      ; keep_whitespace : bool option [@sexp.option]
      ; split_long_lines : bool option [@sexp.option]
      ; interleave : bool option [@sexp.option]
      ; assume_text : bool option [@sexp.option]
      ; shallow : bool option [@sexp.option]
      ; quiet : bool option [@sexp.option]
      ; double_check : bool option [@sexp.option]
      ; hide_uniques : bool option [@sexp.option]
      ; header : Old_header.t option [@sexp.option]
      ; line_same : Style.t list option [@sexp.option]
      ; line_same_prefix : Affix.t option [@sexp.option]
      ; line_changed : Line_changed.t option [@sexp.option]
      ; word_same : Word_same.t option [@sexp.option]
      ; word_changed : Word_changed.t option [@sexp.option]
      ; chunk : Hunk.t option [@sexp.option]
      ; location_style : P.Format.Location_style.t
                         [@default P.Format.Location_style.Diff] [@sexp_drop_default.equal]
      ; warn_if_no_trailing_newline_in_both : bool
                                              [@default warn_if_no_trailing_newline_in_both_default]
                                              [@sexp_drop_default.equal]
      }
    [@@deriving sexp]

    let to_v1 t =
      { V1.config_path = t.config_path
      ; context = t.context
      ; line_big_enough = t.line_big_enough
      ; word_big_enough = t.word_big_enough
      ; unrefined = t.unrefined
      ; dont_produce_unified_lines = None
      ; dont_overwrite_word_old_word_new = None
      ; keep_whitespace = t.keep_whitespace
      ; interleave = t.interleave
      ; assume_text = t.assume_text
      ; split_long_lines = t.split_long_lines
      ; quiet = t.quiet
      ; shallow = t.shallow
      ; double_check = t.double_check
      ; mask_uniques = t.hide_uniques
      ; html = None
      ; alt_old = None
      ; alt_new = None
      ; ext_cmp = t.external_compare
      ; float_tolerance = t.float_tolerance
      ; header_old =
          Option.map t.header ~f:(fun header ->
            { Rule.style = header.Old_header.style_old
            ; prefix = header.Old_header.prefix_old
            ; suffix = header.Old_header.suffix_old
            })
      ; header_new =
          Option.map t.header ~f:(fun header ->
            { Rule.style = header.Old_header.style_new
            ; prefix = header.Old_header.prefix_new
            ; suffix = header.Old_header.suffix_new
            })
      ; hunk = t.chunk
      ; line_same =
          Some
            { Line_rule.default with
              Line_rule.style = t.line_same
            ; prefix = t.line_same_prefix
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_old)
            }
      ; line_old =
          Option.map t.line_changed ~f:(fun line_changed ->
            { Line_rule.default with
              Line_rule.style =
                Option.map t.word_changed ~f:(fun word_changed ->
                  word_changed.Word_changed.style_old)
            ; Line_rule.prefix = Some line_changed.Line_changed.prefix_old
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_old)
            })
      ; line_new =
          Option.map t.line_changed ~f:(fun line_changed ->
            { Line_rule.default with
              Line_rule.style =
                Option.map t.word_changed ~f:(fun word_changed ->
                  word_changed.Word_changed.style_new)
            ; Line_rule.prefix = Some line_changed.Line_changed.prefix_new
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_new)
            })
      ; line_unified = None
      ; word_old = None
      ; word_new = None
      ; location_style = t.location_style
      ; warn_if_no_trailing_newline_in_both = t.warn_if_no_trailing_newline_in_both
      }
    ;;
  end

  type t = V1.t [@@deriving sexp_of]

  let t_of_sexp sexp =
    match V1.t_of_sexp sexp with
    | v1 -> v1
    | exception as_v1_exn ->
      (match V0.t_of_sexp sexp with
       | v0 -> V0.to_v1 v0
       | exception as_v0_exn ->
         raise_s
           [%message
             "Patdiff_lib.Configuration.Config.t_of_sexp: invalid config"
               (as_v1_exn : exn)
               (as_v0_exn : exn)])
  ;;
end

let parse (config : Config.t) =
  let c = config in
  let value = Option.value in
  (**** Command Line Arguments ****)
  let context = value ~default:(-1) c.context in
  let line_big_enough = value ~default:3 c.line_big_enough in
  let word_big_enough = value ~default:7 c.word_big_enough in
  let unrefined = value ~default:false c.unrefined in
  let produce_unified_lines = not (value ~default:false c.dont_produce_unified_lines) in
  let overwrite_word_old_word_new =
    not (value ~default:false c.dont_overwrite_word_old_word_new)
  in
  let keep_ws = value ~default:false c.keep_whitespace in
  let interleave = value ~default:true c.interleave in
  let assume_text = value ~default:false c.assume_text in
  let split_long_lines = value ~default:false c.split_long_lines in
  let shallow = value ~default:false c.shallow in
  let quiet = value ~default:false c.quiet in
  let double_check = value ~default:false c.double_check in
  let mask_uniques = value ~default:false c.mask_uniques in
  let ext_cmp = c.ext_cmp in
  let float_tolerance = c.float_tolerance in
  let alt_prev = c.alt_old in
  let alt_next = c.alt_new in
  let location_style = c.location_style in
  (**** Output Type ****)
  let output =
    let html = Option.value c.html ~default:false in
    if html then P.Output.Html else P.Output.Ansi
  in
  (**** Styling Rules ****)
  (* Words *)
  let create_word_same line_opt =
    let line = value line_opt ~default:Line_rule.default in
    P.Format.Rule.create (value ~default:[] line.word_same)
  in
  let word_same_prev = create_word_same c.line_old in
  let word_same_next = create_word_same c.line_new in
  let word_same_unified = create_word_same c.line_unified in
  (* Lines *)
  let default = Line_rule.default in
  let line_same = value ~default c.line_same in
  let line_prev = value ~default c.line_old in
  let line_next = value ~default c.line_new in
  let line_unified = value ~default c.line_unified in
  (* Words *)
  let create_word ~(line_rule : Line_rule.t) opt =
    if overwrite_word_old_word_new
    then Rule.to_internal { Rule.blank with style = line_rule.style }
    else Rule.to_internal (value opt ~default:Rule.blank)
  in
  let word_prev = create_word ~line_rule:line_prev c.word_old in
  let word_next = create_word ~line_rule:line_next c.word_new in
  (* Padding for prefixes: They should all be the same length. *)
  let len = Affix.length in
  let same_len = len line_same.prefix in
  let prev_len = len line_prev.prefix in
  let next_len = len line_next.prefix in
  let uni_len = len line_unified.prefix in
  let min_width = max (max (max prev_len next_len) same_len) uni_len in
  let pad a = Affix.to_internal ~min_width (value ~default:Affix.blank a) in
  let same_prefix = pad line_same.prefix in
  let prev_prefix = pad line_prev.prefix in
  let next_prefix = pad line_next.prefix in
  let unified_prefix = pad line_unified.prefix in
  (* Construct the rules *)
  let create style ~pre = P.Format.Rule.create (value ~default:[] style) ~pre in
  let line_same = create line_same.style ~pre:same_prefix in
  let line_prev = create line_prev.style ~pre:prev_prefix in
  let line_next = create line_next.style ~pre:next_prefix in
  let line_unified = create line_unified.style ~pre:unified_prefix in
  (* Header *)
  let get_header h_opt = value h_opt ~default:Rule.blank in
  let create h_opt default = Header.to_internal (get_header h_opt) ~default in
  let header_prev = create c.header_old "---" in
  let header_next = create c.header_new "+++" in
  (* Chunks *)
  let hunk = Hunk.to_internal (Option.value c.hunk ~default:Rule.blank) in
  (* Final *)
  let rules =
    { P.Format.Rules.line_same
    ; line_prev
    ; line_next
    ; line_unified
    ; word_same_prev
    ; word_same_next
    ; word_same_unified
    ; word_prev
    ; word_next
    ; hunk
    ; header_prev
    ; header_next
    }
  in
  { rules
  ; output
  ; context
  ; word_big_enough
  ; line_big_enough
  ; unrefined
  ; produce_unified_lines
  ; ext_cmp
  ; float_tolerance
  ; keep_ws
  ; split_long_lines
  ; interleave
  ; assume_text
  ; shallow
  ; quiet
  ; double_check
  ; mask_uniques
  ; prev_alt = alt_prev
  ; next_alt = alt_next
  ; location_style
  ; warn_if_no_trailing_newline_in_both = c.warn_if_no_trailing_newline_in_both
  }
;;

let dark_bg =
  lazy
    (let sexp =
       (* this sexp is copied from /mnt/global/dev/etc/shared/patdiff-dark-bg *)
       Sexp.of_string
         {|
((context 8)
 (line_same ())
 (line_changed
  ((prefix_old ((text "-|") (style (Bold (Fg Red)))))
   (prefix_new ((text "+|") (style (Bold (Fg Green)))))))
 (word_same ((style_old ())
             (style_new ())))
 (word_changed ((style_old (Bold Underline (Fg Red)))
                (style_new ((Fg Green)))))
 (chunk
  ((prefix ((text "@@@@@@@@@@ ") (style (Bold (Fg blue)))))
   (suffix ((text " @@@@@@@@@@") (style (Bold (Fg blue)))))
   (style (Bold (Fg blue)))))
 )|}
     in
     parse (Config.V0.t_of_sexp sexp |> Config.V0.to_v1))
;;

let light_bg =
  lazy
    (let sexp =
       (* this sexp is copied from /mnt/global/dev/etc/shared/patdiff-light-bg *)
       Sexp.of_string
         {|
((context 8)
 (line_same (dim))
 (line_changed ((prefix_old ((text "-|") (style (bold (fg red)))))
                (prefix_new ((text "+|") (style (bold (fg green)))))))
 (word_same ((style_old ((bg white)))
             (style_new ((bg yellow)))))
 (word_changed ((style_old ((bg white) bold))
                (style_new ((bg yellow) bold))))
 )|}
     in
     parse (Config.V0.t_of_sexp sexp |> Config.V0.to_v1))
;;

let%test_module _ =
  (module struct
    (* Ensure both sexps are parseable *)
    let%test_unit _ =
      let dark = Lazy.force dark_bg in
      let light = Lazy.force light_bg in
      ignore (dark : t);
      ignore (light : t)
    ;;
  end)
;;

let load_sexp_conv f conv = Result.try_with (fun () -> Sexp.load_sexp_conv_exn f conv)

let rec load_exn' ~set config_file =
  let config =
    match load_sexp_conv config_file Config.V1.t_of_sexp with
    | Ok c -> c
    | Error exn ->
      let as_old_config =
        Result.map (load_sexp_conv config_file Config.V0.t_of_sexp) ~f:Config.V0.to_v1
      in
      (match as_old_config with
       | Error _another_exn -> raise exn
       | Ok c ->
         (let new_file = config_file ^ ".new" in
          match Sys.file_exists new_file with
          | `Yes | `Unknown -> ()
          | `No ->
            (try Sexp.save_hum new_file (Config.V1.sexp_of_t c) with
             | _ -> ()));
         c)
  in
  match config.config_path with
  | Some config_path ->
    if Set.mem set config_path
    then failwith "Cycle detected! file redirects to itself"
    else load_exn' ~set:(Set.add set config_path) config_path
  | None -> parse config
;;

let load_exn config_file = load_exn' ~set:String.Set.empty config_file

(* prints errors to stderr *)
let load ?(quiet_errors = false) config_file =
  try Some (load_exn config_file) with
  | e ->
    if not quiet_errors
    then eprintf "Note: error loading %S: %s\n%!" config_file (Exn.to_string e);
    None
;;

let default_string =
  sprintf
    {|;; -*- scheme -*-
;; patdiff Configuration file

(
 (context %d)

 (line_same
  ((prefix ((text " |") (style ((bg bright_black) (fg black)))))))

 (line_old
  ((prefix ((text "-|") (style ((bg red)(fg black)))))
   (style ((fg red)))
   (word_same (dim))))

 (line_new
  ((prefix ((text "+|") (style ((bg green)(fg black)))))
   (style ((fg green)))))

 (line_unified
  ((prefix ((text "!|") (style ((bg yellow)(fg black)))))))

 (header_old
  ((prefix ((text "------ ") (style ((fg red)))))
   (style (bold))))

 (header_new
  ((prefix ((text "++++++ ") (style ((fg green)))))
   (style (bold))))


 (hunk
  ((prefix ((text "@|") (style ((bg bright_black) (fg black)))))
   (suffix ((text " ============================================================") (style ())))
   (style (bold))))
)|}
    Patdiff_core.default_context
;;

let default = lazy (parse ([%of_sexp: Config.t] (Sexp.of_string default_string)))

let get_config ?filename () =
  (* Load config file if it exists, use default if not *)
  let file =
    match filename with
    | Some "" -> None
    | Some f -> Some f (* specified file *)
    | None ->
      (* ~/.patdiff exists *)
      Option.bind (Sys.getenv "HOME") ~f:(fun home ->
        let f = home ^/ ".patdiff" in
        match Sys.file_exists f with
        | `Yes -> Some f
        | `No | `Unknown -> None)
  in
  (* load prints warnings to stderr. This is desired because [file] is only Some if it
     was manually specified or if ~/.patdiff exists. The user should be notified of
     errors if the file fails in both cases. *)
  match Option.bind file ~f:load with
  | Some c -> c
  | None -> force default
;;

let save_default ~filename = Out_channel.write_all filename ~data:default_string

include struct
  let%test_unit "default config parses" = ignore (get_config ~filename:"" () : t)
end
