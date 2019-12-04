open Base
open Ppxlib

module Directive = struct
  type t = If | Else | Elif | Endif | Ifdef | Ifndef |
           Define | Undef |
           Error | Warning | Import |
           (* deprecated, but provide useful warnings *)
           Elifdef | Elifndef

  let matches ~expected matched =
      String.(=) expected matched ||
      String.(=) ("optcomp." ^ expected) matched

  (* not using [matches] here because I'm pretty sure the pattern matching
     compiler will make this faster than string equality. *)
  let of_string_opt s =
    match s with
    | "optcomp.if"       | "if"       -> Some If
    | "optcomp.else"     | "else"     -> Some Else
    | "optcomp.elif"     | "elif"     -> Some Elif
    | "optcomp.endif"    | "endif"    -> Some Endif
    | "optcomp.ifdef"    | "ifdef"    -> Some Ifdef
    | "optcomp.ifndef"   | "ifndef"   -> Some Ifndef
    | "optcomp.define"   | "define"   -> Some Define
    | "optcomp.undef"    | "undef"    -> Some Undef
    | "optcomp.error"    | "error"    -> Some Error
    | "optcomp.warning"  | "warning"  -> Some Warning
    | "optcomp.import"   | "import"   -> Some Import
    | "optcomp.elifdef"  | "elifdef"  -> Some Elifdef
    | "optcomp.elifndef" | "elifndef" -> Some Elifndef
    | _ -> None
end

type 'a t =
  | Block of 'a list (** blocks with no optcomp extensions in it *)
  | Directive of Directive.t * location * payload

let make_directive name loc payload = match Directive.of_string_opt name with
  | Some dir -> Directive (dir, loc, payload)
  | None -> Location.raise_errorf ~loc "optcomp: unknown directive"

let just_directives_exn ~loc ls = List.filter_map ls ~f:(fun token ->
  match token with
  | Directive _ as dir -> Some dir
  | Block [] -> None
  | Block _ -> Location.raise_errorf ~loc
                 "optcomp: only optcomp directives allowed in this context"
)
