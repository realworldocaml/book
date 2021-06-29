(* Copyright (c) 2017 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
*)

type t = { major: int; minor: int; patch: int option; prerelease:string option; extra: string option }
let v ?patch ?prerelease ?extra major minor =
  { major; minor; patch; prerelease; extra }

let major { major; _ } = major
let minor { minor; _ } = minor
let patch { patch; _ } = patch
let extra { extra; _ } = extra
let prerelease { prerelease; _ } = prerelease

let choose_seps ~prerelease_sep ~sep = match prerelease_sep, sep with
  | None, None -> "~", "+"
  | Some x, Some y -> String.make 1 x, String.make 1 y
  | Some x, None -> String.make 1 x, "+"
  | None, Some x -> String.make 2 x, String.make 1 x

let with_sep ~sep = function
  | None -> ""
  | Some x -> sep ^ x

let to_string ?prerelease_sep ?sep v =
  let presep, sep = choose_seps ~prerelease_sep ~sep in
  let prerelease = with_sep ~sep:presep v.prerelease in
  let extra = with_sep ~sep v.extra in
  match v.patch with
  | None ->
    Printf.sprintf "%d.%02d%s%s" v.major v.minor prerelease extra
  | Some patch ->
    Printf.sprintf "%d.%02d.%d%s%s" v.major v.minor patch prerelease extra

let parse s =
  let build patch major minor sep extra =
    match sep, extra with
    | "~", extra ->
      begin match String.index_opt extra '+' with
        | None -> v ?patch ~prerelease:extra major minor
        | Some r ->
          let prerelease = String.sub extra 0 r in
          let after_plus = r + 1 in
          let extra = String.sub extra after_plus (String.length extra - after_plus) in
          v ?patch ~prerelease ~extra major minor
      end
    | "+", extra -> v ?patch ~extra:extra major minor
    | "", "" -> v ?patch major minor
    | _ -> raise (Scanf.Scan_failure ("invalid ocaml version: "^ s))
  in
  try Scanf.sscanf s "%d.%d.%d%1[+~]%s" (fun major minor patch -> build (Some patch) major minor)
  with End_of_file | Scanf.Scan_failure _ ->
    Scanf.sscanf s "%d.%d%1[+~]%s" (build None)

let of_string s =
  try Ok (parse s) with
  | _ -> Error (`Msg (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let of_string_exn s =
  try parse s with
  | _ -> raise (Invalid_argument (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let pp ppf v = Format.pp_print_string ppf (to_string v)

let ( ++ ) x fn =
  match x with
  | 0 -> fn ()
  | r -> r

let equal {major; minor; patch; prerelease; extra} a =
  (major : int) = a.major &&
  (minor : int) = a.minor &&
  (patch : int option) = a.patch &&
  (prerelease: string option) = a.prerelease &&
  (extra : string option) = a.extra

let compare_prerelease (x:string option) (y:string option) = match x, y with
    | Some x, Some y -> compare x y
  (* reversed order None > Some _  *)
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1

let compare {major; minor; patch; prerelease; extra} a =
  compare major a.major ++ fun () ->
    compare minor a.minor ++ fun () ->
      compare patch a.patch ++ fun () ->
        compare_prerelease prerelease a.prerelease ++ fun () ->
          compare extra a.extra

let sys_version = of_string_exn Sys.ocaml_version

let with_variant t extra = { t with extra }
let without_variant t = { t with extra=None }
let with_patch t patch = { t with patch }
let without_patch t = { t with patch=None }
let with_just_major_and_minor t = { t with patch=None; extra=None }

module Releases = struct
  let v4_00_0 = of_string_exn "4.00.0"
  let v4_00_1 = of_string_exn "4.00.1"
  let v4_00 = v4_00_1

  let v4_01_0 = of_string_exn "4.01.0"
  let v4_01 = v4_01_0

  let v4_02_0 = of_string_exn "4.02.0"
  let v4_02_1 = of_string_exn "4.02.1"
  let v4_02_2 = of_string_exn "4.02.2"
  let v4_02_3 = of_string_exn "4.02.3"
  let v4_02 = v4_02_3

  let v4_03_0 = of_string_exn "4.03.0"
  let v4_03 = v4_03_0

  let v4_04_0 = of_string_exn "4.04.0"
  let v4_04_1 = of_string_exn "4.04.1"
  let v4_04_2 = of_string_exn "4.04.2"
  let v4_04 = v4_04_2

  let v4_05_0 = of_string_exn "4.05.0"
  let v4_05 = v4_05_0

  let v4_06_0 = of_string_exn "4.06.0"
  let v4_06_1 = of_string_exn "4.06.1"
  let v4_06 = v4_06_1

  let v4_07_0 = of_string_exn "4.07.0"
  let v4_07_1 = of_string_exn "4.07.1"
  let v4_07 = v4_07_1

  let v4_08_0 = of_string_exn "4.08.0"
  let v4_08_1 = of_string_exn "4.08.1"
  let v4_08 = v4_08_1

  let v4_09_0 = of_string_exn "4.09.0"
  let v4_09_1 = of_string_exn "4.09.1"
  let v4_09 = v4_09_1

  let v4_10_0 = of_string_exn "4.10.0"
  let v4_10_1 = of_string_exn "4.10.1"
  let v4_10_2 = of_string_exn "4.10.2"
  let v4_10 = v4_10_2

  let v4_11_0 = of_string_exn "4.11.0"
  let v4_11_1 = of_string_exn "4.11.1"
  let v4_11_2 = of_string_exn "4.11.2"
  let v4_11 = v4_11_2

  let v4_12_0 = of_string_exn "4.12.0"
  let v4_12 = v4_12_0

  let v4_13_0 = of_string_exn "4.13.0"
  let v4_13 = v4_13_0

  let all_patches = [
    v4_00_1; v4_01_0; v4_02_0; v4_02_1; v4_02_2;
    v4_02_3; v4_03_0; v4_04_0; v4_04_1; v4_04_2;
    v4_05_0; v4_06_0; v4_06_1; v4_07_0; v4_07_1;
    v4_08_0; v4_08_1; v4_09_0; v4_09_1; v4_10_0;
    v4_10_1; v4_10_2; v4_11_0; v4_11_1; v4_11_2;
    v4_12_0; v4_13_0 ]

  let all = [ v4_00; v4_01; v4_02; v4_03; v4_04;
              v4_05; v4_06; v4_07; v4_08; v4_09;
              v4_10; v4_11; v4_12; v4_13 ]

  let unreleased_betas = []

  let recent = [ v4_02; v4_03; v4_04; v4_05; v4_06; v4_07; v4_08; v4_09; v4_10; v4_11; v4_12 ]

  let latest = v4_12

  let dev = [ v4_13 ]

  let trunk =
    match dev with
    | [] -> latest
    | [v] -> v
    | _ -> List.hd @@ List.sort (fun x y -> -(compare x y)) dev

  let is_dev t =
    let t = with_just_major_and_minor t in
    let dev = List.map with_just_major_and_minor dev in
    List.mem t dev

  let recent_with_dev = List.concat [recent; dev]
end

type arch = [ `I386 | `X86_64 | `Aarch64 | `Ppc64le | `Aarch32 ]
let arches = [ `I386; `X86_64; `Aarch64; `Ppc64le; `Aarch32 ]

let arch_is_32bit = function `I386 | `Aarch32 -> true |_ -> false

let string_of_arch = function
  | `Aarch64 -> "arm64"
  | `Aarch32 -> "arm32v7"
  | `X86_64 -> "amd64"
  | `Ppc64le -> "ppc64le"
  | `I386 -> "i386"

let arch_of_string = function
  | "arm64" | "aarch64" -> Ok `Aarch64
  | "amd64" | "x86_64" -> Ok `X86_64
  | "i386"  | "i686" | "686" | "386" -> Ok `I386
  | "arm32" | "arm32v7" | "aarch32" -> Ok `Aarch32
  | "ppc64le" -> Ok `Ppc64le
  | arch -> Error (`Msg ("Unknown architecture " ^ arch))

let arch_of_string_exn a =
  match arch_of_string a with
  | Ok a -> a
  | Error (`Msg m) -> raise (Invalid_argument m)

let to_opam_arch = function
  | `I386 -> "x86_32"
  | `X86_64 -> "x86_64"
  | `Ppc64le -> "ppc64"
  | `Aarch32 -> "arm32"
  | `Aarch64 -> "arm64"

let of_opam_arch = function
  | "x86_32" -> Some `I386
  | "x86_64" -> Some `X86_64
  | "ppc64" -> Some `Ppc64le
  | "arm32" -> Some `Aarch32
  | "arm64" -> Some `Aarch64
  | _ -> None

let to_docker_arch = function
   | `I386 -> "386"
   | `X86_64 -> "amd64"
   | `Ppc64le -> "ppc64le"
   | `Aarch32 -> "arm"
   | `Aarch64 -> "arm64"

let of_docker_arch = function
  | "386" -> Some `I386
  | "amd64" -> Some `X86_64
  | "ppc64le" -> Some `Ppc64le
  | "arm" -> Some `Aarch32
  | "arm64" -> Some `Aarch64
  | _ -> None

module Since = struct
  let bytes = Releases.v4_03_0

  let arch (a:arch) =
    match a with
    | `I386 -> Releases.v4_06_0 (* TODO can be earlier *)
    | `Aarch32 -> Releases.v4_06_0
    | `Aarch64 -> Releases.v4_05_0
    | `Ppc64le -> Releases.v4_06_0
    | `X86_64 -> Releases.v4_00_0 (* TODO obviously earlier *)

  let autoconf = Releases.v4_08_0

  let options_packages = Releases.v4_12_0
end

module Has = struct

  let bytes v =
    match compare Since.bytes v with
    |(-1) | 0 -> true
    |_ -> false

  let arch (a:arch) v =
    match compare (Since.arch a) v with
    |(-1) | 0 -> true
    |_ -> false

  let autoconf v =
    match compare Since.autoconf v with
    |(-1) | 0 -> true
    |_ -> false

  let options_packages v =
    match compare Since.options_packages v with
    |(-1) | 0 -> true
    |_ -> false
end

module Configure_options = struct
  type o = [
      `Afl
    | `Default_unsafe_string
    | `Disable_flat_float_array
    | `Flambda
    | `Force_safe_string
    | `Frame_pointer
    | `No_naked_pointers ]

  let to_description (t:o) =
    match t with
    | `Afl -> "AFL (fuzzing) support"
    | `Flambda -> "flambda inlining"
    | `Default_unsafe_string -> "default to unsafe strings"
    | `Force_safe_string -> "force safe string mode"
    | `Frame_pointer -> "frame pointer"
    | `No_naked_pointers -> "forbid unboxed pointers"
    | `Disable_flat_float_array -> "disable float array unboxing"

  let to_string t =
    match t with
    | `Afl -> "afl"
    | `Flambda -> "flambda"
    | `Default_unsafe_string -> "default-unsafe-string"
    | `Force_safe_string -> "force-safe-string"
    | `Frame_pointer -> "fp"
    | `No_naked_pointers -> "nnp"
    | `Disable_flat_float_array -> "no-flat-float-array"

  let of_string = function
    | "afl" -> Some `Afl
    | "flambda" -> Some `Flambda
    | "default-unsafe-string" -> Some `Default_unsafe_string
    | "force-safe-string" -> Some `Force_safe_string
    | "fp" -> Some `Frame_pointer
    | "nnp" -> Some `No_naked_pointers
    | "no-flat-float-array" -> Some `Disable_flat_float_array
    | _ -> None

  let compare_pre_options a b =
    (* For backwards compat reasons, fp always comes first. *)
    match a,b with
    | `Frame_pointer, `Frame_pointer -> 0
    | `Frame_pointer, _ -> (-1)
    | _, `Frame_pointer -> 1
    | a, b -> Stdlib.compare a b

  let compare_post_options a b =
    (* Lexically ordered options since 4.12.0 *)
    String.compare (to_string a) (to_string b)

  let compare t =
    if Has.options_packages t then
      compare_post_options
    else
      compare_pre_options

  let equal t a b = compare t a b = 0

  let to_t t = function
    | [] -> with_variant t None
    | ol ->
      List.sort (compare t) ol |> List.map to_string |> String.concat "+" |>
      fun s -> with_variant t (Some s)

  let of_t t =
    match t.extra with None -> Ok [] | Some extra ->
    String.split_on_char '+' extra |>
    List.map (fun b -> match of_string b with
      | None -> Error (`Msg ("unknown variant: " ^ b))
      | Some v -> Ok v) |>
    List.fold_left (fun a b ->
      match a, b with
      | Ok a, Ok b -> Ok (List.sort (compare t) (b :: a))
      | _, Error b -> Error b
      | Error a, _-> Error a) (Ok [])

  let to_configure_flag t o =
    if Has.autoconf t then
      match o with
      | `Afl -> "--with-afl"
      | `Flambda -> "--enable-flambda"
      | `Default_unsafe_string -> "--enable-default-unsafe-string"
      | `Force_safe_string -> "--force-safe-string"
      | `Frame_pointer -> "--enable-frame-pointers"
      | `No_naked_pointers -> "--disable-naked-pointers"
      | `Disable_flat_float_array -> "--disable-flat-float-array"
    else
      match o with
      | `Afl -> "-afl-instrument"
      | `Flambda -> "-flambda"
      | `Default_unsafe_string -> "-default-unsafe-string"
      | `Force_safe_string -> "-force-safe-string"
      | `Frame_pointer -> "-with-frame-pointer"
      | _ -> ""
end

module Sources = struct
  let trunk = Releases.trunk

  let git_tag ({major; minor; patch; _ } as ov) =
    match major, minor, patch with
    | major, minor, _ when major = trunk.major && minor = trunk.minor -> "trunk"
    | _ -> to_string (with_variant ov None)
end

let trunk_variants (arch:arch) =
  let base = [[]; [`No_naked_pointers]; [`Afl]; [`Flambda]; [`Disable_flat_float_array]] in
  let arch_opts =
    match arch with
    |`X86_64 -> [[`Frame_pointer]; [`Frame_pointer;`Flambda]]
    |_ -> []
  in
  List.map (Configure_options.to_t Sources.trunk) (base @ arch_opts)

let compiler_variants arch ({major; minor; _} as t) =
  let variants = [] in
  let version = (major, minor) in
  if version = (Releases.trunk.major, Releases.trunk.minor) then
    trunk_variants arch
  else
    let variants =
      (* No variants for OCaml < 4.00 *)
      if version < (4, 00) then []
      else
        (* +nnp for OCaml 4.12+ *)
        let variants =
          if version >= (4, 12) then
            [`No_naked_pointers] :: variants
          else
            variants in
        (* +no-flat-float-array for OCaml 4.12+ *)
        let variants =
          if version >= (4, 12) then
            [`Disable_flat_float_array] :: variants
          else
            variants in
        (* +fp+flambda for OCaml 4.12+ on x86_64 *)
        let variants =
          if arch = `X86_64 && version >= (4, 12) then
            [`Frame_pointer;`Flambda] :: variants
          else
            variants in
      (* +fp for OCaml 4.08+ on x86_64 *)
        let variants =
          if arch = `X86_64 && version >= (4, 08) then
            [`Frame_pointer] :: variants
          else
            variants in
        (* +flambda for OCaml 4.03+ *)
        let variants =
          if version >= (4, 03) then
            [`Flambda] :: variants
          else
            variants in
        (* +afl for OCaml 4.05+ *)
        if version >= (4, 05) then
          [`Afl] :: variants
        else
          variants in
    let f = List.map (Configure_options.to_t t) in
    f ([] :: variants)

module Opam = struct

  module V2 = struct
    let package t =
      match t.extra with
      | Some extra when Releases.is_dev t ->
          let version =
            let version = to_string (without_variant t) ^ "+trunk" in
            if Has.options_packages t then
              version
            else
              Printf.sprintf "%s+%s" version extra
          in
            ("ocaml-variants", version)
      | Some _ ->
          let t =
            if Has.options_packages t then
              with_variant t (Some "options")
            else
              t
          in
            ("ocaml-variants", to_string t)
      | None when Releases.is_dev t -> ("ocaml-variants", Printf.sprintf "%s+trunk" (to_string t))
      | None -> ("ocaml-base-compiler", to_string t)

    let additional_packages t =
      if Has.options_packages t then
        match Configure_options.of_t t with
        | Ok []
        | Error _ -> []
        | Ok options ->
            let options_only_package =
              List.map Configure_options.to_string options
              |> String.concat "-"
              |> (^) "ocaml-options-only-" in
            [options_only_package]
      else
        []

    let name t =
      let (name, version) = package t in
      name ^ "." ^ version

    let variant_switch t vs =
      match vs with
      | [] -> with_variant t None
      | vs -> Configure_options.to_t t vs

    let switches arch t =
      compiler_variants arch t
  end
end
