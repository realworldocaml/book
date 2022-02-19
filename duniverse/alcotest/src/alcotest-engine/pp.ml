(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019      Craig Ferguson    <craig@tarides.com>
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
 *)

include Pp_intf
include Pp_intf.Types
open! Import
open Model

let map_theta t ~f ppf = f (fun ppf () -> t ppf) ppf ()
let pp_plural ppf x = Fmt.pf ppf (if x < 2 then "" else "s")

let colour_of_tag = function
  | `Ok -> `Green
  | `Fail -> `Red
  | `Skip | `Todo | `Assert -> `Yellow

let string_of_tag = function
  | `Ok -> "OK"
  | `Fail -> "FAIL"
  | `Skip -> "SKIP"
  | `Todo -> "TODO"
  | `Assert -> "ASSERT"

let pp_tag ~wrapped ppf typ =
  let colour = colour_of_tag typ in
  let tag = string_of_tag typ in
  let tag = if wrapped then "[" ^ tag ^ "]" else tag in
  Fmt.(styled colour string) ppf tag

let tag = pp_tag ~wrapped:false

module Make (P : sig
  val stdout_columns : unit -> int option
end) =
struct
  include Types

  let terminal_width =
    lazy (match P.stdout_columns () with Some w -> w | None -> 80)

  let rresult_error ppf = function
    | `Error (_, e) -> Fmt.pf ppf "%a@," e ()
    | `Exn (_, n, e) -> Fmt.pf ppf "[%s] @[<v>%a@]" n e ()
    | `Ok | `Todo _ | `Skip -> ()

  (* Colours *)
  let color c ppf fmt = Fmt.(styled c string) ppf fmt
  let red_s fmt = color `Red fmt
  let red ppf fmt = Fmt.kstr (fun str -> red_s ppf str) fmt
  let green_s fmt = color `Green fmt
  let yellow_s fmt = color `Yellow fmt
  let left_gutter = 2
  let left_tag = 14
  let left_total = left_gutter + left_tag

  let left nb pp ppf a =
    let s = Fmt.to_to_string pp a in
    let nb = nb - String.length_utf8 s in
    if nb <= 0 then pp ppf a
    else (
      pp ppf a;
      Fmt.string ppf (String.v ~len:nb (fun _ -> ' ')))

  let pp_test_name ~max_label ppf tname =
    let name_len = Test_name.length tname in
    let index = Test_name.index tname in
    let padding =
      match max_label + 8 - name_len with
      | n when n <= 0 -> ""
      | n -> String.v ~len:n (fun _ -> ' ')
    in
    Fmt.pf ppf "%a%s%3d" Fmt.(styled `Cyan Test_name.pp) tname padding index

  let info ?(available_width = Lazy.force terminal_width) ~max_label
      ~doc_of_test_name ppf tname =
    let pp_test_name ppf = Fmt.pf ppf "%a   " (pp_test_name ~max_label) tname in
    let test_doc =
      let test_doc = doc_of_test_name tname in
      let available_width =
        pp_test_name Format.str_formatter;
        let used_width = String.length_utf8 (Format.flush_str_formatter ()) in
        available_width - used_width
      in
      if String.length_utf8 test_doc <= available_width then test_doc
      else String.prefix_utf8 (available_width - 3) test_doc ^ "..."
    in
    Fmt.pf ppf "%t%s" pp_test_name test_doc

  let tag_of_result = function
    | `Ok -> `Ok
    | `Exn _ | `Error _ -> `Fail
    | `Skip -> `Skip
    | `Todo _ -> `Todo

  let pp_result ppf result =
    let tag = tag_of_result result in
    left left_tag (pp_tag ~wrapped:true) ppf tag

  let pp_result_compact ppf result =
    let colour = result |> tag_of_result |> colour_of_tag in
    let char =
      match result with
      | `Ok -> '.'
      | `Exn _ | `Error _ -> 'F'
      | `Skip -> 'S'
      | `Todo _ -> 'T'
    in
    Fmt.(styled colour char) ppf char

  let left_padding ~with_selector =
    let open Fmt in
    (if with_selector then const (styled `Bold (styled `Red char)) '>'
    else const char ' ')
    ++ const char ' '

  let pp_result_full ~max_label ~doc_of_test_name ~selector_on_failure ppf
      (path, result) =
    let with_selector = selector_on_failure && Run_result.is_failure result in
    (left_padding ~with_selector) ppf ();
    pp_result ppf result;
    let available_width = Lazy.force terminal_width - left_total in
    (info ~available_width ~max_label ~doc_of_test_name) ppf path;
    ()

  let event_line ~margins ~max_label ~doc_of_test_name ppf = function
    | `Result (p, r) ->
        pp_result ppf r;
        (info
           ~available_width:(Lazy.force terminal_width - margins - left_total)
           ~max_label ~doc_of_test_name)
          ppf p
    | _ -> assert false

  let event ~isatty ~compact ~max_label ~doc_of_test_name ~selector_on_failure
      ~tests_so_far ppf event =
    match (compact, isatty, event) with
    | true, _, `Start _ | _, false, `Start _ -> ()
    | false, true, `Start tname ->
        Fmt.(
          left_padding ~with_selector:false
          ++ const (left left_tag yellow_s) "..."
          ++ const
               (info
                  ~available_width:(Lazy.force terminal_width - left_total)
                  ~max_label ~doc_of_test_name)
               tname)
          ppf ()
    | true, _, `Result (_, r) ->
        pp_result_compact ppf r;
        (* Wrap compact output to terminal width manually *)
        if (tests_so_far + 1) mod Lazy.force terminal_width = 0 then
          Format.pp_force_newline ppf ();
        ()
    | false, _, `Result (tname, r) ->
        if isatty then Fmt.pf ppf "\r";
        Fmt.pf ppf "%a@,"
          (pp_result_full ~max_label ~doc_of_test_name ~selector_on_failure)
          (tname, r)

  let pp_suite_errors ~show_all = function
    | [] -> Fmt.nop
    | x :: _ as xs -> (if show_all then xs else [ x ]) |> Fmt.concat

  let quoted f = Fmt.(const char '`' ++ f ++ const char '\'')

  let with_surrounding_box (type a) (f : a Fmt.t) : a Fmt.t =
   fun ppf a ->
    (* Peek at the value being pretty-printed to determine the length of the box
       we're going to need. Fortunately, this will not include ANSII colour
       escapes. *)
    let true_width = Fmt.kstr String.length_utf8 "| %a |" f a in
    let min_width = Lazy.force terminal_width in
    let width = max min_width true_width in

    let right_padding = String.v ~len:(width - true_width) (fun _ -> ' ') in
    let s = Fmt.(const (styled `Faint string)) in
    let bars = List.init (width - 2) (fun _ -> "─") |> String.concat in
    let top = s ("┌" ^ bars ^ "┐")
    and mid = Fmt.(s "│ " ++ f ++ s (right_padding ^ " │"))
    and bottom = s ("└" ^ bars ^ "┘") in
    Fmt.(top ++ cut ++ mid ++ cut ++ bottom ++ cut) ppf a

  let horizontal_rule (type a) ppf (_ : a) =
    let open Fmt in
    (const string " "
    ++ const
         (styled `Faint string)
         (List.init (Lazy.force terminal_width - 2) (fun _ -> "─")
         |> String.concat)
    ++ cut)
      ppf ()

  let pp_full_logs ppf log_dir =
    Fmt.pf ppf "Full test results in %t.@,"
      (map_theta ~f:Fmt.(styled `Cyan >> quoted) log_dir)

  let pp_summary ppf r =
    let pp_failures ppf = function
      | 0 -> green_s ppf "Test Successful"
      | n -> red ppf "%d failure%a!" n pp_plural n
    in
    Fmt.pf ppf "%a in %.3fs. %d test%a run.@," pp_failures r.failures r.time
      r.success pp_plural r.success

  let suite_results ~log_dir cfg ppf r =
    let print_summary = (not cfg#compact) || r.failures > 0 in
    match cfg#json with
    | true ->
        (* Return the json for the api, dirty out, to avoid new dependencies *)
        Fmt.pf ppf {|{
  "success": %i,
  "failures": %i,
  "time": %f
}
|}
          r.success r.failures r.time
    | false ->
        Format.pp_force_newline ppf ();
        Format.pp_open_vbox ppf 0;
        if cfg#compact then Fmt.cut ppf ();
        (pp_suite_errors ~show_all:(cfg#verbose || cfg#show_errors) r.errors)
          ppf ();
        if print_summary then (
          if not cfg#verbose then pp_full_logs ppf log_dir;
          pp_summary ppf r);
        Format.pp_close_box ppf ()

  let user_error msg =
    Fmt.epr "%a: %s\n" Fmt.(styled `Red string) "ERROR" msg;
    exit 1
end
