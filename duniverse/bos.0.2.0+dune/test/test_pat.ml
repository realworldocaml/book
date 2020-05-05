(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring
open Bos

let eqp = eq ~eq:Pat.equal ~pp:Pat.pp
let v = Fpath.v

let string_conv = test "Pat.{v,of_string,to_string}" @@ fun () ->
  let trip p = eq_str p Pat.(to_string (v p)) in
  app_invalid ~pp:Pat.pp Pat.v "$(";
  app_invalid ~pp:Pat.pp Pat.v "$(a";
  app_invalid ~pp:Pat.pp Pat.v "$$$(";
  app_invalid ~pp:Pat.pp Pat.v "$$$";
  app_invalid ~pp:Pat.pp Pat.v "$(bla,)";
  app_invalid ~pp:Pat.pp Pat.v "$(b,la)";
  trip "Hey $(ho)";
  trip "Hey $(ho) $(hu)";
  trip "Hey $(ho) $(h$u)";
  trip "Hey mo $$(hu)";
  trip "Hey mo $$30";
  trip "Hey mo $$$$";
  ()

let dom = test "Pat.dom" @@ fun () ->
  let eq s l =
    eq ~eq:String.Set.equal ~pp:String.Set.dump
      (Pat.(dom @@ v s)) (String.Set.of_list l)
  in
  eq "bla" [];
  eq "bla ha $$" [];
  eq "hey $(bla)" ["bla"];
  eq "hey $(bla) $()" ["bla"; ""];
  eq "hey $(bla) $$(ha) $()" ["bla"; ""];
  eq "hey $(bla) $(bli) $()" ["bla"; "bli"; ""];
  ()

let subst = test "Pat.subst" @@ fun () ->
  let eq ?undef defs p s =
    eq_str Pat.(to_string @@ subst ?undef defs (v p)) s
  in
  let defs = String.Map.of_list ["bli", "bla"] in
  let undef = function "blu" -> Some "bla$" | _ -> None in
  eq ~undef defs "hey $$ $(bli) $(bla) $(blu)" "hey $$ bla $(bla) bla$$";
  eq defs "hey $(blo) $(bla) $(blu)" "hey $(blo) $(bla) $(blu)";
  ()

let format = test "Pat.format" @@ fun () ->
  let eq ?undef defs p s = eq_str (Pat.(format ?undef defs (v p))) s in
  let defs = String.Map.of_list ["hey", "ho"; "hi", "ha$"] in
  let undef = fun _ -> "undef" in
  eq ~undef defs "a $$ $(hu)" "a $ undef";
  eq ~undef defs "a $(hey) $(hi)" "a ho ha$";
  eq defs "a $$(hey) $$(hi) $(ha)" "a $(hey) $(hi) ";
  ()

let matches = test "Pat.matches" @@ fun () ->
  let m p s = Pat.(matches (v p) s) in
  eq_bool (m "$(mod).mli" "string.mli") true;
  eq_bool (m "$(mod).mli" "string.mli ") false;
  eq_bool (m "$(mod).mli" ".mli") true;
  eq_bool (m "$(mod).mli" ".mli ") false;
  eq_bool (m "$(mod).$(suff)" "string.mli") true;
  eq_bool (m "$(mod).$(suff)" "string.mli ") true;
  eq_bool (m "$()aaa" "aaa") true;
  eq_bool (m "aaa$()" "aaa") true;
  eq_bool (m "$()a$()aa$()" "aaa") true;
  ()

let query = test "Pat.query" @@ fun () ->
  let u ?init p s = Pat.(query ?init (v p) s) in
  let eq = eq_option
      ~eq:(String.Map.equal String.equal) ~pp:(String.Map.dump String.dump)
  in
  let eq ?init p s = function
  | None -> eq (u ?init p s) None
  | Some l -> eq (u ?init p s) (Some (String.Map.of_list l))
  in
  let init = String.Map.of_list ["hey", "ho"] in
  eq "$(mod).mli" "string.mli" (Some ["mod", "string"]);
  eq ~init "$(mod).mli" "string.mli" (Some ["mod", "string"; "hey", "ho"]);
  eq "$(mod).mli" "string.mli " None;
  eq ~init "$(mod).mli" "string.mli " None;
  eq "$(mod).mli" "string.mli " None;
  eq "$(mod).$(suff)" "string.mli" (Some ["mod", "string"; "suff", "mli"]);
  eq "$(mod).$(suff)" "string.mli" (Some ["mod", "string"; "suff", "mli"]);
  eq "$(m).$(m)" "string.mli" (Some ["m", "string"]);
  ()

let suite = suite "Pat module"
    [ string_conv;
      dom;
      subst;
      format;
      matches;
      query; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
