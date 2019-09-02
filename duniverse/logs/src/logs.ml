(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf

let pp_print_text ppf s =
  (* hint spaces and new lines with Format's funs *)
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ()) else
    incr right
  done;
  if !left <> len then flush ()

(* Reporting levels *)

type level = App | Error | Warning | Info | Debug
let _level = ref (Some Warning)
let level () = !_level
let pp_level ppf = function
| App -> ()
| Error -> Format.pp_print_string ppf "ERROR"
| Warning -> Format.pp_print_string ppf "WARNING"
| Info -> Format.pp_print_string ppf "INFO"
| Debug -> Format.pp_print_string ppf "DEBUG"

let level_to_string = function
| None -> "quiet" | Some App -> "app" | Some Error -> "error"
| Some Warning -> "warning" | Some Info -> "info" | Some Debug -> "debug"

let level_of_string = function
| "quiet" -> Ok None
| "app" -> Ok (Some App)
| "error" -> Ok (Some Error)
| "warning" -> Ok (Some Warning)
| "info" -> Ok (Some Info)
| "debug" -> Ok (Some Debug)
| l -> Error (`Msg (strf "%S: unknown log level" l))

(* Sources *)

module Src = struct
  type t =
    { uid : int;
      name : string;
      doc : string;
      mutable level : level option }

  let uid =
    let id = ref (-1) in
    fun () -> incr id; !id

  let list = ref []

  let create ?(doc = "undocumented") name =
    let src = { uid = uid (); name; doc; level = !_level } in
    list := src :: !list;
    src

  let name s = s.name
  let doc s = s.doc
  let level s = s.level
  let set_level s l = s.level <- l
  let equal src0 src1 = src0.uid = src1.uid
  let compare src0 src1 = (compare : int -> int -> int) src0.uid src1.uid

  let pp ppf src = Format.fprintf ppf
      "@[<1>(src@ @[<1>(name %S)@]@ @[<1>(uid %d)@] @[<1>(doc %S)@])@]"
      src.name src.uid src.doc

  let list () = !list
end

type src = Src.t

let default = Src.create "application" ~doc:"The application log"

let set_level ?(all = true) l =
  _level := l; if all then List.iter (fun s -> Src.set_level s l) (Src.list ())

(* Message tags *)

module Tag = struct

  (* Universal type, see http://mlton.org/UniversalType *)

  type univ = exn
  let univ (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  (* Tag definitions *)

  type 'a def =
    { uid : int;
      to_univ : 'a -> univ;
      of_univ : univ -> 'a option;
      name : string;
      doc : string;
      pp : Format.formatter -> 'a -> unit; }

  type def_e = Def : 'a def -> def_e

  let list = ref ([] : def_e list)
  let uid =
    let id = ref (-1) in
    fun () -> incr id; !id

  let def ?(doc = "undocumented") name pp =
    let to_univ, of_univ = univ () in
    { uid = uid (); to_univ; of_univ; name; doc; pp }

  let name d = d.name
  let doc d = d.doc
  let printer d = d.pp
  let pp_def ppf d = Format.fprintf ppf "tag:%s" d.name
  let list () = !list

  (* Tag values *)

  type t = V : 'a def * 'a -> t

  let pp ppf (V (d, v)) =
    Format.fprintf ppf "@[<1>(%a@ @[%a@])@]" pp_def d d.pp v

  (* Tag sets *)

  module Key = struct
    type t = V : 'a def -> t
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
  end

  module M = Map.Make (Key)

  type set = t M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem k s = M.mem (Key.V k) s
  let add k v s = M.add (Key.V k) (V (k, v)) s
  let rem k s = M.remove (Key.V k) s
  let find : type a. a def -> set -> a option =
  fun k s ->
    try match M.find (Key.V k) s with
    | V (k', v) -> k.of_univ (k'.to_univ v)
    with Not_found -> None

  let get k s = match find k s with
  | None -> invalid_arg (strf "tag named %s not found in set" k.name)
  | Some v -> v

  let fold f s acc = M.fold (fun _ t acc -> f t acc) s acc
  let pp_set ppf s =
    let pp_tag tag is_first =
      if is_first then () else Format.fprintf ppf "@,";
      Format.fprintf ppf "%a" pp tag;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_tag s true);
    Format.fprintf ppf "}@]";
    ()
end

(* Reporters *)

type ('a, 'b) msgf =
  (?header:string -> ?tags:Tag.set ->
   ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type reporter_mutex = { lock : unit -> unit; unlock : unit -> unit }
let _reporter_mutex = ref { lock = (fun () -> ()); unlock = (fun () -> ()) }
let set_reporter_mutex ~lock ~unlock = _reporter_mutex := { lock; unlock }

type reporter =
  { report :
      'a 'b. src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
      ('a, 'b) msgf -> 'b }

let nop_reporter = { report = fun _ _ ~over k _ -> over (); k () }
let _reporter = ref nop_reporter
let set_reporter r = _reporter := r
let reporter () = !_reporter
let report src level ~over k msgf =
  let over () = over (); !_reporter_mutex.unlock () in
  !_reporter_mutex.lock ();
  !_reporter.report src level ~over k msgf

let pp_header ppf (l, h) = match h with
| None -> if l = App then () else Format.fprintf ppf "[%a]" pp_level l
| Some h -> Format.fprintf ppf "[%s]" h

let pp_exec_header =
  let x = match Array.length Sys.argv with
  | 0 -> Filename.basename Sys.executable_name
  | n -> Filename.basename Sys.argv.(0)
  in
  let pf = Format.fprintf in
  let pp_header ppf (l, h) =
    if l = App then (match h with None -> () | Some h -> pf ppf "[%s] " h) else
    match h with
    | None -> pf ppf "%s: [%a] " x pp_level l
    | Some h -> pf ppf "%s: [%s] " x h
  in
  pp_header

let format_reporter
    ?(pp_header = pp_exec_header)
    ?(app = Format.std_formatter)
    ?(dst = Format.err_formatter) ()
  =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    let ppf = if level = App then app else dst in
    Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, header)
  in
  { report }

(* Log functions *)

let _err_count = ref 0
let err_count () = !_err_count
let incr_err_count () = incr _err_count

let _warn_count = ref 0
let warn_count () = !_warn_count
let incr_warn_count () = incr _warn_count

type 'a log = ('a, unit) msgf -> unit

let over () = ()
let kmsg : type a b. (unit -> b) -> ?src:src -> level -> (a, b) msgf -> b =
fun k ?(src = default) level msgf ->
match Src.level src with
| None -> k ()
| Some level' when level > level' ->
    (if level = Error then incr _err_count else
     if level = Warning then incr _warn_count else ());
    (k ())
| Some _ ->
    (if level = Error then incr _err_count else
     if level = Warning then incr _warn_count else ());
    report src level ~over k msgf

let kunit _ = ()
let msg ?src level msgf = kmsg kunit ?src level msgf
let app ?src msgf = kmsg kunit ?src App msgf
let err ?src msgf = kmsg kunit ?src Error msgf
let warn ?src msgf = kmsg kunit ?src Warning msgf
let info ?src msgf = kmsg kunit ?src Info msgf
let debug ?src msgf = kmsg kunit ?src Debug msgf

(* Log result errors *)

let on_error ?src ?(level = Error) ?header ?tags ~pp ~use = function
| Ok v -> v
| Error e ->
    kmsg (fun () -> use e) ?src level @@ fun m ->
    m ?header ?tags "@[%a@]" pp e

let on_error_msg ?src ?(level = Error) ?header ?tags ~use = function
| Ok v -> v
| Error (`Msg msg) ->
    kmsg use ?src level @@ fun m ->
    m ?header ?tags "@[%a@]" pp_print_text msg

(* Source specific logging functions *)

module type LOG = sig
  val msg : level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log
  val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
  val on_error :
    ?level:level -> ?header:string -> ?tags:Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result ->
    'a

  val on_error_msg :
    ?level:level -> ?header:string -> ?tags:Tag.set ->
    use:(unit -> 'a) -> ('a, [`Msg of string]) result -> 'a
end

let src_log src =
  let module Log = struct
    let msg level msgf = msg ~src level msgf
    let kmsg k level msgf = kmsg k ~src level msgf
    let app msgf = msg App msgf
    let err msgf = msg Error msgf
    let warn msgf = msg Warning msgf
    let info msgf = msg Info msgf
    let debug msgf = msg Debug msgf
    let on_error ?level ?header ?tags ~pp ~use =
      on_error ~src ?level ?header ?tags ~pp ~use

    let on_error_msg ?level ?header ?tags ~use =
      on_error_msg ~src ?level ?header ?tags ~use
  end
  in
  (module Log : LOG)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers

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
