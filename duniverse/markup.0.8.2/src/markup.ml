(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)



module type IO =
sig
  type 'a t

  val return : 'a -> 'a t
  val of_cps : ((exn -> unit) -> ('a -> unit) -> unit) -> 'a t
  val to_cps : (unit -> 'a t) -> ((exn -> unit) -> ('a -> unit) -> unit)
end

module Synchronous : IO with type 'a t = 'a =
struct
  type 'a t = 'a

  exception Not_synchronous

  let return x = x

  let of_cps f =
    let result = ref None in
    f raise (fun v -> result := Some v);
    match !result with
    | None -> raise Not_synchronous
    | Some v -> v

  (* Used in to_cps to avoid the need for a match .. with | exception ..
     expression, which would break compatibility with OCaml < 4.02. Flambda
     seems to optimizes the allocation of these results away completely. There
     is a small performance penalty when not using Flambda. *)
  type 'a result = Value of 'a | Exn of exn

  let to_cps f =
    fun throw k ->
      let result =
        try Value (f ())
        with exn -> Exn exn
      in
      match result with
      | Value v -> k v
      | Exn exn -> throw exn
end



type async = unit
type sync = unit

type ('data, 'sync) stream = 'data Kstream.t

let kstream s = s
let of_kstream s = s

let of_list = Kstream.of_list



type location = Common.location
let compare_locations = Common.compare_locations

module Error = Error



type name = Common.name

type xml_declaration = Common.xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}

type doctype = Common.doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}

type signal = Common.signal
type content_signal = Common.content_signal

let signal_to_string = Common.signal_to_string

type 's parser =
  {mutable location : location;
   mutable signals  : (signal, 's) stream}

let signals parser = parser.signals
let location parser = parser.location

let stream_to_parser s =
  let parser = {location = (1, 1); signals = Kstream.empty ()} in
  parser.signals <-
    s |> Kstream.map (fun (l, v) _ k -> parser.location <- l; k v);
  parser

module Cps =
struct
  let parse_xml
      report ?encoding namespace entity context source =
    let with_encoding (encoding : Encoding.t) k =
      source
      |> encoding ~report
      |> Input.preprocess Common.is_valid_xml_char report
      |> Xml_tokenizer.tokenize report entity
      |> Xml_parser.parse context namespace report
      |> k
    in

    let constructor throw k =
      match encoding with
      | Some encoding -> with_encoding encoding k
      | None ->
        Detect.select_xml source throw (fun encoding ->
        with_encoding encoding k)
    in

    Kstream.construct constructor
    |> stream_to_parser

  let write_xml report prefix signals =
    signals
    |> Xml_writer.write report prefix
    |> Utility.strings_to_bytes

  let parse_html report ?encoding context source =
    let with_encoding (encoding : Encoding.t) k =
      source
      |> encoding ~report
      |> Input.preprocess Common.is_valid_html_char report
      |> Html_tokenizer.tokenize report
      |> Html_parser.parse context report
      |> k
    in

    let constructor throw k =
      match encoding with
      | Some encoding -> with_encoding encoding k
      | None ->
        Detect.select_html source throw (fun encoding ->
        with_encoding encoding k)
    in

    Kstream.construct constructor
    |> stream_to_parser

  let write_html ?escape_attribute ?escape_text signals =
    signals
    |> Html_writer.write ?escape_attribute ?escape_text
    |> Utility.strings_to_bytes
end



let string = Stream_io.string
let buffer = Stream_io.buffer
let channel = Stream_io.channel
let file = Stream_io.file

let to_channel c bytes = Stream_io.to_channel c bytes |> Synchronous.of_cps
let to_file f bytes = Stream_io.to_file f bytes |> Synchronous.of_cps



let preprocess_input_stream source =
  Input.preprocess (fun _ -> true) Error.ignore_errors source



include Utility



module Ns =
struct
  let html = Common.html_ns
  let svg = Common.svg_ns
  let mathml = Common.mathml_ns
  let xml = Common.xml_ns
  let xmlns = Common.xmlns_ns
  let xlink = Common.xlink_ns
end



module type ASYNCHRONOUS =
sig
  type 'a io

  module Encoding :
  sig
    type t = Encoding.t

    val decode :
      ?report:(location -> Error.t -> unit io) -> t ->
      (char, _) stream -> (int, async) stream
  end

  val parse_xml :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, _) stream -> async parser

  val write_xml :
    ?report:((signal * int) -> Error.t -> unit io) ->
    ?prefix:(string -> string option) ->
    ([< signal ], _) stream -> (char, async) stream

  val parse_html :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?context:[< `Document | `Fragment of string ] ->
    (char, _) stream -> async parser

  val write_html :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  ([< signal ], _) stream -> (char, async) stream

  val fn : (unit -> char option io) -> (char, async) stream

  val to_string : (char, _) stream -> string io
  val to_buffer : (char, _) stream -> Buffer.t io

  val stream : (unit -> 'a option io) -> ('a, async) stream

  val next : ('a, _) stream -> 'a option io
  val peek : ('a, _) stream -> 'a option io

  val transform :
    ('a -> 'b -> ('c list * 'a option) io) -> 'a -> ('b, _) stream ->
      ('c, async) stream
  val fold : ('a -> 'b -> 'a io) -> 'a -> ('b, _) stream -> 'a io
  val map : ('a -> 'b io) -> ('a, _) stream -> ('b, async) stream
  val filter : ('a -> bool io) -> ('a, _) stream -> ('a, async) stream
  val filter_map : ('a -> 'b option io) -> ('a, _) stream -> ('b, async) stream
  val iter : ('a -> unit io) -> ('a, _) stream -> unit io
  val drain : ('a, _) stream -> unit io

  val to_list : ('a, _) stream -> 'a list io

  val load : ('a, _) stream -> ('a, sync) stream io

  val tree :
    ?text:(string list -> 'a) ->
    ?element:(name -> (name * string) list -> 'a list -> 'a) ->
    ?comment:(string -> 'a) ->
    ?pi:(string -> string -> 'a) ->
    ?xml:(xml_declaration -> 'a) ->
    ?doctype:(doctype -> 'a) ->
    ([< signal ], _) stream -> 'a option io
end

module Asynchronous (IO : IO) =
struct
  let wrap_report report = fun l e -> IO.to_cps (fun () -> report l e)

  module Encoding =
  struct
    include Encoding

    let decode ?(report = fun _ _ -> IO.return ()) (f : Encoding.t) s =
      f ~report:(wrap_report report) s
  end

  let parse_xml
      ?(report = fun _ _ -> IO.return ())
      ?encoding
      ?(namespace = fun _ -> None)
      ?(entity = fun _ -> None)
      ?context
      source =

    Cps.parse_xml
      (wrap_report report) ?encoding namespace entity context source

  let write_xml
      ?(report = fun _ _ -> IO.return ())
      ?(prefix = fun _ -> None)
      signals =

    Cps.write_xml (wrap_report report) prefix signals

  let parse_html
      ?(report = fun _ _ -> IO.return ())
      ?encoding
      ?context
      source =

    Cps.parse_html (wrap_report report) ?encoding context source

  let write_html ?escape_attribute ?escape_text signals =
    Cps.write_html ?escape_attribute ?escape_text signals

  let to_string bytes = Stream_io.to_string bytes |> IO.of_cps
  let to_buffer bytes = Stream_io.to_buffer bytes |> IO.of_cps

  let stream f =
    let f = IO.to_cps f in
    (fun throw e k ->
      f throw (function
        | None -> e ()
        | Some v -> k v))
    |> Kstream.make

  let fn = stream

  let next s = Kstream.next_option s |> IO.of_cps
  let peek s = Kstream.peek_option s |> IO.of_cps

  (* Without Flambda, thunks are repeatedly created and passed on IO.to_cps,
     resulting in a performance penalty. Flambda seems to optimize this away,
     however. *)

  let transform f v s =
    Kstream.transform (fun v s -> IO.to_cps (fun () -> f v s)) v s

  let fold f v s =
    Kstream.fold (fun v v' -> IO.to_cps (fun () -> f v v')) v s |> IO.of_cps

  let map f s = Kstream.map (fun v -> IO.to_cps (fun () -> f v)) s

  let filter f s = Kstream.filter (fun v -> IO.to_cps (fun () -> f v)) s

  let filter_map f s = Kstream.filter_map (fun v -> IO.to_cps (fun () -> f v)) s

  let iter f s =
    Kstream.iter (fun v -> IO.to_cps (fun () -> f v)) s |> IO.of_cps

  let drain s = iter (fun _ -> IO.return ()) s

  let to_list s = Kstream.to_list s |> IO.of_cps

  let load s =
    (fun throw k -> Kstream.to_list s throw (fun l -> k (Kstream.of_list l)))
    |> IO.of_cps

  let tree ?text ?element ?comment ?pi ?xml ?doctype s =
    Utility.tree ?text ?element ?comment ?pi ?xml ?doctype s |> IO.of_cps
end

include Asynchronous (Synchronous)
