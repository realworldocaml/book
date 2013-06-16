# Fast Binary Serialization

<note>
<title>Note to reviewers</title>

This chapter is still incomplete, but the contents here are still
instructive enough that we decided to include it in the public
beta release.

</note>

Now that we've learned the basics of working with Async, let's walk
through a small but non-trivial application: a message broker which
provides clients with a simple pub/sub API that lets them publish and
subscribe to streams of values associated with a given topic.

All of this will require a serialization format for the messages
themselves.  S-expressions, which we encountered in
[xref](#data-serialization-with-s-expressions), are a good
serialization format when you need something machine-parseable as well
as human readable and editable.  But Sexplib's s-expressions are not
particularly performant for a couple of reasons:

* s-expression serialization goes through an intermediate type,
  `Sexp.t`, which must be allocated and is then typically thrown away,
  putting non-trivial pressure on the garbage collector.
* parsing and printing to strings in an ASCII format can be expensive
  for types like `int`s, `float`s and `Time.t`s where some real
  computation needs to be done to produce or parse the ASCII
  representation.

Bin-prot is a library and syntax extension that addresses these issues
by providing efficient serialization in a compact binary format.  You
can enable Bin-prot in your top-level by typing the following:

```ocaml
# #require "bin_prot.syntax";;
```

The syntax extension is triggered on a given type by writing `with
bin_io` to the end of the type definition.  Thus, we can write:

```ocaml
# module M = struct
    type t = { number: int;
               text: string;
               variant : [`Whatever of float | `Nothing ];
             }
    with bin_io
  end;;
module M :
  sig
    type t = {
      number : int;
      text : string;
      variant : [ `Nothing | `Whatever of float ];
    }
    val bin_size_t : t -> int
    val bin_write_t_ :
      Bin_prot.Unsafe_common.sptr ->
      Bin_prot.Unsafe_common.eptr -> t -> Bin_prot.Unsafe_common.sptr
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    val bin_writer_t : t Bin_prot.Type_class.writer0
    val bin_read_t__ : 'a -> 'b -> 'c -> 'd
    val bin_read_t_ :
      Bin_prot.Unsafe_common.sptr_ptr -> Bin_prot.Unsafe_common.eptr -> t
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:int ref -> t
    val bin_reader_t : t Bin_prot.Type_class.reader0
    val bin_t : t Bin_prot.Type_class.t0
  end
```

The details of the generated values are not particularly important,
but they give you the functionality needed to serialize and
deserialize binary messages efficiently.

Clients can either publish values under a topic, or subscribe to
the stream of values associated with a given topic.  The server will
maintain a cache of the last value published under any given topic, so
that a subscriber immediately receives the most recently published
value under said topic.  To make it easier to see what's going on,
we'll also implement a query for dumping the current state of the
server.

We'll use Async's `Rpc` module for implementing that client/server
protocol.  The following module specifies the specific message types
we'll use, as well as the RPCs that will be used for communicating
with the server.

First, we'll start with the basic types.

```ocaml
(* file: protocol.ml *)
open Core.Std
open Async.Std

module Username : Identifiable = String
module Topic    : Identifiable = String

module Message = struct
  type t = { text: string;
             topic: Topic.t;
             from: Username.t;
             time: Time.t;
           }
  with sexp, bin_io
end
```

`Username.t` and `Topic.t` are just abstract types that are
implemented as strings.  The `Message.t` type contains the basic
information associated with a message, including the text of the
message, who it's from, the topic, and the time it was sent.

Note that the declaration of `Message.t` is followed by the annotation
`with sexp, bin_io`.  We've seen `with sexp` before in
[xref](#data-serialization-with-s-expressions), but `bin_io` is new.
S-expressions are a convenient serialization format, but like any
human-readable serialization format, 

Now we can move on to declaring the `Rpc` protocol we'll use.  The
`Rpc` module actually supports two different kinds of RPC protocols:
an ordinary RPC, represented by an `Rpc.Rpc.t`, is a simple
back-and-forth style of communication: the client sends a message, and
the server sends a response.  In the following, we use
`Rpc.Rpc.create` to declare the `Rpc` interface.

```ocaml
let publish_rpc = Rpc.Rpc.create
  ~name:"publish"
  ~version:0
  ~bin_query:Message.bin_t
  ~bin_response:Unit.bin_t
```

Note that we declare a name for the RPC and a version number.  The
name and the version number are used together to identify which RPC is
being sent, with the version number allowing the minting of multiple
revisions of the RPC, potentially with different types and behavior.

The argument `bin_query` and `bin_response` are used


```ocaml
let subscribe_rpc = Rpc.Pipe_rpc.create
  ~name:"subscribe"
  ~version:0
  ~bin_query:Topic.bin_t
  ~bin_response:Message.bin_t
  ~bin_error:String.bin_t

module Dump = struct
  type single = { topic : Topic.t;
                  message : Message.t;
                  num_subscribers: int; }
  with sexp,bin_io
  type t = single list with sexp,bin_io
end

let dump_rpc = Rpc.Rpc.create
  ~name:"dump"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Dump.bin_t
```
# Fast Binary Serialization with bin_prot

S-expressions are a good serialization format when you need something
machine-parseable as well as human readable and editable.  But Sexplib's
s-expressions are not particularly performant for a couple of reasons:

* s-expression serialization goes through an intermediate type,
  `Sexp.t`, which must be allocated and is then typically thrown away,
  putting non-trivial pressure on the garbage collector.
* parsing and printing to strings in an ASCII format can be expensive
  for types like `int`s, `float`s and `Time.t`s where some real
  computation needs to be done to produce or parse the ASCII
  representation.

`Bin_prot` is a library that addresses these issues by providing fast
serialization in a compact binary format.  We'll also introduce the
Core `Bigstring` library for handling large binary strings efficiently
during this chapter.

<note>
<title>Using `bin_prot` in the toplevel</title>

The `bin_prot` syntax extension isn't activated by default in the
toplevel, but is easily available if you add this to your
`~/.ocamlinit` file.  You can also just type this in directly into
`utop` (with `;;` to finish the line) instead.

```
#require "bin_prot.syntax"
```

The extension is activated by putting `with bin_io` after the type
declaration.  This looks a bit unsightly in the toplevel because of
all the definitions that are generated.  We'll elide those definitions
in the book, but you can see them for yourself in the toplevel.

</note>

## Defining a message broker

Here's a small complete example of a program that can read and write
values using `bin_io`.  Here, the serialization is of types that might
be used as part of a message-queue, where each message has a topic,
some content, and a source, which is in turn a hostname and a port.

```ocaml
open Core.Std

module Message = struct
  module Source = struct
    type t = { hostname: string;
               port: int;
             }
    with bin_io
  end

  type t = { topic: string;
             content: string;
             source: Source.t;
           }
  with bin_io
end
```

You can can combine multiple syntax generators in the same
type declaration by comma-separating them, so you could generate both
formats via `with bin_io,sexp` above.

Next we need to define how to marshal and unmarshal these messages.
The interface is a little more complex than for s-expressions since we
don't just want to serialise from the normal OCaml `string`, but also
to the `bigstring` type.  We'll explain what this is in more detail
shortly, but for now think of it as a more efficient alternative for
large binary data.

```ocaml
let binable =
  (module Message : Binable.S with type t = Message.t)

let save_message outc msg =
  let s = Binable.to_bigstring binable msg in
  let len = Bigstring.length s in
  Out_channel.output_binary_int outc len;
  Bigstring.really_output outc s
```

The `binable` value above captures all the auto-generated `bin_io`
functions into a first-class module of type `Binable.S`.  This
module has the low-level reader and writer functions which we don't
want to have to manually construct.

The `save_message` is then responsible for writing the binary content
out to a `bigstring`.  It first invokes the `Binable.to_bigstring` on
a `Message.t` value to retrieve a marshalled string.  It then determines
the length of this string, and writes out the length and the string
to the output channel.

The `Binable` interface in Core is pretty simple:
```
type 'a m = (module Binable.S with type t = 'a) 
val of_bigstring : 'a m -> bigstring -> 'a
val to_bigstring : ?prefix_with_length:bool -> 'a m -> 'a -> bigstring
val of_string : 'a m -> string -> 'a
val to_string : 'a m -> 'a -> string
```

Since the `Binable.S` module values are generated for you automatically,
the only functions you'll need to regularly use are the conversion functions
above.

Reading back the binary value we've just defined is quite similar. We
read in the length field, read that much data into a `bigstring`, and
convert it to our type using `Binable.of_bigstring`.

```ocaml
let load_message inc =
  match In_channel.input_binary_int inc with
  | None -> failwith "length missing from header"
  | Some len ->
    let buf = Bigstring.create len in
    Bigstring.really_input ~pos:0 ~len inc buf;
    Binable.of_bigstring binable buf
```

The code to generate and read and write these messages now just uses
the static `Message.t` type, with no need to worry about the marshalling
mechanism.

```ocaml
(* Generate some example messages *)
let example content =
  let source =
    { Message.Source.
      hostname = "ocaml.org"; port = 2322 }
  in
  { Message.
    topic = "rwo-example"; content; source; }

(* write out three messages... *)
let write_messages () =
  let outc = Out_channel.create "tmp.bin" in
  List.iter ~f:(save_message outc) [
    example "a wonderful";
    example "trio";
    example "of messages";
  ];
  Out_channel.close outc

(* ... and read them back in *)
let read_messages () =
  let inc = In_channel.create "tmp.bin" in
  for i = 1 to 3 do
    let msg = load_message inc in
    printf "msg %d: %s\n" i msg.Message.content
  done

let () =
  write_messages (); read_messages ()
```

