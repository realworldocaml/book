# Fast Binary Serialization with bin_prot

S-expressions are a good serialization format when you need something
machine-parseable as well as human readable and editable.  But Sexplib's
s-expressions are not particularly performant for a couple of reasons:

* s-expression serialization goes through an intermediate type, `Sexp.t`, which must be allocated and is then typically
thrown away, putting non-trivial pressure on the garbage collector.
* parsing and printing to strings in an ASCII format can be expensive
for types like `int`s, `float`s and `Time.t`s where some real computation needs
to be done to produce or parse the ASCII representation.

`Bin_prot` is a library that addresses these issues by providing
fast serialization in a compact binary format.  We'll also introduce the
Core `Bigstring` library for handling large binary strings efficiently during this chapter.

<note>
<title>Using `bin_prot` in the toplevel</title>

The `bin_prot` syntax extension isn't activated by default in the toplevel, but is easily available
if you add this to your `~/.ocamlinit` file:

```
#require "bin_prot.syntax"
```

You can also just type this in directly into `utop` (with `;;` to finish the line) instead.
The extension is activated by putting `with bin_io` after the type declaration.
This looks a bit unsightly in the toplevel because of all the definitions that are
generated.  We'll elide those definitions in the book, but you can see them for yourself in the toplevel.

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

## Bigstring

We earlier mentioned that `bigstring` is a more efficient version of `string`.  Understanding the difference requires some understanding of how OCaml allocates values.
TODO.

## Fieldslib
TODO: out of place

One common idiom when using records is to provide field accessor
functions for a particular record.

```ocaml
type t = { topic: string;
           content: string;
           source: Source.t;
         }

let topic   t = t.topic
let content t = t.content
let source  t = t.source
```

Similarly, sometimes you simultaneously want an accessor to a field of
a record and a textual representation of the name of that field.  This
might come up if you were validating a field and needed the string
representation to generate an error message, or if you wanted to
scaffold a form in a GUI automatically based on the fields of a
record.  Fieldslib provides a module `Field` for this purpose.  Here's
some code for creating `Field.t`'s for all the fields of our type `t`.

```ocaml
# module Fields = struct
    let topic =
      { Field.
        name   = "topic";
        setter = None;
        getter = (fun t -> t.topic);
        fset   = (fun t topic -> { t with topic });
      }
    let content =
      { Field.
        name   = "content";
        setter = None;
        getter = (fun t -> t.content);
        fset   = (fun t content -> { t with content });
      }
    let source =
      { Field.
        name   = "source";
        setter = None;
        getter = (fun t -> t.source);
        fset   = (fun t source -> { t with source });
      }
  end ;;
module Fields :
  sig
    val topic : (t, string list) Core.Std.Field.t
    val content : (t, string) Core.Std.Field.t
    val source : (t, Source.t) Core.Std.Field.t
  end
```


There are several syntax extensions distributed with Core, including:

- **Sexplib**: provides serialization for s-expressions.
- **Bin_prot**: provides serialization to an efficient binary
  format.
- **Fieldslib**: generates first-class values that represent fields of
  a record, as well as accessor functions and setters for mutable
  record fields.
- **Variantslib**: like Fieldslib for variants, producing first-class
  variants and other helper functions for interacting with variant
  types.
- **Pa_compare**: generates efficient, type-specialized comparison
  functions.
- **Pa_typehash**: generates a hash value for a type definition,
  _i.e._, an integer that is highly unlikely to be the same for two
  distinct types.

We'll discuss each of these syntax extensions in detail, starting with
Sexplib.

