****************
Atdgen reference
****************

Description
===========

Atdgen is a command-line program that takes as input type definitions in
the `ATD syntax <http://mjambon.com/atd>`__ and produces OCaml code
suitable for data serialization and deserialization.

Two data formats are currently supported, these are
`JSON <http://json.org/>`__ and
`biniou <http://mjambon.com/biniou.html>`__, a binary format with
extensibility properties similar to JSON. Atdgen-json and Atdgen-biniou
will refer to Atdgen used in one context or the other.

Atdgen was designed with efficiency and durability in mind. Software
authors are encouraged to use Atdgen directly and to write tools that
may reuse part of Atdgen's source code.

Atdgen uses the following packages that were developed in conjunction
with Atdgen:

-  ``atd``: parser for the syntax of type definitions
-  ``biniou``: parser and printer for biniou, a binary extensible data
   format
-  ```yojson`` <http://mjambon.com/yojson.html>`__: parser and printer
   for JSON, a widespread text-based data format

Command-line usage
==================

Command-line help
-----------------

Call ``atdgen -help`` for the full list of available options.

Atdgen-json example
-------------------

::

    $ atdgen -t example.atd
    $ atdgen -j -j-std example.atd

Input file ``example.atd``:

.. code:: ocaml

    type profile = {
      id : string;
      email : string;
      ~email_validated : bool;
      name : string;
      ?real_name : string option;
      ~about_me : string list;
      ?gender : gender option;
      ?date_of_birth : date option;
    }

    type gender = [ Female | Male ]

    type date = {
      year : int;
      month : int;
      day : int;
    }

is used to produce files ``example_t.mli``, ``example_t.ml``,
``example_j.mli`` and ``example_j.ml``. This is ``example_j.mli``:

.. code:: ocaml

    (* Auto-generated from "example.atd" *)


    type gender = Example_t.gender

    type date = Example_t.date = { year: int; month: int; day: int }

    type profile = Example_t.profile = {
      id: string;
      email: string;
      email_validated: bool;
      name: string;
      real_name: string option;
      about_me: string list;
      gender: gender option;
      date_of_birth: date option
    }

    val write_gender :
      Bi_outbuf.t -> gender -> unit
      (** Output a JSON value of type {!gender}. *)

    val string_of_gender :
      ?len:int -> gender -> string
      (** Serialize a value of type {!gender}
          into a JSON string.
          @param len specifies the initial length
                     of the buffer used internally.
                     Default: 1024. *)

    val read_gender :
      Yojson.Safe.lexer_state -> Lexing.lexbuf -> gender
      (** Input JSON data of type {!gender}. *)

    val gender_of_string :
      string -> gender
      (** Deserialize JSON data of type {!gender}. *)

    val write_date :
      Bi_outbuf.t -> date -> unit
      (** Output a JSON value of type {!date}. *)

    val string_of_date :
      ?len:int -> date -> string
      (** Serialize a value of type {!date}
          into a JSON string.
          @param len specifies the initial length
                     of the buffer used internally.
                     Default: 1024. *)

    val read_date :
      Yojson.Safe.lexer_state -> Lexing.lexbuf -> date
      (** Input JSON data of type {!date}. *)

    val date_of_string :
      string -> date
      (** Deserialize JSON data of type {!date}. *)

    val write_profile :
      Bi_outbuf.t -> profile -> unit
      (** Output a JSON value of type {!profile}. *)

    val string_of_profile :
      ?len:int -> profile -> string
      (** Serialize a value of type {!profile}
          into a JSON string.
          @param len specifies the initial length
                     of the buffer used internally.
                     Default: 1024. *)

    val read_profile :
      Yojson.Safe.lexer_state -> Lexing.lexbuf -> profile
      (** Input JSON data of type {!profile}. *)

    val profile_of_string :
      string -> profile
      (** Deserialize JSON data of type {!profile}. *)

Module ``Example_t`` (files ``example_t.mli`` and ``example_t.ml``)
contains all OCaml type definitions that can be used independently from
Biniou or JSON.

For convenience, these definitions are also made available from the
``Example_j`` module whose interface is shown above. Any type name,
record field name or variant constructor can be referred to using either
module. For example, the OCaml expressions
``((x : Example_t.date) : Example_j.date)`` and
``x.Example_t.year = x.Example_j.year`` are both valid.

Atdgen-biniou example
---------------------

::

    $ atdgen -t example.atd
    $ atdgen -b example.atd

Input file ``example.atd``:

.. code:: ocaml

    type profile = {
      id : string;
      email : string;
      ~email_validated : bool;
      name : string;
      ?real_name : string option;
      ~about_me : string list;
      ?gender : gender option;
      ?date_of_birth : date option;
    }

    type gender = [ Female | Male ]

    type date = {
      year : int;
      month : int;
      day : int;
    }

is used to produce files ``example_t.mli``, ``example_t.ml``,
``example_b.mli`` and ``example_b.ml``.

This is ``example_b.mli``:

.. code:: ocaml

    (* Auto-generated from "example.atd" *)


    type gender = Example_t.gender

    type date = Example_t.date = { year: int; month: int; day: int }

    type profile = Example_t.profile = {
      id: string;
      email: string;
      email_validated: bool;
      name: string;
      real_name: string option;
      about_me: string list;
      gender: gender option;
      date_of_birth: date option
    }

    (* Writers for type gender *)

    val gender_tag : Bi_io.node_tag
      (** Tag used by the writers for type {!gender}.
          Readers may support more than just this tag. *)

    val write_untagged_gender :
      Bi_outbuf.t -> gender -> unit
      (** Output an untagged biniou value of type {!gender}. *)

    val write_gender :
      Bi_outbuf.t -> gender -> unit
      (** Output a biniou value of type {!gender}. *)

    val string_of_gender :
      ?len:int -> gender -> string
      (** Serialize a value of type {!gender} into
          a biniou string. *)

    (* Readers for type gender *)

    val get_gender_reader :
      Bi_io.node_tag -> (Bi_inbuf.t -> gender)
      (** Return a function that reads an untagged
          biniou value of type {!gender}. *)

    val read_gender :
      Bi_inbuf.t -> gender
      (** Input a tagged biniou value of type {!gender}. *)

    val gender_of_string :
      ?pos:int -> string -> gender
      (** Deserialize a biniou value of type {!gender}.
          @param pos specifies the position where
                     reading starts. Default: 0. *)

    (* Writers for type date *)

    val date_tag : Bi_io.node_tag
      (** Tag used by the writers for type {!date}.
          Readers may support more than just this tag. *)

    val write_untagged_date :
      Bi_outbuf.t -> date -> unit
      (** Output an untagged biniou value of type {!date}. *)

    val write_date :
      Bi_outbuf.t -> date -> unit
      (** Output a biniou value of type {!date}. *)

    val string_of_date :
      ?len:int -> date -> string
      (** Serialize a value of type {!date} into
          a biniou string. *)

    (* Readers for type date *)

    val get_date_reader :
      Bi_io.node_tag -> (Bi_inbuf.t -> date)
      (** Return a function that reads an untagged
          biniou value of type {!date}. *)

    val read_date :
      Bi_inbuf.t -> date
      (** Input a tagged biniou value of type {!date}. *)

    val date_of_string :
      ?pos:int -> string -> date
      (** Deserialize a biniou value of type {!date}.
          @param pos specifies the position where
                     reading starts. Default: 0. *)

    (* Writers for type profile *)

    val profile_tag : Bi_io.node_tag
      (** Tag used by the writers for type {!profile}.
          Readers may support more than just this tag. *)

    val write_untagged_profile :
      Bi_outbuf.t -> profile -> unit
      (** Output an untagged biniou value of type {!profile}. *)

    val write_profile :
      Bi_outbuf.t -> profile -> unit
      (** Output a biniou value of type {!profile}. *)

    val string_of_profile :
      ?len:int -> profile -> string
      (** Serialize a value of type {!profile} into
          a biniou string. *)

    (* Readers for type profile *)

    val get_profile_reader :
      Bi_io.node_tag -> (Bi_inbuf.t -> profile)
      (** Return a function that reads an untagged
          biniou value of type {!profile}. *)

    val read_profile :
      Bi_inbuf.t -> profile
      (** Input a tagged biniou value of type {!profile}. *)

    val profile_of_string :
      ?pos:int -> string -> profile
      (** Deserialize a biniou value of type {!profile}.
          @param pos specifies the position where
                     reading starts. Default: 0. *)

Module ``Example_t`` (files ``example_t.mli`` and ``example_t.ml``)
contains all OCaml type definitions that can be used independently from
Biniou or JSON.

For convenience, these definitions are also made available from the
``Example_b`` module whose interface is shown above. Any type name,
record field name or variant constructor can be referred to using either
module. For example, the OCaml expressions
``((x : Example_t.date) : Example_b.date)`` and
``x.Example_t.year = x.Example_b.year`` are both valid.

Validator example
-----------------

::

    $ atdgen -t example.atd
    $ atdgen -v example.atd

Input file ``example.atd``:

.. code:: ocaml

    type month = int <ocaml valid="fun x -> x >= 1 && x <= 12">
    type day = int <ocaml valid="fun x -> x >= 1 && x <= 31">

    type date = {
      year : int;
      month : month;
      day : day;
    }
      <ocaml validator="Date_util.validate_date">

is used to produce files ``example_t.mli``, ``example_t.ml``,
``example_v.mli`` and ``example_v.ml``. This is ``example_v.ml``,
showing how the user-specified validators are used:

.. code:: ocaml

    (* Auto-generated from "example.atd" *)


    type gender = Example_t.gender

    type date = Example_t.date = { year: int; month: int; day: int }

    type profile = Example_t.profile = {
      id: string;
      email: string;
      email_validated: bool;
      name: string;
      real_name: string option;
      about_me: string list;
      gender: gender option;
      date_of_birth: date option
    }

    val validate_gender :
      Atdgen_runtime.Util.Validation.path -> gender -> Atdgen_runtime.Util.Validation.error option
      (** Validate a value of type {!gender}. *)

    val create_date :
      year: int ->
      month: int ->
      day: int ->
      unit -> date
      (** Create a record of type {!date}. *)

    val validate_date :
      Atdgen_runtime.Util.Validation.path -> date -> Atdgen_runtime.Util.Validation.error option
      (** Validate a value of type {!date}. *)

    val create_profile :
      id: string ->
      email: string ->
      ?email_validated: bool ->
      name: string ->
      ?real_name: string ->
      ?about_me: string list ->
      ?gender: gender ->
      ?date_of_birth: date ->
      unit -> profile
      (** Create a record of type {!profile}. *)

    val validate_profile :
      Atdgen_runtime.Util.Validation.path -> profile -> Atdgen_runtime.Util.Validation.error option
      (** Validate a value of type {!profile}. *)

Default type mapping
====================

The following table summarizes the default mapping between ATD types and
OCaml, biniou and JSON data types. For each language more
representations are available and are detailed in the next section of
this manual.

+-----------------+---------------------+-------------------+--------------------+
| ATD             | OCaml               | JSON              | Biniou             |
+=================+=====================+===================+====================+
| ``unit``        | ``unit``            | null              | unit               |
+-----------------+---------------------+-------------------+--------------------+
| ``bool``        | ``bool``            | boolean           | bool               |
+-----------------+---------------------+-------------------+--------------------+
| ``int``         | ``int``             | -?(0\|[1-9][0-9]\ | svint              |
|                 |                     | *)                |                    |
+-----------------+---------------------+-------------------+--------------------+
| ``float``       | ``float``           | number            | float64            |
+-----------------+---------------------+-------------------+--------------------+
| ``string``      | ``string``          | string            | string             |
+-----------------+---------------------+-------------------+--------------------+
| ``'a option``   | ``'a option``       | ``"None"`` or     | numeric variants   |
|                 |                     | ``["Some", ...]`` | (tag 0)            |
+-----------------+---------------------+-------------------+--------------------+
| ``'a nullable`` | ``'a option``       | ``null`` or       | numeric variants   |
|                 |                     | representation of | (tag 0)            |
|                 |                     | ``'a``            |                    |
+-----------------+---------------------+-------------------+--------------------+
| ``'a list``     | ``'a list``         | array             | array              |
+-----------------+---------------------+-------------------+--------------------+
| ``'a shared``   | no wrapping         | not implemented   | no longer          |
|                 |                     |                   | supported          |
+-----------------+---------------------+-------------------+--------------------+
| ``'a wrap``     | defined by          | representation of | representation of  |
|                 | annotation,         | ``'a``            | ``'a``             |
|                 | converted from      |                   |                    |
|                 | ``'a``              |                   |                    |
+-----------------+---------------------+-------------------+--------------------+
| variants        | polymorphic         | variants          | regular variants   |
|                 | variants            |                   |                    |
+-----------------+---------------------+-------------------+--------------------+
| record          | record              | object            | record             |
+-----------------+---------------------+-------------------+--------------------+
| ``('a * 'b)``   | ``('a * 'b)``       | array             | tuple              |
+-----------------+---------------------+-------------------+--------------------+
| ``('a)``        | ``'a``              | array             | tuple              |
+-----------------+---------------------+-------------------+--------------------+

Notes:

-  Null JSON fields by default are treated as if the field was missing.
   They can be made meaningful with the ``keep_nulls`` flag.
-  JSON nulls are used to represent the unit value and is useful for
   instanciating parametrized types with "nothing".
-  OCaml floats are written to JSON numbers with either a decimal point
   or an exponent such that they are distinguishable from ints, even
   though the JSON standard does not require a distinction between the
   two.
-  The optional values of record fields denoted in ATD by a question
   mark are unwrapped or omitted in both biniou and JSON.
-  JSON option values and JSON variants are represented in standard JSON
   (``atdgen -j -j-std``) by a single string e.g. ``"None"`` or a pair
   in which the first element is the name (constructor) e.g.
   ``["Some", 1234]``. Yojson also provides a specific syntax for
   variants using edgy brackets: ``<"None">``, ``<"Some": 1234>``.
-  Biniou field names and variant names other than the option types use
   the hash of the ATD field or variant name and cannot currently be
   overridden by annotations.
-  JSON tuples in standard JSON (``atdgen -j -j-std``) use the array
   notation e.g. ``["ABC", 123]``. Yojson also provides a specific
   syntax for tuples using parentheses, e.g. ``("ABC", 123)``.
-  Types defined as abstract are defined in another module.

ATD Annotations
===============

Section ``json``
------------------

Field ``keep_nulls``
~~~~~~~~~~~~~~~~~~~~~~

Position: after record

Values: none, ``true`` or ``false``

Semantics: this flag, if present or set to true, indicates that fields
whose JSON value is ``null`` should not be treated as if they were
missing. In this case, ``null`` is parsed as a normal value, possibly of
a ``nullable`` type.

Example: patch semantics

.. code:: ocaml

    (* Type of the objects stored in our database *)
    type t = {
      ?x : int option;
      ?y : int option;
      ?z : int option;
    }

.. code:: ocaml

    (* Type of the requests to modify some of the fields of an object. *)
    type t_patch = {
      ?x : int nullable option; (* OCaml type: int option option *)
      ?y : int nullable option;
      ?z : int nullable option;
    } <ocaml field_prefix="patch_"> <json keep_nulls>

Let's consider the following json patch that means "set ``x`` to 1,
clear ``y`` and keep ``z`` as it is":

::

    {
      "x": 1,
      "y": null
    }

It will be parsed by the generated function ``t_patch_of_string`` into
the following OCaml value:

.. code:: ocaml

    {
      patch_x = Some (Some 1);
      patch_y = Some None;
      patch_z = None;
    }

Then presumably some code would be written to apply the patch to an
object of type ``t``. Such code is not generated by atdgen at this time.

Available: from atd 1.12

Field ``name``
~~~~~~~~~~~~~~~~

Position: after field name or variant name

Values: any string making a valid JSON string value

Semantics: specifies an alternate object field name or variant name to
be used by the JSON representation.

Example:

.. code:: ocaml

    type color = [
        Black <json name="black">
      | White <json name="white">
      | Grey <json name="grey">
    ]

    type profile = {
      id <json name="ID"> : int;
      username : string;
      background_color : color;
    }

A valid JSON object of the ``profile`` type above is:

::

    {
      "ID": 12345678,
      "username": "kimforever",
      "background_color": "black"
    }

Field ``repr``
~~~~~~~~~~~~~~~~

Association lists
^^^^^^^^^^^^^^^^^

Position: after ``(string * _) list`` type

Values: ``object``

Semantics: uses JSON's object notation to represent association lists.

Example:

.. code:: ocaml

    type counts = (string * int) list <json repr="object">

A valid JSON object of the ``counts`` type above is:

::

    {
      "bob": 3,
      "john": 1408,
      "mary": 450987,
      "peter": 93087
    }

Without the annotation ``<json repr="object">``, the data above would be
represented as:

::

    [
      [ "bob", 3 ],
      [ "john", 1408 ],
      [ "mary", 450987 ],
      [ "peter", 93087 ]
    ]

Floats
^^^^^^

Position: after ``float`` type

Values: ``int``

Semantics: specifies a float value that must be rounded to the nearest
integer and represented in JSON without a decimal point nor an exponent.

Example:

.. code:: ocaml

    type unixtime = float <json repr="int">

Field ``tag_field``
~~~~~~~~~~~~~~~~~~~~~

Superseded by ``<json adapter.ocaml="...">``. Available since atdgen
1.5.0 and yojson 1.2.0 until atdgen 1.13.

This feature makes it possible to read JSON objects representing
variants that use one field for the tag and another field for the
untagged value of the specific type associated with that tag.

Position: on a record field name, for a field holding a variant type.

Value: name of another JSON field which holds the string representing
the constructor for the variant.

Semantics: The type definition

.. code:: ocaml

    type t = {
      value <json tag_field="kind">: [ A | B <json name="b"> of int ];
    }

covers JSON objects that have an extra field ``kind`` which holds either
``"A"`` or ``"b"``. Valid JSON values of type ``t`` include
``{ "kind": "A" }`` and ``{ "kind": "b", "value": 123 }``.

Field ``untyped``
~~~~~~~~~~~~~~~~~~~

Superseded by ``<json open_enum>`` and ``<json adapter.ocaml="...">``.
Available since atdgen 1.10.0 and atd 1.2.0 until atdgen 1.13.

This flag enables parsing of arbitrary variants without prior knowledge
of their type. It is useful for constructing flexible parsers for
extensible serializations. ``json untyped`` is compatible with regular
variants, ``json tag_field`` variants, default values, and implicit
``tag_field`` constructors.

Position: on a variant constructor with argument type
``string * json option`` (at most one per variant type)

Value: none, ``true`` or ``false``

Semantics: The type definition

.. code:: ocaml

    type v = [
      | A
      | B <json name="b"> of int
      | Unknown <json untyped> of (string * json option)
    ]

will parse and print ``"A"``, ``["b", 0]``, ``"foo"``, and
``["bar", [null]]`` in a regular variant context. In the ``tag_field``
type ``t`` context in the previous section, ``v`` will parse and print
``{ "kind": "foo" }`` and ``{ "kind": "bar", "value": [null] }`` as well
as the examples previously given.

Field ``open_enum``
~~~~~~~~~~~~~~~~~~~~~

Where an enum (finite set of strings) is expected, this flag allows
unexpected strings to be kept under a catch-all constructor rather than
producing an error.

Position: on a variant type comprising exactly one constructor with an
argument. The type of that argument must be ``string``. All other
constructors must have no arguments.

Value: none

For example:

.. code:: ocaml

    type language = [
      | English
      | Chinese
      | Other of string
    ] <json open_enum>

maps the json string ``"Chinese"`` to the OCaml value ```Chinese`` and
maps ``"French"`` to ```Other "French"``.

Available since atdgen 2.0.

Field ``adapter.ocaml``
~~~~~~~~~~~~~~~~~~~~~~~~~

Json adapters are a mechanism for rearranging json data on-the-fly, so
as to make them compatible with ATD. The programmer must provide an
OCaml module that provides converters between the original json
representation and the ATD-compatible representation. The signature of
the user-provided module must be equal to
``Atdgen_runtime.Json_adapter.S``, which is:

.. code:: ocaml

    sig
      (** Convert from original json to ATD-compatible json *)
      val normalize : Yojson.Safe.t -> Yojson.Safe.t

      (** Convert from ATD-compatible json to original json *)
      val restore : Yojson.Safe.t -> Yojson.Safe.t
    end

The type ``Yojson.Safe.t`` is the type of parsed JSON as provided by
the yojson library.

Position: on a variant type or on a record type.

Value: an OCaml module identifier. Note that
``Atdgen_runtime.Json_adapter`` provides a few modules and functors that
are ready to use. Users are however encouraged to write their own to
suit their needs.

Sample ATD definitions:

.. code:: ocaml

    type document = [
      | Image of image
      | Text of text
    ] <json adapter.ocaml="Atdgen_runtime.Json_adapter.Type_field">

    type image = {
      url: string;
    }

    type text = {
      title: string;
      body: string;
    }

ATD-compliant json values:

-  ``["Image", {"url": "https://example.com/ocean123.jpg"}]``
-  ``["Text", {"title": "Cheeses Around the World", "body": "..."}]``

Corresponding json values given by some API:

-  ``{"type": "Image", "url": "https://example.com/ocean123.jpg"}``
-  ``{"type": "Text", "title": "Cheeses Around the World", "body": "..."}``

The json adapter ``Type_field`` that ships with the atdgen runtime takes
care of converting between these two forms. For information on how to
write your own adapter, please consult the documentation for the yojson
library.

Section ``biniou``
--------------------

Field ``repr``
~~~~~~~~~~~~~~~~

Integers
^^^^^^^^

Position: after ``int`` type

Values: ``svint`` (default), ``uvint``, ``int8``, ``int16``, ``int32``,
``int64``

Semantics: specifies an alternate type for representing integers. The
default type is ``svint``. The other integers types provided by biniou
are supported by Atdgen-biniou. They have to map to the corresponding
OCaml types in accordance with the following table:

+---------------+------------------------+---------------------------------------+
| Biniou type   | Supported OCaml type   | OCaml value range                     |
+===============+========================+=======================================+
| ``svint``     | ``int``                | ``min_int`` ... ``max_int``           |
+---------------+------------------------+---------------------------------------+
| ``uvint``     | ``int``                | 0 ... ``max_int``, ``min_int`` ... -1 |
+---------------+------------------------+---------------------------------------+
| ``int8``      | ``char``               | ``'\000`` ... ``'\255``             |
+---------------+------------------------+---------------------------------------+
| ``int16``     | ``int``                | 0 ... 65535                           |
+---------------+------------------------+---------------------------------------+
| ``int32``     | ``int32``              | ``Int32.min_int`` ...                 |
|               |                        | ``Int32.max_int``                     |
+---------------+------------------------+---------------------------------------+
| ``int64``     | ``int64``              | ``Int64.min_int`` ...                 |
|               |                        | ``Int64.max_int``                     |
+---------------+------------------------+---------------------------------------+

In addition to the mapping above, if the OCaml type is ``int``, any
biniou integer type can be read into OCaml data regardless of the
declared biniou type.

Example:

.. code:: ocaml

    type t = {
      id : int
        <ocaml repr="int64">
        <biniou repr="int64">;
      data : string list;
    }

Floating-point numbers
^^^^^^^^^^^^^^^^^^^^^^

Position: after ``float`` type

Values: ``float64`` (default), ``float32``

Semantics: ``float32`` allows for a shorter serialized representation of
floats, using 4 bytes instead of 8, with reduced precision. OCaml floats
always use 8 bytes, though.

Example:

.. code:: ocaml

    type t = {
      lat : float <biniou repr="float32">;
      lon : float <biniou repr="float32">;
    }

Arrays and tables
^^^^^^^^^^^^^^^^^

Position: applies to lists of records

Values: ``array`` (default), ``table``

Semantics: ``table`` uses biniou's table format instead of a regular
array for serializing OCaml data into biniou. Both formats are supported
for reading into OCaml data regardless of the annotation. The table
format allows

Example:

.. code:: ocaml

    type item = {
      id : int;
      data : string list;
    }

    type items = item list <biniou repr="table">

Section ``ocaml``
-------------------

Field ``predef``
~~~~~~~~~~~~~~~~~~

Position: left-hand side of a type definition, after the type name

Values: none, ``true`` or ``false``

Semantics: this flag indicates that the corresponding OCaml type
definition must be omitted.

Example:

.. code:: ocaml

    (* Some third-party OCaml code *)
    type message = {
      from : string;
      subject : string;
      body : string;
    }

.. code:: ocaml

    (*
       Our own ATD file used for making message_of_string and
       string_of_message functions.
    *)
    type message <ocaml predef> = {
      from : string;
      subject : string;
      body : string;
    }

Field ``mutable``
~~~~~~~~~~~~~~~~~~~

Position: after a record field name

Values: none, ``true`` or ``false``

Semantics: this flag indicates that the corresponding OCaml record field
is mutable.

Example:

.. code:: ocaml

    type counter = {
      total <ocaml mutable> : int;
      errors <ocaml mutable> : int;
    }

translates to the following OCaml definition:

.. code:: ocaml

    type counter = {
      mutable total : int;
      mutable errors : int;
    }

Field ``default``
~~~~~~~~~~~~~~~~~~~

Position: after a record field name marked with a ``\~{``} symbol or at
the beginning of a tuple field.

Values: any valid OCaml expression

Semantics: specifies an explicit default value for a field of an OCaml
record or tuple, allowing that field to be omitted. Default strings must
be escaped.

Example:

.. code:: ocaml

    type color = [ Black | White | Rgb of (int * int * int) ]

    type ford_t = {
      year : int;
      ~color <ocaml default="`Black"> : color;
      ~name <ocaml default="\"Ford Model T\""> : string;
    }

    type point = (int * int * <ocaml default="0"> : int)

Field ``from``
~~~~~~~~~~~~~~~~

Position: left-hand side of a type definition, after the type name

Values: OCaml module name without the ``_t``, ``_b``, ``_j`` or ``_v``
suffix. This can be also seen as the name of the original ATD file,
without the ``.atd`` extension and capitalized like an OCaml module
name.

Semantics: specifies the base name of the OCaml modules where the type
and values coming with that type are defined.

It is useful for ATD types defined as ``abstract`` and for types
annotated as predefined using the annotation ``<ocaml predef>``. In both
cases, the missing definitions must be provided by modules composed of
the base name and the standard suffix assumed by Atdgen which is ``_t``,
``_b``, ``_j`` or ``_v``.

Example: First input file ``part1.atd``:

.. code:: ocaml

    type point = { x : int; y : int }

Second input file ``part2.atd`` depending on the first one:

.. code:: ocaml

    type point <ocaml from="Part1"> = abstract
    type points = point list

To use a different type name than defined in the ``Part1`` module, add a
``t`` field declaration to the annotation which refers to the original
type name:

.. code:: ocaml

    type point_xy <ocaml from="Part1" t="point"> = abstract
    type points = point_xy list


Field ``module``
~~~~~~~~~~~~~~~~~~

Using a custom wrapper
^^^^^^^^^^^^^^^^^^^^^^

Using the built-in ``wrap`` constructor, it is possible to add a layer
of abstraction on top of the concrete structure used for serialization.

Position: after a ``wrap`` type constructor

Values: OCaml module name

A common use case is to parse strings used as unique identifiers and
wrap the result into an abstract type. Our OCaml module ``Uid`` needs to
provide a type ``t``, and two functions ``wrap`` and ``unwrap`` as
follows:

.. code:: ocaml

    type t
    val wrap : string -> t
    val unwrap : t -> string

Given that ``Uid`` OCaml module, we can write the following ATD
definition:

.. code:: ocaml

    type uid = string wrap <ocaml module="Uid">

Other languages than OCaml using the same ATD type definitions may or
may not add their own abstract layer. Without an annotation, the
``wrap`` construct has no effect on the value being wrapped, i.e.
``wrap`` and ``unwrap`` default to the identity function.

It is also possible to define ``t``, ``wrap``, and ``unwrap`` inline:

.. code:: ocaml

    type uid = string wrap <ocaml t="Uid.t"
                                  wrap="Uid.wrap"
                                  unwrap="Uid.unwrap">

This can be useful for very simple validation:

.. code:: ocaml

    type uid = string wrap
      <ocaml wrap="fun s ->
                     if String.length s <> 16 then
                       failwith \"Invalid user ID\";
                     s"
      >

Importing an external type definition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In most cases since Atdgen 1.2.0 ``module`` annotations are deprecated
in favor of ``from`` annotations previously described.

Position: left-hand side of a type definition, after the type name

Values: OCaml module name

Semantics: specifies the OCaml module where the type and values coming
with that type are defined. It is useful for ATD types defined as
``abstract`` and for types annotated as predefined using the annotation
``<ocaml predef>``. In both cases, the missing definitions can be
provided either by globally opening an OCaml module with an OCaml
directive or by specifying locally the name of the module to use.

The latter approach is recommended because it allows to create type and
value aliases in the OCaml module being generated. It results in a
complete module signature regardless of the external nature of some
items.

Example: Input file ``example.atd``:

.. code:: ocaml

    type document <ocaml module="Doc"> = abstract

    type color <ocaml predef module="Color"> =
      [ Black | White ] <ocaml repr="classic">

    type point <ocaml predef module="Point"> = {
      x : float;
      y : float;
    }

gives the following OCaml type definitions (file ``example.mli``):

.. code:: ocaml

    type document = Doc.document

    type color = Color.color =  Black | White

    type point = Point.point = { x: float; y: float }

Now for instance ``Example.Black`` and ``Color.Black`` can be used
interchangeably in other modules.

Field ``t``
~~~~~~~~~~~~~

Using a custom wrapper
^^^^^^^^^^^^^^^^^^^^^^

Specifies the OCaml type of an abstract ``wrap`` construct, possibly
overriding the default *M*\ ``.t`` if *M* is the module where the
``wrap`` and ``unwrap`` functions are found.

Position: after a ``wrap`` type constructor

Values: OCaml type name

Example:

.. code:: ocaml

    type uid = string wrap <ocaml module="Uid" t="Uid.uid">

is equivalent to:

.. code:: ocaml

    type uid = string wrap <ocaml t="Uid.uid" wrap="Uid.wrap" unwrap="Uid.unwrap">

Importing an external type definition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Position: left-hand side of a type definition, after the type name. Must
be used in conjunction with a ``module`` field.

Values: OCaml type name as found in an external module.

Semantics: This option allows to specify the name of an OCaml type
defined in an external module.

It is useful when the type needs to be renamed because its original name
is already in use or not enough informative. Typically we may want to
give the name ``foo`` to a type originally defined in OCaml as
``Foo.t``.

Example:

.. code:: ocaml

    type foo <ocaml_biniou module="Foo" t="t"> = abstract
    type bar <ocaml_biniou module="Bar" t="t"> = abstract
    type t <ocaml_biniou module="Baz"> = abstract

allows local type names to be unique and gives the following OCaml type
definitions:

.. code:: ocaml

    type foo = Foo.t
    type bar = Bar.t
    type t = Baz.t

Fields ``wrap`` and ``unwrap``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See "Using a custom wrapper" under section ``ocaml``, fields
``module`` and ``t``.

Field ``field_prefix``
~~~~~~~~~~~~~~~~~~~~~~~~

Position: record type expression

Values: any string making a valid prefix for OCaml record field names

Semantics: specifies a prefix to be prepended to each field of the OCaml
definition of the record. Overridden by alternate field names defined on
a per-field basis.

Example:

.. code:: ocaml

    type point2 = {
      x : int;
      y : int;
    } <ocaml field_prefix="p2_">

gives the following OCaml type definition:

.. code:: ocaml

    type point2 = {
      p2_x : int;
      p2_y : int;
    }

Field ``name``
~~~~~~~~~~~~~~~~

Position: after record field name or variant name

Values: any string making a valid OCaml record field name or variant
name

Semantics: specifies an alternate record field name or variant names to
be used in OCaml.

Example:

.. code:: ocaml

    type color = [
        Black <ocaml name="Grey0">
      | White <ocaml name="Grey100">
      | Grey <ocaml name="Grey50">
    ]

    type profile = {
      id <ocaml name="profile_id"> : int;
      username : string;
    }

gives the following OCaml type definitions:

.. code:: ocaml

    type color = [
        `Grey0
      | `Grey100
      | `Grey50
    ]

    type profile = {
      profile_id : int;
      username : string;
    }

Field ``repr``
~~~~~~~~~~~~~~~~

Integers
^^^^^^^^

Position: after ``int`` type

Values: ``char``, ``int32``, ``int64``, ``float``

Semantics: specifies an alternate type for representing integers. The
default type is ``int``, but ``char``, ``int32``, ``int64`` or ``float``
can be used instead.

The three types ``char``, ``int32`` and ``int64`` are supported by both
Atdgen-biniou and Atdgen-json but Atdgen-biniou currently requires that
they map to the corresponding fixed-width types provided by the biniou
format.

The type ``float`` is only supported in conjunction with JSON and is
useful when an OCaml float is used to represent an integral value, such
as a time in seconds returned by ``Unix.time()``. When converted into
JSON, floats are rounded to the nearest integer.

Example:

.. code:: ocaml

    type t = {
      id : int
        <ocaml repr="int64">
        <biniou repr="int64">;
      data : string list;
    }

Lists and arrays
^^^^^^^^^^^^^^^^

Position: after a ``list`` type

Values: ``array``

Semantics: maps to OCaml's ``array`` type instead of ``list``.

Example:

.. code:: ocaml

    type t = {
      id : int;
      data : string list
        <ocaml repr="array">;
    }

Sum types
^^^^^^^^^

Position: after a sum type (denoted by square brackets)

Values: ``classic``

Semantics: maps to OCaml's classic variants instead of polymorphic
variants.

Example:

.. code:: ocaml

    type fruit = [ Apple | Orange ] <ocaml repr="classic">

translates to the following OCaml type definition:

.. code:: ocaml

    type fruit = Apple | Orange

Shared values (obsolete)
^^^^^^^^^^^^^^^^^^^^^^^^

Position: after a ``shared`` type

This feature is obsolete and was last supported by atdgen 1.3.1.

Field ``valid``
~~~~~~~~~~~~~~~~~

Since atdgen 1.6.0.

Position: after any type expression except type variables

Values: OCaml function that takes one argument of the given type and
returns a bool

Semantics: ``atdgen -v`` produces for each type named *t* a function
``validate_``\ *t*:

.. code:: ocaml

    val validate_t : Atdgen_runtime.Util.Validation.path -> t -> Atdgen_runtime.Util.Validation.error option

Such a function returns ``None`` if and only if the value and all of its
subnodes pass all the validators specified by annotations of the form
``<ocaml validator="...">`` or ``<ocaml valid="...">`` (at most one per
node).

Example:

.. code:: ocaml

    type positive = int <ocaml validator="fun x -> x > 0">

    type point = {
      x : positive;
      y : positive;
      z : int;
    }
      <ocaml valid="Point.validate">
      (* Some validating function from a user-defined module Point *)

The generated ``validate_point`` function calls the validator for the
containing object first (``Point.validate``) and continues on its fields
``x`` then ``y`` until an error is returned.

.. code:: ocaml

    match validate_point [] { x = 1; y = 0; z = 1 } with
    | None -> ()
    | Some e ->
        Printf.eprintf "Error: %s\n%!"
          (Atdgen_runtime.Util.Validation.string_of_error e)

The above code prints the following error message:

::

    Error: Validation error; path = <root>.y

In order to customize the error message and print the faulty value, use
``validator`` instead of ``valid``, as described next.

Field ``validator``
~~~~~~~~~~~~~~~~~~~~~

This is a variant of the ``valid`` annotation that allows full control
over the error message that gets generated in case of an error.

Position: after any type expression except type variables

Values: OCaml function that takes the path in current JSON structure and
the object to validate, and returns an optional error.

Semantics: ``atdgen -v`` produces for each type named *t* a function
``validate_``\ *t*:

.. code:: ocaml

    val validate_t : Atdgen_runtime.Util.Validation.path -> t -> Atdgen_runtime.Util.Validation.error option

Such a function returns ``None`` if and only if the value and all of its
subnodes pass all the validators specified by annotations of the form
``<ocaml validator="...">`` or ``<ocaml valid="...">`` (at most one per
node).

Example:

.. code:: ocaml

    type positive = int <ocaml validator="
      fun path x ->
        if x > 0 then None
        else
          Some (
            Atdgen_runtime.Util.Validation.error
              ~msg: (\"Not a positive integer: \" ^ string_of_int x)
              path
          )
    ">

    type point = {
      x : positive;
      y : positive;
      z : int;
    }
      <ocaml validator="Point.validate">
      (* Some validating function from a user-defined module Point *)

The following user code

.. code:: ocaml

    match Toto_v.validate_point [] { x = 1; y = 0; z = 1 } with
    | None -> ()
    | Some e ->
        Printf.eprintf "Error: %s\n%!"
          (Atdgen_runtime.Util.Validation.string_of_error e)

results in printing:

::

    Error: Validation error: Not a positive integer: 0; path = <root>.y

Section ``ocaml_biniou``
--------------------------

Section ``ocaml_biniou`` takes precedence over section ``ocaml`` in
Biniou mode (``-b``) for the following fields:

-  ``predef`` (see section ``ocaml``, field ``predef``)
-  ``module`` (see section ``ocaml``, field ``module``)
-  ``t`` (see section ``ocaml.t``)

Section ``ocaml_json`` (obsolete)
-----------------------------------

Section ``ocaml_json`` takes precedence over section ``ocaml`` in JSON
mode (``-json`` or ``-j``) for the following fields:

-  ``predef`` (see section ``ocaml``, field ``predef``)
-  ``module`` (see section ``ocaml``, field ``module``)
-  ``t`` (see section ``ocaml``, field ``t``)

Please note that ``atdgen -json`` is now deprecated in favor of
``atdgen -j`` (json) and ``atdgen -t`` (types). The latter is in charge
of producing type definitions independently from JSON and will ignore
``<ocaml_json ...>`` annotations, making them almost useless. The
equivalent ``<ocaml ...>`` annotations are almost always preferable.

Example:

This example shows how to parse a field into a generic tree of type
``Yojson.Safe.t`` rather than a value of a specialized OCaml type.

.. code:: ocaml

    type dyn <ocaml_json module="Yojson.Safe" t="json"> = abstract

    type t = { foo: int; bar: dyn }

translates to the following OCaml type definitions:

.. code:: ocaml

    type dyn = Yojson.Safe.t

    type t = { foo : int; bar : dyn }

Sample OCaml value of type ``t``:

.. code:: ocaml

    {
      foo = 12345;
      bar =
        `List [
          `Int 12;
          `String "abc";
          `Assoc [
            "x", `Float 3.14;
            "y", `Float 0.0;
            "color", `List [ `Float 0.3; `Float 0.0; `Float 1.0 ]
          ]
        ]
    }

Corresponding JSON data as obtained with ``string_of_t``:

::

    {"foo":12345,"bar":[12,"abc",{"x":3.14,"y":0.0,"color":[0.3,0.0,1.0]}]}

Section ``doc``
-----------------

Unlike comments, ``doc`` annotations are meant to be propagated into the
generated source code. This is useful for making generated interface
files readable without having to consult the original ATD file.

Generated source code comments can comply to a standard format and take
advantage of documentation generators such as javadoc or ocamldoc.

Field ``text``
~~~~~~~~~~~~~~~~

Position:

-  after the type name on the left-hand side of a type definition
-  after the type expression on the right hand of a type definition (but
   not after any type expression)
-  after record field names
-  after variant names

Values: UTF-8-encoded text using a minimalistic markup language

Semantics: The markup language is defined as follows:

-  Blank lines separate paragraphs.
-  ``{{ }}`` can be used to enclose inline verbatim text.
-  ``{{{ }}}`` can be used to enclose verbatim text where whitespace is
   preserved.
-  The backslash character is used to escape special character
   sequences. In regular paragraph mode the special sequences are ``\``,
   ``{{`` and ``{{{``. In inline verbatim text, special sequences are
   ``\`` and ``}}``. In verbatim text, special sequences are ``\`` and
   ``}}}``.

Example: The following is an example demonstrating the use of ``doc``
annotations generated using:

::

    $ atdgen -t ocamldoc_example.atd

Input file ``ocamldoc_example.atd``:

.. code:: ocaml

    <doc text="This is the title">

    type point = {
      x <doc text="The first coordinate">: float;
      y <doc text="The second coordinate">: float;
    }
      <doc text="
    The type of a point. A value {{p}} can be created as follows:
    {{{
    let p = { x = 1.2; y = 5.0 }
    }}}
    ">

    type color = [
     | Black <doc text="Same as {{RGB (0,0,0)}}">
     | White <doc text="Same as {{RGB (255, 255, 255)}}">
     | RGB
         <doc text="Red, green, blue components">
         of (int * int * int)
    ]

translates using ``atdgen -t ocamldoc_example.atd`` into the following
OCaml interface file ``ocamldoc_example_t.mli`` with ocamldoc-compliant
comments:

.. code:: ocaml

    (* Auto-generated from "ocamldoc_example.atd" *)


    (** This is the title *)

    (**
      The type of a point. A value [p] can be created as follows:
      
    {v
    let p = \{ x = 1.2; y = 5.0 \}
    v}
    *)
    type point = {
      x: float (** The first coordinate *);
      y: float (** The second coordinate *)
    }

    type color = [
        `Black (** Same as [RGB (0,0,0)] *)
      | `White (** Same as [RGB (255, 255, 255)] *)
      | `RGB of (int * int * int) (** Red, green, blue components *)
    ]

Atdgen runtime library
======================

A library named `atdgen-runtime <https://github.com/mjambon/atd/tree/master/atdgen-runtime/src>`_ is installed by the standard installation
process. Only a fraction of it is officially supported and documented.

Modules intended for all users are:

-  ``Util``
-  ``Json_adapter``

The other modules exported by the library are used directly by
generated code. Tool developers may use them but we don't guarantee
strong compatibility across releases.
