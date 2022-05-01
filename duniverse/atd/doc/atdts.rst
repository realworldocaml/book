**************************
TypeScript Support - atdts
**************************

This documentation is incomplete. Your help would be appreciated! In
particular, some how-to guides would be great.

..
  The atdts documentation is divided in four parts as advocated by
  Daniele Procida: https://documentation.divio.com/
  I recommend watching the 30-min presentation.

Tutorials
=========

..
  Tutorials are learning-oriented. The reader is taken through a
  series of actions that will directly show them what the tool is
  capable of. Explanations should not be necessary.
  documentation category: practical/exploring

Hello World
-----------

Install ``atdts`` with ``opam``::

  opam install atdts

Create a file ``hello.atd`` containing this:

.. code-block:: ocaml

  type message = {
    subject: string;
    body: string;
  }

Call ``atdts`` to produce ``hello.ts``::

  $ atdts hello.atd

There's now a file ``hello.ts`` that contains a class looking like
this:

.. code-block:: typescript

  ...

  export type Message = {
    subject: string;
    body: string;
  }

  export function writeMessage(x: Message, context: any = x): any {
    ...
  }

  export function readMessage(x: any, context: any = x): Message {
    ...
  }

  ...

Let's write a TypeScript program ``say_hello.ts`` that uses this code:

.. code-block:: typescript

  import * as hello from "./hello"

  const msg: hello.Message = {
    subject: "Hello",
    body: "Dear friend, I hope you are well."
  }

  console.log(JSON.stringify(hello.writeMessage(msg)))

Running it will print the JSON message::

  $ tsc --lib es2017,dom say_hello.ts
  {"subject":"Hello","body":"Dear friend, I hope you are well."}

Such JSON data can be parsed. Let's write a program
``read_message.ts`` that consumes JSON data from standard input:

.. code-block:: typescript

  import * as hello from "./hello"
  import * as readline from "readline"

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  })

  rl.question('', (data: string) => {
    const msg = hello.readMessage(JSON.parse(data))
    console.log("subject: " + msg.subject)
  })


Output::

  # Install dependencies
  $ npm install --save-dev @types/node
  $ npm install readline

  # Compile
  $ tsc --lib es2017,dom read_message.ts

  # Run
  $ echo '{"subject": "big news", "body": ""}' | js read_message.js
  subject: big news

It works! But what happens if the JSON data lacks a ``"subject"``
field? Let's see::

  $ echo '{"body": ""}' | js read_message.js
  {"body": ""}
  readline.js:1086
              throw err;
              ^

  Error: missing field 'subject' in JSON object of type 'Message'
  ...

And what if our program also thought that the correct field name was
``subj`` rather than subject? Here's ``read_message_wrong.ts`` which
tries to access a ``subj`` field:

.. code-block:: typescript

  import * as hello from "./hello"
  import * as readline from "readline"

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  })

  rl.question('', (data: string) => {
    const msg = hello.readMessage(JSON.parse(data))
    console.log("subject: " + msg.subj)
  })

Let's compile our program::

  $ tsc --lib es2017,dom read_message_wrong.ts
  read_message_wrong.ts:11:33 - error TS2339: Property 'subj' does not exist on type 'Message'.

  11   console.log("subject: " + msg.subj)
                                   ~~~~


  Found 1 error in read_message_wrong.ts:11

The typechecker detected that our program makes incorrect assumptions
about the message format without running it.

ATD Records, JSON objects, TypeScript objects
---------------------------------------------

An ATD file contains types that describe the structure of JSON
data. JSON objects map to TypeScript types and objects. They're called
records in the ATD language. Let's define a simple record type
in the file ``hello_plus.atd``:

.. code-block:: ocaml

   type message = {
     subject: string;
     ~body: string;
   }

Note the `~` in front of the ``body`` field. It means that this field
has a default value. Whenever the JSON field is missing from a JSON
object, a default value is assumed. The implicit default value for a
string is ``""``.

Let's add a ``signature`` field whose default value isn't the empty
string:

.. code-block:: ocaml

   type message = {
     subject: string;
     ~body: string;
     ~signature <ts default="'anonymous'">: string;
   }

Finally, we'll add an optional ``url`` field that doesn't take a default value
at all:

.. code-block:: ocaml

   type message = {
     subject: string;
     ~body: string;
     ~signature <ts default="'anonymous'">: string;
     ?url: string option;
   }

Let's generate the TypeScript code for this.

::

   $ atdts hello_plus.atd

Let's update our reader program ``read_message_plus.ts`` to this:

.. code-block:: typescript

  import * as hello_plus from "./hello_plus"
  import * as readline from "readline"

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  })

  rl.question('', (data: string) => {
    const msg = hello_plus.readMessage(JSON.parse(data))
    console.log(msg)
  })

We can test it, showing us the final value of each field::

  $ tsc --lib es2017,dom read_message_plus.ts
  $ echo '{"subject":"hi"}' | js read_message_plus.js
  {"subject":"hi"}
  { subject: 'hi',
    body: '',
    signature: 'anonymous',
    url: undefined }

How-to guides
=============

..
  How-to guides are goal-oriented. They're for solving specific
  problems once the reader is a user with a sense of what the tool
  can do for them.
  documentation category: practical/producing

Defining default field values
-----------------------------

[missing]

Renaming field names
--------------------

[missing]


Deep dives
==========

..
  Deep dives are focused on understanding. They're discussions on a
  topic.
  documentation category: theoretical/exploring

[missing]

Reference
=========

..
  A reference is precise and complete.
  documentation category: theoretical/producing

Type mapping
------------

+--------------------+-------------------------------+-------------------------+
| ATD type           | TypeScript type               | JSON example            |
+====================+===============================+=========================+
| ``unit``           | ``null``                      | ``null``                |
+--------------------+-------------------------------+-------------------------+
| ``bool``           | ``bool``                      | ``True``                |
+--------------------+-------------------------------+-------------------------+
| ``int``            | ``Int``\*                     | ``42`` or ``42.0``      |
+--------------------+-------------------------------+-------------------------+
| ``float``          | ``number``                    | ``6.28``                |
+--------------------+-------------------------------+-------------------------+
| ``string``         | ``string``                    | ``"Hello"``             |
+--------------------+-------------------------------+-------------------------+
| ``string list``    | ``string[]``                  | ``["a", "b", "c!"]``    |
+--------------------+-------------------------------+-------------------------+
| ``(bool * float)`` | ``[boolean, number]``         | ``[-1, 1]``             |
+--------------------+-------------------------------+-------------------------+
| ``int nullable``   | ``Int | null``                | ``42`` or ``null``      |
+--------------------+-------------------------------+-------------------------+
| ``{ id: string }`` | ``{ id: string }``            | ``{"id": "3hj8d"}``     |
+--------------------+-------------------------------+-------------------------+
| ``[A | B of int]`` | ``{kind: 'A'}``               | ``"A"`` or ``["B", 5]`` |
|                    | ``| {kind: 'B', value: Int}`` |                         |
+--------------------+-------------------------------+-------------------------+
| ``foo_bar``        | ``FooBar``                    |                         |
+--------------------+-------------------------------+-------------------------+

\*the ``Int`` type is an alias for ``number`` but additionally, the
read and write functions generated by atdts check that the number
is a whole number.

Supported ATD annotations
-------------------------

Default field values
^^^^^^^^^^^^^^^^^^^^

Record fields following a ``~`` assume a default value. The default value can
be implicit as mandated by the ATD language specification (false for
``bool``, zero for ``int``, etc.) or it can be a user-provided value.

A user-provided default uses an annotation of the form
``<ts default="VALUE">`` where ``VALUE`` evaluates to a TypeScript
expression e.g.

.. code-block:: ocaml

  type foo = {
    ~answer <ts default="42">: int;
  }

For example, the JSON value ``{}`` will be read as ``{answer: 42}``.

Field and constructor renaming
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Alternate JSON object field names can be specified using an annotation
of the form ``<json name="NAME">`` where ``NAME`` is the desired field
name to be used in the JSON representation. For example, the following
specifies the JSON name of the ``id`` field is ``ID``:

.. code-block:: ocaml

   type foo = {
     id <json name="ID">: string
   }

Similarly, the constructor names of sum types can also be given
alternate names in the JSON representation. Here's an example:

.. code-block:: ocaml

   type bar = [
   | Alpha <json name="alpha">
   | Beta <json name="beta"> of int
   ]


Alternate representations for association lists
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

List of pairs can be represented by JSON objects or by
TypeScript maps if the correct annotations are provided:

* ``(string * bar) list <json repr="object">`` will use JSON objects to
  represent a list of pairs of TypeScript type ``[string, Bar][]``.
  Using the annotation ``<json repr="array">`` is equivalent to the default.
* ``(foo * bar) list <ts repr="map">`` will use a TypeScript
  map of type ``Map<Foo, Bar>`` to represent the association list.
  Using the annotation ``<ts repr="array">`` is equivalent to the default.
