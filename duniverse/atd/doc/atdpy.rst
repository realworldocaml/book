**********************
Python Support - atdpy
**********************

This documentation is incomplete. Your help would be appreciated! In
particular, some how-to guides would be great.

..
  The atdpy documentation is divided in four parts as advocated by
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

Install ``atdpy`` with ``opam``::

  opam install atdpy

Create a file ``hello.atd`` containing this:

.. code-block:: ocaml

  type message = {
    subject: string;
    body: string;
  }

Call ``atdpy`` to produce ``hello.py``::

   $ atdpy hello.atd

There's now a file ``hello.py`` that contains a class looking like
this:

.. code-block:: python

  ...

  @dataclass
  class Message:
    """Original type: message = { ... }"""

    subject: str
    body: str

    @classmethod
    def from_json(cls, x: Any) -> 'Message':
        ...

    def to_json(self) -> Any:
        ...

    @classmethod
    def from_json_string(cls, x: str) -> 'Message':
        ...

    def to_json_string(self, **kw: Any) -> str:
        ...

Let's write a Python program ``say_hello.py`` that uses this code:

.. code-block:: python

   import hello

   msg = hello.Message("Hello", "Dear friend, I hope you are well.")
   print(msg.to_json_string())

Running it will print the JSON message::

   $ python3 say_hello.py
   {"subject": "Hello", "body": "Dear friend, I hope you are well."}

Such JSON data can be parsed. Let's write a program
``read_message.py`` that consumes JSON data from standard input:

.. code-block:: python

   import hello, sys, json

   data = json.load(sys.stdin)
   msg = hello.Message.from_json(data)
   print(f"subject: {msg.subject}")

Output::

   $ echo '{"subject": "big news", "body": ""}' | python3 read_message.py
   subject: big news

It works! But what happens if the JSON data lacks a ``"subject"``
field? Let's see::

   $ echo '{"subj": "big news", "body": ""}' | python3 read_message.py
   Traceback (most recent call last):
   ...
   ValueError: missing field 'subject' in JSON object of type 'Message'

And what if our program also thought that the correct field name was
``subj`` rather than subject? Here's ``read_message_wrong.py`` which
tries to access a ``subj`` field::

   import hello, sys, json

   data = json.load(sys.stdin)
   msg = hello.Message.from_json(data)
   print(f"subject: {msg.subj}")

Let's run the program through mypy::

   $ mypy read_message_wrong.py
   read_message_wrong.py:5: error: "Message" has no attribute "subj"
   Found 1 error in 1 file (checked 1 source file)

Mypy detected that our program makes incorrect assumptions about the
message format without running it. On the correct program
``read_message.py``, we get a reassuring message::

   $ mypy read_message.py
   Success: no issues found in 1 source file


ATD Records, JSON objects, Python classes
-----------------------------------------

An ATD file contains types that describe the structure of JSON
data. JSON objects map to Python classes and objects. They're called
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
     ~signature <python default="'anonymous'">: string;
   }

Finally, we'll add an optional ``url`` field that doesn't take a default value
at all:

.. code-block:: ocaml

   type message = {
     subject: string;
     ~body: string;
     ~signature <python default="'anonymous'">: string;
     ?url: string option;
   }

Let's generate the Python code for this.

::

   $ atdpy hello_plus.atd

Let's update our reader program ``read_message_plus.py`` to this:

.. code-block:: python

   import hello_plus, sys, json

   data = json.load(sys.stdin)
   msg = hello_plus.Message.from_json(data)
   print(msg)

We can test it, showing us the final value of each field::

   $ echo '{"subject":"hi"}' | python3 read_message_plus.py
   Message(subject='hi', body='', signature='anonymous', url=None)


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

+--------------------+----------------------+-------------------------+
| ATD type           | Python type          | JSON example            |
+====================+======================+=========================+
| ``unit``           | ``None``             | ``null``                |
+--------------------+----------------------+-------------------------+
| ``bool``           | ``bool``             | ``True``                |
+--------------------+----------------------+-------------------------+
| ``int``            | ``int``              | ``42``                  |
+--------------------+----------------------+-------------------------+
| ``float``          | ``float``            | ``6.28``                |
+--------------------+----------------------+-------------------------+
| ``string``         | ``str``              | ``"Hello"``             |
+--------------------+----------------------+-------------------------+
| ``int list``       | ``List[int]``        | ``[1, 2, 3]``           |
+--------------------+----------------------+-------------------------+
| ``(int * int)``    | ``Tuple[int, int]``  | ``[-1, 1]``             |
+--------------------+----------------------+-------------------------+
| ``int nullable``   | ``Union[int, None]`` | ``42`` or ``null``      |
+--------------------+----------------------+-------------------------+
| ``abstract``       | ``Any``              | anything                |
+--------------------+----------------------+-------------------------+
| record type        | class                | ``{"id": 17}``          |
+--------------------+----------------------+-------------------------+
| ``[A | B of int]`` | ``Union[A, B]``      | ``"A"`` or ``["B", 5]`` |
+--------------------+----------------------+-------------------------+
| ``foo_bar``        | ``FooBar``           |                         |
+--------------------+----------------------+-------------------------+

Supported ATD annotations
-------------------------

Default field values
^^^^^^^^^^^^^^^^^^^^

Record fields following a ``~`` assume a default value. The default value can
be implicit as mandated by the ATD language specification (false for
``bool``, zero for ``int``, etc.) or it can be a user-provided value.

A user-provided default uses an annotation of the form
``<python default="VALUE">`` where ``VALUE`` evaluates to a Python
expression e.g.

.. code-block:: ocaml

  type foo = {
    ~answer <python default="42">: int;
  }

Default values are always honored when reading JSON data from
Python. However, the implementation of ``dataclass`` via the
``@dataclass`` decorator prevents the use of mutable values for
defaults. This causes class constructors to not have default fields
that are mutable such as ``[]``. For example:

.. code-block:: ocaml

   type bar = {
     ~items: int list;
   }

will translate to a class constructor that requires one argument of
type list. For example, ``Bar([1, 2, 3])`` would be legal but
``Bar()`` would be illegal. Reading from the JSON object ``{}`` would
however succeed. Therefore, the following two Python expressions would
be valid and equivalent:

.. code-block:: python

   Bar([])
   Bar.from_json_string('{}')


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

Note that field names and constructor names in the generated Python
code are assigned automatically so as to avoid conflicts with
Python keywords or reserved identifiers.


Alternate representations for association lists
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

List of pairs can be represented by JSON objects or by
Python dictionaries if the correct annotations are provided:

* ``(string * bar) list <json repr="object">`` will use JSON objects to
  represent a list of pairs of Python type ``List[str, Bar]``.
  Using the annotation ``<json repr="array">`` is equivalent to the default.
* ``(foo * bar) list <python repr="dict">`` will use a Python
  dictionary of type ``Dict[Foo, Bar]`` to represent the association list.
  Using the annotation ``<python repr="list">`` is equivalent to the default.


Additional imports
^^^^^^^^^^^^^^^^^^

At the beginning of the ATD file, placing annotations like this one
allow inserting arbitrary Python code or comments:

::

   <python text="import deco">

This is the recommended mechanism for inserting imports. In contrast, it
should be used only as last resort for inserting functions or classes.

In the future, atdpy may generate more than one kind of files. An
annotation of the form ``<python text="...">`` will insert that text
into all the generated files. In order to insert code only in the
``.py`` file that handles JSON, it is recommended to use a more
specific annotation of the form ``<python json_py.text="...">``:

::

   <python json_py.text="import deco">


Custom class decorators
^^^^^^^^^^^^^^^^^^^^^^^

Extra class decorators can be specified in addition to ``@dataclass``.
The following ATD definition will add 3 decorators:

.. code-block:: ocaml

   type thing <python decorator="deco.deco1"
                      decorator="deco.deco2(42)"
                      decorator="dataclass(order=True)"> = {
     foo: int;
     bar: string;
   }

The generated Python class will start like this:

.. code-block:: python

   @deco.deco1
   @deco.deco2(42)
   @dataclass(order=True)
   @dataclass
   class Thing:
       ...

If extra class decorators are specifed on a sum type, the python classes generated
for the constructors of the sum type will also have the extra class decorators.
