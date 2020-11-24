***************************
Java/Json support with atdj
***************************

The ATDJ tool generates a Java interface from an ATD interface. In
particular, given a set of ATD types, this tool generates a set of Java
classes representing those types. These classes may then be instantiated
from JSON representations of those same ATD types.

The primary benefits of using the generated interface, over manually
manipulating JSON strings from within Java, are safety and ease of use.
Specifically, the generated interface offers the following features:

-  JSON strings are automatically checked for correctness with respect
   to the ATD specificion.

-  Details such as optional fields and their associated default values
   are automatically handled.

-  Several utility methods are included “for free”. These support
   equality testing, the visitor pattern and conversion back to JSON.

Installation
============

Build and install the ``atdj`` command with `opam <https://opam.ocaml.org/>`__:

  ::

       opam install atdj

Quick-start
===========

In this section we briefly describe how to to generate a Java interface
from an example ATD file ``test.atd``. We then show how to build and run
an example application ``AtdjTest`` that uses the generated interface.

#. Generate and compile the interface:

   ::

       atdj -graph -package com.mylife.test test.atd
       export CLASSPATH='.:json.jar'
       javac com/mylife/test/*.java

#. Compile and run the example, saving the output for later inspection:

   ::

       javac AtdjTest.java
       java AtdjTest >test.out

#. Optionally, generate Javadoc documentation:

   ::

         javadoc -d doc -public com.mylife.test

   The resulting documentation is located in the directory ``doc``.

#. Optionally, generate a class graph of the generated interface:

   ::

       dot -Tpdf test.dot >test.pdf

The output file ``test.pdf`` contains a class graph of the generated
Java interface. The required ``dot`` program is part of the Graphviz
graph visualisation package, and may be downloaded from
http://www.graphviz.org/.

In the following sections we discuss the individual steps in more
detail, using the example from above.

Generating the interface
========================

In this section we describe the process of generating a Java interface
from an ATD specification.

A Java interface is generated from an ATD file as

::

    atdj -package <package> <atd_file>

This outputs a set of Java source files. The ``-package`` option causes
the resulting classes to be members of the specified package, and also
to be located in the corresponding output directory. If no package is
specified, then the default package of ``out`` is used.

For example, the command

::

    atdj -graph -package com.mylife.test test.atd

causes the generated files to be members of the package
``com.mylife.test`` and to be located in the directory
``com/mylife/test``.

The generated source files reference various members of the included
org.json package. Therefore, in order to compile the generated files,
the ``org.json`` package must be located within the Java classpath.
Supposing that the ``org.json`` package is located within the archive
``json.jar`` within the current directory, it is sufficient to set the
classpath as follows:

::

    export CLASSPATH='json.jar'

Returning to our example, the generated source files may then be
compiled as:

::

    javac com/mylife/test/*.java

Generating Javadoc documentation
================================

The generated Java code contains embedded Javadoc comments. These may be
extracted to produce Javadoc documentation. In the case of our example,
it is sufficient to run the following command:

::

      javadoc -d doc/example -public com.mylife.test

Generating a class graph
========================

We now discuss the ``-graph`` option of ATDJ. When enabled, this causes
ATDJ to output a graph of the class hierarchy of the generated code. The
output is intended to document the generated code, helping users to
avoid consulting the source code.

Continuing with our example, the use of this option results in the
generation of an additional output file named ``test.dot``. Assuming
that the ``dot`` program is installed, a PDF class graph named
``test.pdf`` can then created by running the command

::

    dot -Tpdf test.dot >test.pdf

In the generated class graph, rectangular and oval nodes correspond to
classes and interfaces, respectively. Field names are specified in the
second line of retangular (class) nodes. Solid arcs denote subtyping
(``implements``/``extends``), whilst dashed arcs link fields to their
types.

Translation reference
=====================

In this section we informally define how Java types are generated from
ATD types.

Bools, ints, floats, string, lists
----------------------------------

+---------------+------------------+
| ATD type, t   | Java type, <t>   |
+===============+==================+
| bool          | boolean          |
+---------------+------------------+
| int           | int              |
+---------------+------------------+
| float         | double           |
+---------------+------------------+
| string        | String           |
+---------------+------------------+
| t list        | <t>[]            |
+---------------+------------------+

Options
-------

Suppose that we have ATD type ``t option``. Then this is translated into
the following Java reference type:

::

    public class CNAME implements Atdj {
      // Constructor
      public CNAME(String s) throws JSONException { ... }

    // Get the optional value, if present
    public CNAME get() throws JSONException     { ... }

    // Comparison and equality
    public int     compareTo(CNAME that)        { ... }
    public boolean equals(CNAME that)           { ... }

    public <t> value;           // The value
    public boolean is_set;      // Whether the value is set
    }

Records
-------

Suppose that we have the ATD record type

::

    { f_1: t_1
    ;  ...
    ; f_n: t_n
    }

Then this is translated into the following Java reference type:

::

    public class CNAME implements Atdj {
      // Constructor
      public CNAME(String s) throws JSONException { ... }

    // Comparison and equality
    public int     compareTo(CNAME that)        { ... }
    public boolean equals(CNAME that)           { ... }

    // The individual fields
    public <t_1> f_1;
    ...
    public <t_n> f_n;
    }

An optional field ``~f_i: t_i`` causes the class field ``f_i`` to be
given a default value of type ``<t_i>`` if the field is absent from the
JSON string used to instantiate the class. The default values are as
follows:

+------------+---------------------------------------+
| ATD type   | Default Java value                    |
+============+=======================================+
| bool       | false                                 |
+------------+---------------------------------------+
| int        | 0                                     |
+------------+---------------------------------------+
| float      | 0.0                                   |
+------------+---------------------------------------+
| string     | “”                                    |
+------------+---------------------------------------+
| t list     | Empty array                           |
+------------+---------------------------------------+
| t option   | Optional value with is\_set = false   |
+------------+---------------------------------------+

Default values cannot be defined for record and sum types.

An optional field ``?f_i: t_i option`` has the same default behaviour as
above, with the additional behaviour that if the field is present in the
JSON string then the value must be of type <t> (not <t> option); the
value is then automatically lifted into a <t> option, with is\_set =
true.

Sums
----

Suppose that we have the ATD sum type

::

    [ C_1 of t_1
    | ...
    | C_n of t_n
    ]

Then this is translated into the following Java reference types:

::

    public interface IFCNAME extends Atdj {
      public int     compareTo(IFCNAME that);
      public boolean equals(IFCNAME that);
      ...
    }

::

    public class CNAME_i implements IFCNAME, Atdj {
      // Comparison and equality
      public int     compareTo(CNAME that)        { ... }
      public boolean equals(CNAME that)           { ... }

    public <t_i> value;
    }

The value field is absent if the constructor C\_i has no argument.

The Atdj and Visitor interfaces
-------------------------------

All generated reference types additionally implement the interface

::

    interface Atdj {
      String toString();
      String toString(int indent);
      int hashCode();
      Visitor accept(Visitor v);
    }

where the Visitor interface is defined as

::

    public interface Visitor {
      public void visit(CNAME_1 value);
      ...
      public void visit(CNAME_n value);
    }

for generated reference types ``CNAME``\ \_i. Visit methods for
primitive and optional primitive types are omitted.
