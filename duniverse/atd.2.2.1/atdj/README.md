Atdj
====

Atdj is a program that generates a Java interface from type definitions.
In particular, given a set of ATD type definitions,
this tool generates a set of Java classes representing those types
with built-in JSON serializers and deserializers.

The primary benefits of using the generated interface, over manually
manipulating JSON strings from within Java, are safety and ease of use.
Specifically, the generated interface offers the following features:

* JSON strings are automatically checked for correctness with
  respect to the ATD specification.
* Details such as optional fields and their associated default values are
  automatically handled.
