Atds
====

Atds is a program that generates a Scala interface from type definitions.
In particular, given a set of ATD type definitions,
this tool generates a set of Scala case classes representing those types
with built-in JSON serializers. (Deserializers planned.)

The primary benefits of using the generated interface, over using a 
JSON library and writing case classes directly, are safety and ease of use
when the same interface is needed in other languages.

Don't repeat yourself when defining a JSON interface in multiple languages. 
Instead generate compatible interfaces for all languages from a single definition.

Even if you're only using Scala, you may find the ATD definitions more concise
and less boilerplaity than the equivalent Scala types. (Especially for sum types)
