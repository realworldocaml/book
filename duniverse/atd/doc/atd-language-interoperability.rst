=================================
Interoperability with other tools
=================================

JSON Schema
-----------

ATD type definitions can be translated to
`JSON Schema <https://json-schema.org/>`_
with ``atdcat``. The user must specify the main type on the command
line since ATD doesn't have a notion of main type or root type.
This can be useful for target languages that are not yet supported by
ATD or for educational purposes.

Example
^^^^^^^

Input: ATD file ``message.atd``::

  type msg = {
    subject: string;
    ?body: string option;
    ~attachments: attachment list;
  }

  type attachment = [
    | Image of string
    | Virus
  ]

Conversion to JSON Schema::

  $ atdcat -jsonschema msg message.atd -o message.schema.json

Output: JSON Schema file ``message.schema.json``::

  {
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "description": "Translated by atdcat from 'message.atd'",
    "type": "object",
    "required": [ "subject" ],
    "properties": {
      "subject": { "type": "string" },
      "body": { "type": "string" },
      "attachments": {
        "type": "array",
        "items": { "$ref": "#/definitions/attachment" }
      }
    },
    "definitions": {
      "attachment": {
        "oneOf": [
          {
            "type": "array",
            "minItems": 2,
            "items": false,
            "prefixItems": [ { "const": "Image" }, { "type": "string" } ]
          },
          { "const": "Virus" }
        ]
      }
    }
  }

The ``jsonschema`` tool (Python implementation) can validate JSON data
using the JSON Schema file that we generated. For example, passing an
empty object ``{}`` correctly results in an error telling us the ``subject``
field is missing::

  $ jsonschema message.json -i <(echo '{}')
  {}: 'subject' is a required property

With valid JSON input such as
``{"subject": "hello", "attachments": ["Virus"]}``, the command
exits successfully and silently::

  $ jsonschema message.json -i <(echo '{"subject": "hello", "attachments": ["Virus"]}')
