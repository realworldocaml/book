  $ dune build test.exe
        menhir parser.{ml,mli}
  Warning: you are using the standard library and/or the %inline keyword. We
  recommend switching on --infer in order to avoid obscure type error messages.
  $ ./_build/default/test.exe test1.json
  true
  false
  null
  [1, 2, 3.000000, 4.000000, 0.500000, 550000.000000, 6.300000]
  "Hello World"
  { "field1": "Hello",
    "field2": 170000000000000.000000,
    "field3": [1, 2, 3],
    "field4": { "fieldA": 1,
    "fieldB": "Hello" } }
