let json_value =
  `Assoc
    [ ("null", `Null)
    ; ("bool", `Bool true)
    ; ("int", `Int 0)
    ; ("intlit", `Intlit "10000000000000000000")
    ; ("float", `Float 0.)
    ; ("string", `String "string")
    ; ("list", `List [`Int 0; `Int 1; `Int 2])
    ]

let json_string =
  "{"
  ^ {|"null":null,|}
  ^ {|"bool":true,|}
  ^ {|"int":0,|}
  ^ {|"intlit":10000000000000000000,|}
  ^ {|"float":0.0,|}
  ^ {|"string":"string",|}
  ^ {|"list":[0,1,2]|}
  ^ "}"
