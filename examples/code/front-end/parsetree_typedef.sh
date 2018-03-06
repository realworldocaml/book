  $ ocamlc -dparsetree typedef.ml 2>&1
  [
    structure_item (typedef.ml[1,0+0]..[1,0+18])
      Pstr_type Rec
      [
        type_declaration "t" (typedef.ml[1,0+5]..[1,0+6]) (typedef.ml[1,0+0]..[1,0+18])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ptype_variant
              [
                (typedef.ml[1,0+9]..[1,0+12])
                  "Foo" (typedef.ml[1,0+9]..[1,0+12])
                  []
                  None
                (typedef.ml[1,0+13]..[1,0+18])
                  "Bar" (typedef.ml[1,0+15]..[1,0+18])
                  []
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (typedef.ml[2,19+0]..[2,19+11])
      Pstr_value Nonrec
      [
        <def>
          pattern (typedef.ml[2,19+4]..[2,19+5])
            Ppat_var "v" (typedef.ml[2,19+4]..[2,19+5])
          expression (typedef.ml[2,19+8]..[2,19+11])
            Pexp_construct "Foo" (typedef.ml[2,19+8]..[2,19+11])
            None
      ]
  ]
  
