  $ ocamlc -dtypedtree typedef.ml 2>&1
  [
    structure_item (typedef.ml[1,0+0]..typedef.ml[1,0+18])
      Tstr_type Rec
      [
        type_declaration t/1002 (typedef.ml[1,0+0]..typedef.ml[1,0+18])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (typedef.ml[1,0+9]..typedef.ml[1,0+12])
                  Foo/1003
                  []
                  None
                (typedef.ml[1,0+13]..typedef.ml[1,0+18])
                  Bar/1004
                  []
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (typedef.ml[2,19+0]..typedef.ml[2,19+11])
      Tstr_value Nonrec
      [
        <def>
          pattern (typedef.ml[2,19+4]..typedef.ml[2,19+5])
            Tpat_var "v/1005"
          expression (typedef.ml[2,19+8]..typedef.ml[2,19+11])
            Texp_construct "Foo"
            []
      ]
  ]
  
