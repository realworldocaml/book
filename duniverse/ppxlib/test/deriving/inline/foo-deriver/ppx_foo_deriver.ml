open Ppxlib

(*
   [[@@deriving foo]] expands to:
   {[
     let _ = [%foo]
   ]}

   and then [[%foo]] expands to ["foo"].
*)

let add_deriver () =
  let str_type_decl =
    Deriving.Generator.make_noarg (
      fun ~loc ~path:_ _ ->
        let expr desc : expression=
          { pexp_desc = desc;
            pexp_loc = loc;
            pexp_attributes = [];
            pexp_loc_stack = [];
          }
        in
        [
          {pstr_loc = loc;
           pstr_desc =
             (Pstr_value (Nonrecursive, [{
                pvb_pat =
                  { ppat_desc = Ppat_any;
                    ppat_loc = loc;
                    ppat_attributes = [];
                    ppat_loc_stack = [];
                  }
              ;
                pvb_expr = expr (
                  Pexp_extension ({loc; txt = "foo"}, PStr []));
                pvb_attributes = [];
                pvb_loc = loc;
              }]));
          }
        ]
    )
      ~attributes:[]
  in
  let sig_type_decl =
    Deriving.Generator.make_noarg (
      fun ~loc ~path decl ->
        ignore loc;
        ignore path;
        ignore decl;
        [
        ]
    )
  in
  Deriving.add "foo"
    ~str_type_decl
    ~sig_type_decl

let () =
  Driver.register_transformation "foo"
    ~rules:[
      Context_free.Rule.extension
        (Extension.declare "foo"
           Expression Ast_pattern.__
           (fun ~loc ~path:_ _payload ->
              { pexp_desc = Pexp_constant (Pconst_string ("foo", loc, None));
                pexp_loc = loc;
                pexp_attributes = [];
                pexp_loc_stack = [];
              }))
    ]

let (_ : Deriving.t) = add_deriver ()
