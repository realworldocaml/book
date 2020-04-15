#use "topfind";;
#require "base";;
#load "ppxlib_metaquot_lifters.cmo";;
#load "ppxlib_metaquot.cmo";;

open Ppxlib

module Ast = Ast_builder.Default
[%%expect{|
module Ast = Ppxlib.Ast_builder.Default
|}]

let quoter = Quoter.create ();;
[%%expect{|
val quoter : Quoter.t = <abstr>
|}]

#install_printer Pprintast.expression;;

let expr1 =
  Ast.evar "foo" ~loc:Location.none
  |> Quoter.quote quoter
[%%expect{|
val expr1 : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_ident
     {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "__0";
      loc =
       {Ppxlib__.Import.loc_start =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppxlib__.Import.loc_start =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_loc_stack = []; pexp_attributes = []}
|}]

Pprintast.string_of_expression expr1;;
[%%expect{|
- : string = "__0"
|}]

let expr2 =
  Ast_builder.Default.evar ~loc:Location.none "bar"
  |> Quoter.quote quoter
[%%expect{|
val expr2 : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_ident
     {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "__1";
      loc =
       {Ppxlib__.Import.loc_start =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppxlib__.Import.loc_start =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_loc_stack = []; pexp_attributes = []}
|}]

let quoted =
  let expr = Ast.elist ~loc:Location.none [expr1; expr2] in
  Quoter.sanitize quoter expr
[%%expect{|
val quoted : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_let (Ppxlib__.Import.Recursive,
     [{Ppxlib__.Import.pvb_pat =
        {Ppxlib__.Import.ppat_desc =
          Ppxlib__.Import.Ppat_var
           {Ppxlib__.Import.txt = "__1";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_loc_stack = []; ppat_attributes = []};
       pvb_expr =
        {Ppxlib__.Import.pexp_desc =
          Ppxlib__.Import.Pexp_fun (Ppxlib__.Import.Nolabel, None,
           {Ppxlib__.Import.ppat_desc =
             Ppxlib__.Import.Ppat_construct
              ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "()";
                loc =
                 {Ppxlib__.Import.loc_start =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            ppat_loc_stack = []; ppat_attributes = []},
           {Ppxlib__.Import.pexp_desc =
             Ppxlib__.Import.Pexp_ident
              {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "bar";
               loc =
                {Ppxlib__.Import.loc_start =
                  {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_end =
                  {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            pexp_loc_stack = []; pexp_attributes = []});
         pexp_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_loc_stack = []; pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppxlib__.Import.loc_start =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}};
      {Ppxlib__.Import.pvb_pat =
        {Ppxlib__.Import.ppat_desc =
          Ppxlib__.Import.Ppat_var
           {Ppxlib__.Import.txt = "__0";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_loc_stack = []; ppat_attributes = []};
       pvb_expr =
        {Ppxlib__.Import.pexp_desc =
          Ppxlib__.Import.Pexp_fun (Ppxlib__.Import.Nolabel, None,
           {Ppxlib__.Import.ppat_desc =
             Ppxlib__.Import.Ppat_construct
              ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "()";
                loc =
                 {Ppxlib__.Import.loc_start =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            ppat_loc_stack = []; ppat_attributes = []},
           {Ppxlib__.Import.pexp_desc =
             Ppxlib__.Import.Pexp_ident
              {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "foo";
               loc =
                {Ppxlib__.Import.loc_start =
                  {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_end =
                  {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            pexp_loc_stack = []; pexp_attributes = []});
         pexp_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_loc_stack = []; pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppxlib__.Import.loc_start =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}}],
     {Ppxlib__.Import.pexp_desc =
       Ppxlib__.Import.Pexp_construct
        ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "::";
          loc =
           {Ppxlib__.Import.loc_start =
             {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
              pos_bol = 0; pos_cnum = -1};
            loc_end =
             {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
              pos_bol = 0; pos_cnum = -1};
            loc_ghost = true}},
        Some
         {Ppxlib__.Import.pexp_desc =
           Ppxlib__.Import.Pexp_tuple
            [{Ppxlib__.Import.pexp_desc = Ppxlib__.Import.Pexp_ident ...;
               pexp_loc = ...; pexp_loc_stack = ...; pexp_attributes = ...};
              ...];
           pexp_loc = ...; pexp_loc_stack = ...; pexp_attributes = ...});
       pexp_loc = ...; pexp_loc_stack = ...; pexp_attributes = ...});
    pexp_loc = ...; pexp_loc_stack = ...; pexp_attributes = ...}
|}]

Pprintast.string_of_expression quoted;;
[%%expect{|
- : string = "let rec __1 () = bar\nand __0 () = foo in [__0; __1]"
|}]
