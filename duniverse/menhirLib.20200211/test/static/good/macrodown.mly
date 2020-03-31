%{
  open Types

  type literal_reading_state = Normal | ReadingSpace


  (* untyped_abstract_tree -> code_range *)
  let get_range utast =
    let (rng, _) = utast in rng

  let rec append_argument_list arglsta arglstb =
    match arglsta with
    | UTEndOfArgument              -> arglstb
    | UTArgumentCons(arg, arglstl) ->
        UTArgumentCons(arg, (append_argument_list arglstl arglstb))

  (* ctrlseq_name -> untyped_abstract_tree -> untyped_abstract_tree -> untyped_argument_cons -> untyped_abstract_tree *)
  let rec convert_into_apply csutast clsnmutast idnmutast argcons =
    convert_into_apply_sub argcons ((-12, 0, 0, 0), UTApplyClassAndID(clsnmutast, idnmutast, csutast))

  (* argument_cons -> untyped_abstract_tree -> untyped_abstract_tree *)
  and convert_into_apply_sub argcons utastconstr =
    match argcons with
    | UTEndOfArgument             -> utastconstr
    | UTArgumentCons(arg, actail) ->
        convert_into_apply_sub actail (get_range arg, UTApply(utastconstr, arg))

  let class_name_to_abstract_tree clsnm =
    UTStringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  let id_name_to_abstract_tree idnm =
    UTStringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  let rec curry_lambda_abstract rng argvarcons utastdef =
    match argvarcons with
    | UTEndOfArgumentVariable -> utastdef
    | UTArgumentVariableCons(varrng, argvar, avtail) ->
        (rng, UTLambdaAbstract(varrng, argvar, curry_lambda_abstract (-11, 0, 0, 0) avtail utastdef))

  let error_reporting msg disp pos =
    let (pos_ln, pos_start, pos_end) = pos in
      "Syntax error: " ^ msg ^ ".\n\n    " ^ disp
        ^ "\n\n  (at line " ^ (string_of_int pos_ln) ^ ", "
        ^ (string_of_int pos_start) ^ "-" ^ (string_of_int pos_end) ^ ")"

  let rec string_of_avc argvarcons =
    match argvarcons with
    | UTEndOfArgumentVariable                   -> ""
    | UTArgumentVariableCons(_, argvar, avtail) -> argvar ^ " " ^ (string_of_avc avtail)

  let rec stringify_literal ltrl =
    let (_, ltrlmain) = ltrl in
      match ltrlmain with
      | UTConcat(utastf, utastl) -> (stringify_literal utastf) ^ (stringify_literal utastl)
      | UTStringConstant(s)      -> s
      | UTStringEmpty            -> ""
      | _  -> raise (ParseErrorDetail("illegal token in literal area; this cannot happen"))

  let rec omit_pre_spaces str =
    if String.sub str 0 1 = " "  then
      omit_pre_spaces (String.sub str 1 ((String.length str) - 1))
(*    else if String.sub str 0 1 = "\n" then
      (String.sub str 1 ((String.length str) - 1)) *)
    else
      str

  let rec omit_post_spaces str =
    if String.sub str ((String.length str) - 1) 1 = " " then
      omit_post_spaces (String.sub str 0 ((String.length str) - 1))
    else if String.sub str ((String.length str) - 1) 1 = "\n" then
      (String.sub str 0 ((String.length str) - 1))
    else
      str

  (* untyped_abstract_tree -> untyped_abstract_tree_main *)
  let rec omit_spaces ltrl =
    let str_ltrl = omit_post_spaces (omit_pre_spaces (stringify_literal ltrl)) in
      let min_indent = min_indent_space str_ltrl in
        let str_shaved = shave_indent str_ltrl min_indent in
          if str_shaved.[(String.length str_shaved) - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 ((String.length str_shaved) - 1) in
              UTConcat(
                ((-13, 0, 0, 0), UTStringConstant(str_no_last_break)),
                ((-14, 0, 0, 0), UTBreakAndIndent)
              )
          else
            UTStringConstant(str_shaved)

  (* string -> int *)
  and min_indent_space str_ltrl =
    min_indent_space_sub str_ltrl 0 ReadingSpace 0 (String.length str_ltrl)

  (* string -> int -> literal_reading_state -> int -> int -> int *)
  and min_indent_space_sub str_ltrl index lrstate spnum minspnum =
    if index >= (String.length str_ltrl) then
      (* ( print_string ("min_indent: " ^ (string_of_int minspnum) ^ "\n") ; *)
        minspnum
      (* ) *)
    else
      match lrstate with
      | Normal ->
          ( match str_ltrl.[index] with
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 minspnum
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' '  -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace (spnum + 1) minspnum
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
                (* does not take space-only line into account *)
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 (if spnum < minspnum then spnum else minspnum)
          )

  and shave_indent str_ltrl minspnum =
    shave_indent_sub str_ltrl minspnum 0 "" Normal 0

  and shave_indent_sub str_ltrl minspnum index str_constr lrstate spnum =
    if index >= (String.length str_ltrl) then
      str_constr
    else
      match lrstate with
      | Normal ->
          ( match str_ltrl.[index] with
            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' ' ->
                if spnum < minspnum then
                  shave_indent_sub str_ltrl minspnum (index + 1) str_constr ReadingSpace (spnum + 1)
                else
                  shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ " ") ReadingSpace (spnum + 1)

            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )

  let binary_operator opname lft op rgt =
    let (sttln, sttpos, _, _) = get_range lft in
    let (_, _, endln, endpos) = get_range rgt in
    let (opln, opstt, opend) = op in
    let oprng = (opln, opstt, opln, opend) in
    let rng = (sttln, sttpos, endln, endpos) in
      (rng, UTApply(((-15, 0, 0, 0), UTApply((oprng, UTContentOf(opname)), lft)), rgt))

  let make_let_expression lettk vartk utargcons utastdef decs utastaft =
    let (sttln, sttpos, _) = lettk in
    let ((varln, varstt, varend), varnm) = vartk in
    let (_, _, endln, endpos) = get_range utastaft in
    let rng = (sttln, sttpos, endln, endpos) in
    let varrng = (varln, varstt, varln, varend) in
    let curried = curry_lambda_abstract varrng utargcons utastdef in
      (rng, UTLetIn(UTMutualLetCons(varnm, curried, decs), utastaft))

  let make_let_mutable_expression letmuttk vartk utastdef utastaft =
    let (sttln, sttpos, _) = letmuttk in
    let ((varln, varstt, varend), vn) = vartk in
    let (_, _, endln, endpos) = get_range utastaft in
    let varrng = (varln, varstt, varln, varend) in
    let rng = (sttln, sttpos, endln, endpos) in
      (rng, UTLetMutableIn(varrng, vn, utastdef, utastaft))

%}

%token <Types.token_position * Types.var_name> VAR
%token <Types.token_position * Types.var_name> VARINSTR
%token <Types.token_position * string> NUMCONST
%token <Types.token_position * string> CHAR
%token <Types.token_position> SPACE
%token <Types.token_position> BREAK
%token <Types.token_position * Types.ctrlseq_name> CTRLSEQ
%token <Types.token_position * Types.id_name> IDNAME
%token <Types.token_position * Types.class_name> CLASSNAME
%token <Types.token_position> END
%token <Types.token_position> LAMBDA ARROW
%token <Types.token_position> LET DEFEQ LETAND IN
%token <Types.token_position> LETMUTABLE OVERWRITEEQ
%token <Types.token_position> REFNOW REFFINAL
%token <Types.token_position> IF THEN ELSE IFCLASSISVALID IFIDISVALID
%token <Types.token_position> LPAREN
%token <Types.token_position> RPAREN
%token <Types.token_position> TIMES DIVIDES
%token <Types.token_position> MOD
%token <Types.token_position> PLUS MINUS
%token <Types.token_position> EQ NEQ GEQ LEQ GT LT
%token <Types.token_position> LNOT
%token <Types.token_position> LAND
%token <Types.token_position> LOR
%token <Types.token_position> CONCAT
%token <Types.token_position> OPENQT CLOSEQT
%token <Types.token_position> OPENSTR CLOSESTR
%token <Types.token_position> OPENNUM CLOSENUM
%token <Types.token_position> BGRP EGRP
%token <Types.token_position> TRUE FALSE
%token <Types.token_position> FINISH
%token <Types.token_position> SEP
%token <Types.token_position> BLIST LISTPUNCT ELIST CONS
%token <Types.token_position> BEFORE
%token <Types.token_position> UNITVALUE
%token <Types.token_position> WHILE DO
%token <Types.token_position> DECGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
%token <Types.token_position> MUTUAL ENDMUTUAL
%token EOI
%token IGNORED

%nonassoc LET DEFEQ IN LETAND LETMUTABLE OVERWRITEEQ
%nonassoc IF THEN ELSE
%left OVERWRITEGLOBALHASH
%left BEFORE
%nonassoc WHILE
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%left PLUS
%right MINUS
%left TIMES
%right MOD DIVIDES
%nonassoc VAR
%nonassoc LPAREN RPAREN

%start main
%type <Types.untyped_abstract_tree> main
%type <Types.untyped_abstract_tree> nxlet
%type <Types.untyped_mutual_let_cons> nxdec
%type <Types.untyped_abstract_tree> nxbfr
%type <Types.untyped_abstract_tree> nxwhl
%type <Types.untyped_abstract_tree> nxif
%type <Types.untyped_abstract_tree> nxlor
%type <Types.untyped_abstract_tree> nxland
%type <Types.untyped_abstract_tree> nxcomp
%type <Types.untyped_abstract_tree> nxconcat
%type <Types.untyped_abstract_tree> nxlplus
%type <Types.untyped_abstract_tree> nxltimes
%type <Types.untyped_abstract_tree> nxrplus
%type <Types.untyped_abstract_tree> nxrtimes
%type <Types.untyped_abstract_tree> nxun
%type <Types.untyped_abstract_tree> nxapp
%type <Types.untyped_abstract_tree> nxbot
%type <Types.untyped_abstract_tree> nxlist
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxsepsub
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> sxbot
%type <Types.untyped_abstract_tree> sxclsnm
%type <Types.untyped_abstract_tree> sxidnm
%type <Types.untyped_argument_cons> narg
%type <Types.untyped_argument_cons> sarg
%type <Types.untyped_argument_cons> sargsub
%type <Types.untyped_argument_variable_cons> argvar
%type <string> binop

%%


main:
  | nxtoplevel EOI { $1 }
  | sxblock EOI { $1 }
;
nxtoplevel:
/* ---- toplevel style ---- */
  | LET VAR     argvar DEFEQ nxlet nxdec nxtoplevel { make_let_expression $1 $2 $3 $5 $6 $7 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec nxtoplevel { make_let_expression $1 $2 $3 $5 $6 $7 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet nxtoplevel     { make_let_mutable_expression $1 $2 $4 $5 }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual nxtoplevel { make_let_expression $2 $3 $4 $6 $7 $8 }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual nxtoplevel { make_let_expression $2 $3 $4 $6 $7 $8 }
/* ---- toplevel terminal ---- */
  | LET VAR argvar DEFEQ nxlet nxdec {
        make_let_expression $1 $2 $3 $5 $6 ((-256, 0, 0, 0), UTFinishHeaderFile)
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec {
        make_let_expression $1 $2 $3 $5 $6 ((-257, 0, 0, 0), UTFinishHeaderFile)
      }
  | LETMUTABLE VAR OVERWRITEEQ nxlet {
        make_let_mutable_expression $1 $2 $4 ((-258, 0, 0, 0), UTFinishHeaderFile)
      }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual {
        make_let_expression $2 $3 $4 $6 $7 ((-259, 0, 0, 0), UTFinishHeaderFile)
      }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual {
        make_let_expression $2 $3 $4 $6 $7 ((-259, 0, 0, 0), UTFinishHeaderFile)
      }
/* ---- transition to expression style ---- */
  | LET VAR     argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet     { make_let_mutable_expression $1 $2 $4 $6 }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual IN nxlet { make_let_expression $2 $3 $4 $6 $7 $9 }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual IN nxlet { make_let_expression $2 $3 $4 $6 $7 $9 }
/* ---- for syntax error log ---- */
  | LET error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'let'" "let ..<!>.." $1))
      }
  | LET VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'let'"
            ("let" ^ vn ^ " ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ..<!>..") $1))
      }
  | LET CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ csname ^ " ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ " = ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") ln))
      }
  | LETMUTABLE error {
        raise (ParseErrorDetail(error_reporting "missing identifier after 'let-mutable'"
          "let-mutable ..<!>.." $1))
      }
  | LETMUTABLE VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting ("missing '<-' after '" ^ vn ^ "'")
            ("let-mutable " ^ vn ^ " ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '<-'"
            ("let-mutable " ^ vn ^ " <- ..<!>..") $1))
      }
;
nxmutual: /* -> Types.untyped_mutual_let_cons */
  | LET VAR argvar DEFEQ nxlet nxmutual {
        let ((varln, varstt, varend), vn) = $2 in
        let varrng = (varln, varstt, varln, varend) in
        let curried = curry_lambda_abstract varrng $3 $5 in
          UTMutualLetCons(vn, curried, $6)
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxmutual {
        let ((csln, csstt, csend), cn) = $2 in
        let csrng = (csln, csstt, csln, csend) in
        let curried = curry_lambda_abstract csrng $3 $5 in
          UTMutualLetCons(cn, curried, $6)
      }
  | ENDMUTUAL { UTEndOfMutualLet }
;
nxlet:
  | LET VAR argvar DEFEQ nxlet nxdec IN nxlet     { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet     { make_let_mutable_expression $1 $2 $4 $6 }
  | nxwhl { $1 }
/* -- for syntax error log -- */
  | LET error {
        raise (ParseErrorDetail(error_reporting
          "missing identifier after 'let'" "let ..<!>.." $1))
      }
  | LET VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ vn ^ " ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ nxlet nxdec IN error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") $1))
      }
  | LET CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ csname ^ " ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ " = ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") ln))
      }
  | LETMUTABLE error {
        raise (ParseErrorDetail(error_reporting "missing identifier after 'let-mutable'"
          "let-mutable ..<!>.." $1))
      }
  | LETMUTABLE VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting ("missing '<-' after '" ^ vn ^ "'")
            ("let-mutable " ^ vn ^ " ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '<-'"
            ("let-mutable " ^ vn ^ " <- ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
          "in ..<!>.." $5))
      }
;
nxdec: /* -> Types.mutual_let_cons */
  | LETAND VAR argvar DEFEQ nxlet nxdec {
        let ((varln, varstt, varend), vn) = $2 in
        let varrng = (varln, varstt, varln, varend) in
        let curried = curry_lambda_abstract varrng $3 $5 in
          UTMutualLetCons(vn, curried, $6)
      }
  | LETAND CTRLSEQ argvar DEFEQ nxlet nxdec {
        let ((csln, csstt, csend), csname) = $2 in
        let csrng = (csln, csstt, csln, csend) in
        let curried = curry_lambda_abstract csrng $3 $5 in
          UTMutualLetCons(csname, curried, $6)
      }
  | { UTEndOfMutualLet }
/* -- for syntax error log -- */
  | LETAND VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ vn ^ " ..<!>..") $1))
      }
  | LETAND CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ csname ^ " ..<!>..") ln))
      }
;
nxwhl:
  | WHILE nxlet DO nxwhl {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTWhileDo($2, $4))
      }
  | nxif { $1 }
/* -- for syntax error log --*/
  | WHILE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'while'" "while ..<!>.." $1))
      }
  | WHILE nxlet DO error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'do'" "do ..<!>.." $3))
      }
nxif:
  | IF nxlet THEN nxlet ELSE nxlet {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $6 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTIfThenElse($2, $4, $6))
      }
  | IFCLASSISVALID nxlet ELSE nxlet {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTIfClassIsValid($2, $4))
      }
  | IFCLASSISVALID THEN nxlet ELSE nxlet {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $5 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTIfClassIsValid($3, $5))
      }
  | IFIDISVALID nxlet ELSE nxlet {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTIfIDIsValid($2, $4))
      }
  | IFIDISVALID THEN nxlet ELSE nxlet {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $5 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTIfIDIsValid($3, $5))
      }
  | nxbfr { $1 }
/* -- for syntax error log -- */
  | IF error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if'" "if ..<!>.." $1))
      }
  | IF nxlet THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $3))
      }
  | IF nxlet THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $5))
      }
  | IFCLASSISVALID error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if-class-is-valid'" "if-class-is-valid ..<!>.." $1))
      }
  | IFCLASSISVALID nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $3))
      }
  | IFCLASSISVALID THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $2))
      }
  | IFCLASSISVALID THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $4))
      }
  | IFIDISVALID error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if-id-is-valid'" "if-id-is-valid ..<!>.." $1))
      }
  | IFIDISVALID nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $3))
      }
  | IFIDISVALID THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $2))
      }
  | IFIDISVALID THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $4))
      }
;
nxbfr:
  | nxlambda BEFORE nxbfr {
        let (sttln, sttpos, _, _) = get_range $1 in
        let (_, _, endln, endpos) = get_range $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTSequential($1, $3))
      }
  | nxlambda { $1 }
/* -- for syntax error log -- */
  | nxlambda BEFORE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'before'" "before ..<!>.." $2))
      }
;
nxlambda:
  | VAR OVERWRITEEQ nxlor {
        let ((sttln, sttpos, varend), vn) = $1 in
        let (_, _, endln, endpos) = get_range $3 in
        let varrng = (sttln, sttpos, sttln, varend) in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTOverwrite(varrng, vn, $3))
      }
  | DECGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTDeclareGlobalHash($2, $4))
      }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTOverwriteGlobalHash($2, $4))
      }
  | LAMBDA argvar ARROW nxlor {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          curry_lambda_abstract rng $2 $4
      }
  | nxlor { $1 }
/* -- for syntax error log -- */
  | LAMBDA error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'function'" "function ..<!>.." $1))
      }
  | LAMBDA argvar ARROW error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '->'" "-> ..<!>.." $3))
      }
  | DECGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after 'declare-global-hash'" "declare-global-hash ..<!>.." $1))
      }
  | DECGLOBALHASH nxlet OVERWRITEGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after '<<-'" "<<- ..<!>.." $3))
      }
  | RENEWGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after 'renew'" "renew ..<!>.." $1))
      }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after '<<-'" "<<- ..<!>.." $3))
      }
;
argvar: /* -> Types.argument_variable_cons */
  | VAR argvar {
        let ((varln, varstt, varend), vn) = $1 in
        let varrng = (varln, varstt, varln, varend) in
          UTArgumentVariableCons(varrng, vn, $2)
      }
  | { UTEndOfArgumentVariable }
;
nxlor:
  | nxland LOR nxlor { binary_operator "||" $1 $2 $3 }
  | nxland { $1 }
/* -- for syntax error log -- */
  | nxland LOR error {
        raise (ParseErrorDetail(error_reporting "illegal token after '||'" "|| ..<!>.." $2))
      }
;
nxland:
  | nxcomp LAND nxland { binary_operator "&&" $1 $2 $3 }
  | nxcomp { $1 }
/* -- for syntax error log -- */
  | nxcomp LAND error {
        raise (ParseErrorDetail(error_reporting "illegal token after '&&'" "&& ..<!>.." $2))
      }
;
nxcomp:
  | nxconcat EQ nxcomp  { binary_operator "==" $1 $2 $3 }
  | nxconcat NEQ nxcomp { binary_operator "<>" $1 $2 $3 }
  | nxconcat GEQ nxcomp { binary_operator ">=" $1 $2 $3 }
  | nxconcat LEQ nxcomp { binary_operator "<=" $1 $2 $3 }
  | nxconcat GT nxcomp  { binary_operator ">" $1 $2 $3 }
  | nxconcat LT nxcomp  { binary_operator "<" $1 $2 $3 }
  | nxconcat { $1 }
/* -- for syntax error log -- */
  | nxconcat EQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '=='" "== ..<!>.." $2))
      }
  | nxconcat NEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<>'" "<> ..<!>.." $2))
      }
  | nxconcat GEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '>='" ">= ..<!>.." $2))
      }
  | nxconcat LEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<='" "<= ..<!>.." $2))
      }
  | nxconcat GT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '>'" "> ..<!>.." $2))
      }
  | nxconcat LT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<'" "< ..<!>.." $2))
      }
;
nxconcat:
  | nxlplus CONCAT nxconcat { binary_operator "^" $1 $2 $3 }
  | nxlplus CONS nxconcat   { binary_operator "::" $1 $2 $3 }
  | nxlplus { $1 }
/* -- for syntax error log -- */
  | nxlplus CONCAT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '^'" "^ ..<!>.." $2))
      }
;
nxlplus:
  | nxlminus PLUS nxrplus { binary_operator "+" $1 $2 $3 }
  | nxlminus { $1 }
/* -- for syntax error log -- */
  | nxlminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxlminus:
  | nxlplus MINUS nxrtimes { binary_operator "-" $1 $2 $3 }
  | nxltimes { $1 }
/* -- for syntax error log -- */
  | nxlplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxrplus:
  | nxrminus PLUS nxrplus { binary_operator "+" $1 $2 $3 }
  | nxrminus { $1 }
/* -- for syntax error log -- */
  | nxrminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxrminus:
  | nxrplus MINUS nxrtimes { binary_operator "+" $1 $2 $3 }
  | nxrtimes { $1 }
/* -- for syntax error log -- */
  | nxrplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxltimes:
  | nxun TIMES nxrtimes    { binary_operator "*" $1 $2 $3 }
  | nxltimes DIVIDES nxapp { binary_operator "/" $1 $2 $3 }
  | nxltimes MOD nxapp     { binary_operator "mod" $1 $2 $3 }
  | nxun { $1 }
/* -- for syntax error log -- */
  | nxun TIMES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '*'" "* ..<!>.." $2))
      }
  | nxltimes DIVIDES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '/'" "/ ..<!>.." $2))
      }
  | nxltimes MOD error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'mod'" "mod ..<!>.." $2))
      }
;
nxrtimes:
  | nxapp TIMES nxrtimes   { binary_operator "*" $1 $2 $3 }
  | nxrtimes DIVIDES nxapp { binary_operator "/" $1 $2 $3 }
  | nxrtimes MOD nxapp     { binary_operator "mod" $1 $2 $3 }
  | nxapp { $1 }
/* -- for syntax error log -- */
  | nxapp TIMES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '*'" "* ..<!>.." $2))
      }
  | nxrtimes DIVIDES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '/'" "/ ..<!>.." $2))
      }
  | nxrtimes MOD error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'mod'" "mod ..<!>.." $2))
      }
;
nxun:
  | MINUS nxapp { binary_operator "-" ((-16, 0, 0, 0), UTNumericConstant(0)) $1 $2 }
  | LNOT nxapp  {
        let (sttln, sttpos, lnotend) = $1 in
        let lnotrng = (sttln, sttpos, sttln, lnotend) in
        let (_, _, endln, endpos) = get_range $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTApply((lnotrng, UTContentOf("not")), $2))
      }
  | REFNOW nxapp {
        let (sttln, sttpos, refnowend) = $1 in
        let refnowrng = (sttln, sttpos, sttln, refnowend) in
        let (_, _, endln, endpos) = get_range $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTApply((refnowrng, UTContentOf("!")), $2))
      }
  | REFFINAL nxapp {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTReferenceFinal($2))
      }
  | nxapp { $1 }
/* -- for syntax error log -- */
  | MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after unary '-'" "- ..<!>.." $1))
      }
  | LNOT error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'not'" "not ..<!>.." $1))
      }
  | REFNOW error {
        raise (ParseErrorDetail(error_reporting "illegal token after '!'" "! ..<!>.." $1))
      }
  | REFFINAL error {
        raise (ParseErrorDetail(error_reporting "illegal token after '!!'" "!! ..<!>.." $1))
      }
;
nxapp:
  | nxapp nxbot {
        let (sttln, sttpos, _, _) = get_range $1 in
        let (_, _, endln, endpos) = get_range $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTApply($1, $2))
      }
  | nxbot { $1 }
;
nxbot:
  | VAR {
        let ((varln, varstt, varend), vn) = $1 in
        let rng = (varln, varstt, varln, varend) in
          (rng, UTContentOf(vn))
      }
  | NUMCONST {
        let ((ncln, ncstt, ncend), cs) = $1 in
        let rng = (ncln, ncstt, ncln, ncend) in
          (rng, UTNumericConstant(int_of_string cs))
      }
  | TRUE  {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTBooleanConstant(true))
      }
  | FALSE {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTBooleanConstant(false))
      }
  | LPAREN nxlet RPAREN    {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let (_, utast) = $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, utast)
      }
  | OPENSTR sxsep CLOSESTR {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let (_, utast) = $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, utast)
      }
  | OPENQT sxsep CLOSEQT {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, omit_spaces $2)
      }
  | BLIST ELIST {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTEndOfList)
      }
  | BLIST nxlet nxlist ELIST {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $4 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTListCons($2, $3))
      }
  | UNITVALUE {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTUnitConstant)
      }
  | FINISH {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTFinishHeaderFile)
      }
  | LPAREN binop RPAREN {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTContentOf($2))
  }
/* -- for syntax error log -- */
  | BLIST error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '['" "[ ..<!>.." $1))
      }
  | OPENSTR error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after beginning of string area '{'" "{ ..<!>.." $1))
      }
  | LPAREN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '('" "( ..<!>.." $1))
      }
;
binop:
  | PLUS    { "+" }
  | MINUS   { "-" }
  | MOD     { "mod" }
  | TIMES   { "*" }
  | DIVIDES { "/" }
  | CONCAT  { "^" }
  | EQ      { "==" }
  | NEQ     { "<>" }
  | GEQ     { ">=" }
  | LEQ     { "<=" }
  | GT      { ">" }
  | LT      { "<" }
  | LAND    { "&&" }
  | LOR     { "||" }
  | LNOT    { "not" }
  | BEFORE  { "before" }
;
nxlist:
  | LISTPUNCT nxlet nxlist {
        let (sttln, sttpos, _) = $1 in
        let (_, _, endln, endpos) = get_range $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTListCons($2, $3))
      }
  | { ((-17, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | LISTPUNCT error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after ';'" "; ..<!>.." $1))
      }
;
sxsep:
  | SEP sxsepsub { $2 }
  | sxblock { $1 }
/* -- for syntax error log -- */
  | SEP error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '|'" "| ..<!>.." $1))
      }
;
sxsepsub:
  | sxblock SEP sxsepsub {
        let (sttln, sttpos, _, _) = get_range $1 in
        let (_, _, endln, endpos) = get_range $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTListCons($1, $3))
      }
  | { ((-18, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | sxblock SEP error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '|'" "| ..<!>.." $2))
      }
;
sxblock:
  | sxbot sxblock {
        let (sttln, sttpos, _, _) = get_range $1 in
        let (_, _, endln, endpos) = get_range $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTConcat($1, $2))
      }
  | { ((-19, 0, 0, 0), UTStringEmpty) }
  ;
sxbot:
  | CHAR  {
        let ((ln, sttpos, endpos), ch) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTStringConstant(ch))
      }
  | SPACE {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTStringConstant(" "))
      }
  | BREAK {
        let (ln, sttpos, endpos) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, UTBreakAndIndent)
      }
  | VARINSTR END {
        let ((sttln, sttpos, _), vn) = $1 in
        let (endln, _, endpos) = $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTContentOf(vn))
      }
  | CTRLSEQ sxclsnm sxidnm narg sarg {
        let ((csln, csstt, csend), csname) = $1 in
        let csrng = (csln, csstt, csln, csend) in
          convert_into_apply (csrng, UTContentOf(csname)) $2 $3 (append_argument_list $4 $5)
      }
/* -- for syntax error log -- */
  | CTRLSEQ error {
        let (ln, csname) = $1 in
        raise (ParseErrorDetail(error_reporting ("illegal token after '" ^ csname ^ "'") (csname ^ " ..<!>..") ln))
      }
sxclsnm:
  | CLASSNAME {
        let ((ln, sttpos, endpos), clsnm) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, class_name_to_abstract_tree clsnm)
      }
  | { ((-20, 0, 0, 0), UTNoContent) }
sxidnm:
  | IDNAME {
        let ((ln, sttpos, endpos), idnm) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          (rng, id_name_to_abstract_tree idnm)
      }
  | { ((-21, 0, 0, 0), UTNoContent) }
;
narg: /* -> Types.untyped_argument_cons */
  | OPENNUM nxlet CLOSENUM narg {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let (_, utastmain) = $2 in
        let rng = (sttln, sttpos, endln, endpos) in
          UTArgumentCons((rng, utastmain), $4)
      }
  | { UTEndOfArgument }
/* -- for syntax error log -- */
  | OPENNUM error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after beginning of program '('" "( ..<!>.." $1))
      }
  | OPENNUM nxlet CLOSENUM error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after end of program ')'" ") ..<!>.." $3))
      }
;
sarg: /* -> Types.untyped_argument_cons */
  | BGRP sxsep EGRP sargsub { UTArgumentCons($2, $4) }
  | OPENQT sxsep CLOSEQT sargsub {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | END { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let rng = (sttln, sttpos, endln, endpos) in
        let (_, utast) = $2 in
          UTArgumentCons((rng, utast), $4)
      }
  | OPENQT sxsep CLOSEQT sargsub {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $3 in
        let rng = (sttln, sttpos, endln, endpos) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
