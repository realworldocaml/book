%{
(* each spec_line has a *_line parsing rule *)
type spec_line =
    | Package of Common.category * string
    | Version of string
    | Revision of int
    | Depends of Common.dep list
    | Build_Depends of Common.dep list
    | Sources of string
    | Unpacks_To of string
    | Homepage of string
    | Maintainer of string
    | Options of Common.options list
    | Docfiles of string list
    | Description of string * string
    | Confopts of Common.confopt list
    | Bytecode_Target of string
    | Opt_Target of string

(* this gets filled in by the parser *)
let base_spec =
  {
    EL.name = "";
    EL.version = "";
    EL.revision = 0;
    EL.category = `godi;
    EL.depends = [];
    EL.build_depends = [];
    EL.sources_site = "";
    EL.sources_basename = "";
    EL.sources_extension = "";
    EL.sources_unpacksto = None;
    EL.homepage = "";
    EL.maintainer = "";
    EL.short_desc = "";
    EL.long_desc = "";
    EL.options = [];
    EL.docfiles = [];
    EL.confopts = [];
    EL.all_target = None;
    EL.opt_target = None;

    EL.specfile = "";
  }

let extensions = [".tar.gz"; ".tgz"; ".tar.bz2"; ".tbz2"]

let parse_sources url =
    try
        let dirname = Filename.dirname url in
        let basename = Filename.basename url in
        let extension = List.find (Filename.check_suffix url) extensions in
        let basename' = Filename.chop_suffix basename extension in
        (dirname, basename', extension)
    with
        Not_found ->
            raise (Syntax.Error (Lexing.dummy_pos, Lexing.dummy_pos,
                                 "Invalid extension in `Sources:' line"))

(* replace a single line's worth of info in the record `spec'.
   this gets folded over `base_spec', from above *)
let replace_line spec line =
    match line with
        | Package (cat, name) -> { spec with EL.name = name; EL.category = cat }
        | Version ver -> { spec with EL.version = ver }
        | Revision rev -> { spec with EL.revision = rev }
        | Depends deps -> { spec with EL.depends = deps }
        | Build_Depends deps -> { spec with EL.build_depends = deps }
        | Sources url -> let (site, basename, extension) = parse_sources url in
                        { spec with EL.sources_site = site;
                                    EL.sources_basename = basename;
                                    EL.sources_extension = extension }
        | Unpacks_To dir -> { spec with EL.sources_unpacksto = Some dir }
        | Homepage url -> { spec with EL.homepage = url }
        | Maintainer m -> { spec with EL.maintainer = m }
        | Description (short, long) -> { spec with EL.short_desc = short;
                                                   EL.long_desc = long }
        | Options opts -> { spec with EL.options = opts }
        | Docfiles docs -> { spec with EL.docfiles = docs }
        | Confopts confs -> { spec with EL.confopts = confs }
        | Bytecode_Target t -> { spec with EL.all_target = Some t }
        | Opt_Target t -> {spec with EL.opt_target = Some t }

let missing rhs_n s =
    let pos = Parsing.rhs_end_pos rhs_n in
    raise (Syntax.Error (pos, pos, s))

let error n s =
    raise (Syntax.Error (Parsing.rhs_start_pos n, Parsing.rhs_end_pos n, s))

let sym_error s =
    raise (Syntax.Error (Parsing.symbol_start_pos (),
                         Parsing.symbol_end_pos (),
                         s))

let trail n s = error n ("Trailing junk after " ^ s)
let missing s = sym_error ("Missing " ^ s)
let invalid n s = error n ("Invalid " ^ s)
%}

%token PACKAGE
%token VERSION
%token REVISION
%token DEPENDS BUILD_DEPENDS
%token SOURCES UNPACKSTO
%token HOMEPAGE
%token MAINTAINER
%token OPTIONS
%token DOCFILES
%token DESCRIPTION
%token CONFOPTS
%token BC_TARGET OPT_TARGET
%token <string> STRING
%token <string * string> STRINGPAIR
%token <int> INT
%token <Common.relop> RELOP
%token <Common.options> OPTION
%token LPAREN RPAREN COMMA
%token EOF
%token EOL
%token <Common.category * string> PKGNAME

%start spec
%type <EL.spec> spec

%%

spec:
    | lines EOF     { let spec = List.fold_left replace_line base_spec $1 in
                      let curr_p = Parsing.symbol_end_pos () in
                      { spec with EL.specfile = curr_p.Lexing.pos_fname } }
;

lines:
    | line lines    { $1 :: $2 }
    |               { [] }
;

line:
    | package_line      { $1 }
    | version_line      { $1 }
    | revision_line     { $1 }
    | depends_line      { $1 }
    | build_depends_line { $1 }
    | sources_line      { $1 }
    | unpacksto_line    { $1 }
    | homepage_line     { $1 }
    | maintainer_line   { $1 }
    | options_line      { $1 }
    | docfiles_line     { $1 }
    | description_line  { $1 }
    | confopts_line     { $1 }
    | bytecode_target_line { $1 }
    | opt_target_line   { $1 }
;

package_line:
    | PACKAGE PKGNAME EOL   { let (c, n) = $2 in Package (c, n) }
    | PACKAGE PKGNAME error EOL { trail 3 "package name" }
    | PACKAGE EOL           { missing "package name" }
;

version_line:
    | VERSION STRING EOL    { Version $2 }
    | VERSION STRING error EOL { trail 3 "version number" }
    | VERSION EOL           { missing "version number" }
;

revision_line:
    | REVISION INT EOL      { Revision $2 }
    | REVISION INT error EOL { trail 3 "revision number" }
    | REVISION EOL           { missing "revision number" }
;

depends_line:
    | DEPENDS deps EOL      { Depends $2 }
;

build_depends_line:
    | BUILD_DEPENDS deps EOL { Build_Depends $2 }
;

sources_line:
    | SOURCES STRING EOL    { Sources $2 }
    | SOURCES STRING error EOL { trail 3 "sources url" }
    | SOURCES EOL           { missing "sources url" }
;

unpacksto_line:
    | UNPACKSTO STRING EOL  { Unpacks_To $2 }
    | UNPACKSTO STRING error EOL { trail 3 "`unpacks to' directory" }
    | UNPACKSTO EOL           { missing "`unpacks to' directory" }
;

homepage_line:
    | HOMEPAGE STRING EOL   { Homepage $2 }
    | HOMEPAGE STRING error EOL { trail 3 "homepage url" }
    | HOMEPAGE EOL           { missing "homepage url" }
;

maintainer_line:
    | MAINTAINER STRING EOL { Maintainer $2 }
    /* maintainer can't have trailing junk or be missing, since it's a *-line */
;

options_line:
    | OPTIONS options EOL   { Options $2 }
;

docfiles_line:
    | DOCFILES docfiles EOL { Docfiles $2 }
;

description_line:
    | DESCRIPTION STRINGPAIR { let (s, l) = $2 in Description (s, l) }
;

confopts_line:
    | CONFOPTS confopts EOL    { Confopts $2 }
;

bytecode_target_line:
    | BC_TARGET STRING EOL  { Bytecode_Target $2 }
    | BC_TARGET STRING error EOL { trail 3 "bytecode target" }
    | BC_TARGET EOL         { missing "bytecode target" }
;

opt_target_line:
    | OPT_TARGET STRING EOL { Opt_Target $2 }
    | OPT_TARGET STRING error EOL { trail 3 "opt target" }
    | OPT_TARGET EOL        { missing "opt target" }
;

deps:
  | dep COMMA deps  { $1 :: $3 }
  | dep             { [$1] }
  | dep error       { error 2 "Missing comma" }
  | /* epsilon */   { [] }
;

dep:
  | PKGNAME LPAREN RELOP STRING RPAREN
        { let (cat, name) = $1 in (cat, name, Some ($3, $4)) }
  | PKGNAME
        { let (cat, name) = $1 in (cat, name, None) }
  | error
        { invalid 1 "dependency" }
;

options:
  | OPTION COMMA options    { $1 :: $3 }
  | OPTION                  { [$1] }
  | error                   { invalid 1 "option" }
  | OPTION error            { error 2 "Missing comma" }
  | /* epsilon */           { [] }
;

docfiles:
  | STRING COMMA docfiles   { $1 :: $3 }
  | STRING                  { [$1] }
  | error                   { invalid 1 "doc file" }
  | STRING error            { error 2 "Missing comma" }
  | /* epsilon */           { [] }
;

confopts:
    |                       { [] }
;

%%
