/********************************************************************************/
/*  Lambdoc_rlambtex_parser.mly
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*/
/********************************************************************************/

%{
module Globalenv = Lambdoc_rlambtex_globalenv

open Lambdoc_reader
open Globalenv
%}


/********************************************************************************/
/* Operators.                                                                   */
/********************************************************************************/

%token EOF
%token BEGIN
%token END

%token <Lambdoc_reader_ast.command_t> NEW_PAR
%token <Lambdoc_reader_ast.command_t> ROW_END
%token <Lambdoc_reader_ast.command_t> CELL_MARK

%token <Lambdoc_reader_ast.command_t * BatText.t> PLAIN
%token <Lambdoc_reader_ast.command_t * string> ENTITY
%token <BatText.t> RAW


/********************************************************************************/
/* Environment operators.  These are used in an inline context.                 */
/* Presently the only existing environment operators are [$ $] and <$ $>.       */
/********************************************************************************/

%token <Lambdoc_reader_ast.command_t> BEGIN_MATHTEX_INL
%token <Lambdoc_reader_ast.command_t> END_MATHTEX_INL

%token <Lambdoc_reader_ast.command_t> BEGIN_MATHML_INL
%token <Lambdoc_reader_ast.command_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.               */
/* All environment commands are composed of a begin/end pair.                   */
/* While the opening tag is different for each environment,                     */
/* END_BLOCK is used as the common termination tag.                             */
/********************************************************************************/

%token <string> BEGIN_ITEMIZE
%token <string> BEGIN_ENUMERATE
%token <string> BEGIN_DESCRIPTION
%token <string> BEGIN_QANDA
%token <string> BEGIN_VERSE
%token <string> BEGIN_QUOTE
%token <string> BEGIN_MATHTEX_BLK
%token <string> BEGIN_MATHML_BLK
%token <string> BEGIN_SOURCE
%token <string> BEGIN_TABULAR
%token <string> BEGIN_SUBPAGE
%token <string> BEGIN_VERBATIM
%token <string> BEGIN_PULLQUOTE
%token <string> BEGIN_CUSTOM
%token <string> BEGIN_EQUATION
%token <string> BEGIN_PRINTOUT
%token <string> BEGIN_TABLE
%token <string> BEGIN_FIGURE
%token <string> BEGIN_ABSTRACT
%token <string> BEGIN_BIB
%token <string> BEGIN_NOTE
%token <string> BEGIN_BLKPAT_LIT
%token <string> BEGIN_BLKPAT_FRAG

%token END_BLOCK


/********************************************************************************/
/* Simple commands.                                                             */
/********************************************************************************/

%token <Lambdoc_reader_ast.command_t> LINEBREAK
%token <Lambdoc_reader_ast.command_t> GLYPH
%token <Lambdoc_reader_ast.command_t> BOLD
%token <Lambdoc_reader_ast.command_t> EMPH
%token <Lambdoc_reader_ast.command_t> CODE
%token <Lambdoc_reader_ast.command_t> CAPS
%token <Lambdoc_reader_ast.command_t> INS
%token <Lambdoc_reader_ast.command_t> DEL
%token <Lambdoc_reader_ast.command_t> SUP
%token <Lambdoc_reader_ast.command_t> SUB
%token <Lambdoc_reader_ast.command_t> MBOX
%token <Lambdoc_reader_ast.command_t> SPAN
%token <Lambdoc_reader_ast.command_t> LINK
%token <Lambdoc_reader_ast.command_t> SEE
%token <Lambdoc_reader_ast.command_t> CITE
%token <Lambdoc_reader_ast.command_t> DREF
%token <Lambdoc_reader_ast.command_t> SREF
%token <Lambdoc_reader_ast.command_t> MREF

%token <Lambdoc_reader_ast.command_t> PARAGRAPH
%token <Lambdoc_reader_ast.command_t> PICTURE
%token <Lambdoc_reader_ast.command_t> PART
%token <Lambdoc_reader_ast.command_t> APPENDIX
%token <Lambdoc_reader_ast.command_t * int> SECTION
%token <Lambdoc_reader_ast.command_t> BIBLIOGRAPHY
%token <Lambdoc_reader_ast.command_t> NOTES
%token <Lambdoc_reader_ast.command_t> TOC
%token <Lambdoc_reader_ast.command_t * int> TITLE
%token <Lambdoc_reader_ast.command_t> RULE
%token <Lambdoc_reader_ast.command_t> MACRODEF
%token <Lambdoc_reader_ast.command_t> BOXOUTDEF
%token <Lambdoc_reader_ast.command_t> THEOREMDEF

%token <Lambdoc_reader_ast.command_t> ITEM
%token <Lambdoc_reader_ast.command_t> QUESTION
%token <Lambdoc_reader_ast.command_t> RQUESTION
%token <Lambdoc_reader_ast.command_t> ANSWER
%token <Lambdoc_reader_ast.command_t> RANSWER

%token <Lambdoc_reader_ast.command_t> THEAD
%token <Lambdoc_reader_ast.command_t> TFOOT
%token <Lambdoc_reader_ast.command_t> TBODY
%token <Lambdoc_reader_ast.command_t> BIB_AUTHOR
%token <Lambdoc_reader_ast.command_t> BIB_TITLE
%token <Lambdoc_reader_ast.command_t> BIB_RESOURCE

%token <Lambdoc_reader_ast.command_t> MACROARG
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t > MACROCALL

%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_EMPTY
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_SEQ
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_RAW
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_RAW_RAW
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_RAW_SEQ
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> INLPAT_RAW_SEQOPT

%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> BLKPAT_EMPTY
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> BLKPAT_SEQ
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> BLKPAT_RAW
%token <Lambdoc_reader_ast.command_t * Lambdoc_core_basic.Ident.t> BLKPAT_RAW_RAW


/********************************************************************************/
/* Dummy tokens.                                                                */
/********************************************************************************/

%token <Lambdoc_reader_ast.command_t> BEGIN_DUMMY
%token <string> END_DUMMY
%token OPEN_DUMMY
%token CLOSE_DUMMY


/********************************************************************************/
/* Type declarations.                                                           */
/********************************************************************************/

%type <Lambdoc_reader_ast.t> document


/********************************************************************************/
/* Begin grammar specification and declare top-level rules.                     */
/********************************************************************************/

%start document

%%

document:
    | block* EOF    {$1}


/********************************************************************************/
/* Blocks.                                                                      */
/********************************************************************************/

block:
    | NEW_PAR inline+   {($1, Ast.Paragraph $2)}
    | simple_block      {$1}
    | env_block         {$1}

simple_block:
    | PARAGRAPH inline_bundle                       {($1, Ast.Paragraph $2)}
    | PICTURE raw_bundle raw_bundle                 {($1, Ast.Picture ($2, $3))}
    | PART inline_bundle                            {($1, Ast.Part $2)}
    | APPENDIX                                      {($1, Ast.Appendix)}
    | SECTION inline_bundle                         {let (comm, level) = $1 in (comm, Ast.Section (level, $2))}
    | BIBLIOGRAPHY                                  {($1, Ast.Bibliography)}
    | NOTES                                         {($1, Ast.Notes)}
    | TOC                                           {($1, Ast.Toc)}
    | TITLE inline_bundle                           {let (comm, level) = $1 in (comm, Ast.Title (level, $2))}
    | RULE                                          {($1, Ast.Rule)}
    | MACRODEF raw_bundle raw_bundle inline_bundle  {($1, Ast.Macrodef ($2, $3, $4))}
    | BOXOUTDEF raw_bundle boxoutdef                {let (caption, counter) = $3 in ($1, Ast.Boxoutdef ($2, caption, counter))}
    | THEOREMDEF raw_bundle theoremdef              {let (caption, counter) = $3 in ($1, Ast.Theoremdef ($2, caption, counter))}
    | sim_blkpat                                    {let (comm, tag, blkpat) = $1 in (comm, Ast.Extcomm_blk (tag, blkpat))}

env_block:
    | begin_block(blk_itemize) anon_item_frag* end_block                {let (comm, _) = $1 in (comm, Ast.Itemize $2)}
    | begin_block(blk_enumerate) anon_item_frag* end_block              {let (comm, _) = $1 in (comm, Ast.Enumerate $2)}
    | begin_block(blk_description) desc_item_frag* end_block            {let (comm, _) = $1 in (comm, Ast.Description $2)}
    | begin_block(blk_qanda) qanda_frag* end_block                      {let (comm, _) = $1 in (comm, Ast.Qanda $2)}
    | begin_block(blk_verse) block* end_block                           {let (comm, _) = $1 in (comm, Ast.Verse $2)}
    | begin_block(blk_quote) block* end_block                           {let (comm, _) = $1 in (comm, Ast.Quote $2)}
    | begin_block(blk_mathtex_blk) RAW end_block                        {let (comm, _) = $1 in (comm, Ast.Mathtex_blk (BatText.to_string $2))}
    | begin_block(blk_mathml_blk) RAW end_block                         {let (comm, _) = $1 in (comm, Ast.Mathml_blk (BatText.to_string $2))}
    | begin_block(blk_source) RAW end_block                             {let (comm, _) = $1 in (comm, Ast.Source (BatText.to_string $2))}
    | begin_block(blk_tabular) raw_bundle tabular end_block             {let (comm, _) = $1 in (comm, Ast.Tabular ($2, $3))}
    | begin_block(blk_subpage) block* end_block                         {let (comm, _) = $1 in (comm, Ast.Subpage $2)}
    | begin_block(blk_verbatim) RAW end_block                           {let (comm, _) = $1 in (comm, Ast.Verbatim (BatText.to_string $2))}
    | begin_block(blk_pullquote) inline_bundle? block* end_block        {let (comm, _) = $1 in (comm, Ast.Pullquote ($2, $3))}
    | begin_block(blk_custom) inline_bundle? block* end_block           {let (comm, tag) = $1 in (comm, Ast.Custom (None, tag, $2, $3))}
    | begin_block(blk_equation) inline_bundle? block end_block          {let (comm, _) = $1 in (comm, Ast.Equation ($2, $3))}
    | begin_block(blk_printout) inline_bundle? block end_block          {let (comm, _) = $1 in (comm, Ast.Printout ($2, $3))}
    | begin_block(blk_table) inline_bundle? block end_block             {let (comm, _) = $1 in (comm, Ast.Table ($2, $3))}
    | begin_block(blk_figure) inline_bundle? block end_block            {let (comm, _) = $1 in (comm, Ast.Figure ($2, $3))}
    | begin_block(blk_abstract) block* end_block                        {let (comm, _) = $1 in (comm, Ast.Abstract $2)}
    | begin_block(blk_bib) bib_author bib_title bib_resource end_block  {let (comm, _) = $1 in (comm, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
    | begin_block(blk_note) block* end_block                            {let (comm, _) = $1 in (comm, Ast.Note $2)}
    | env_blkpat                                                        {let (comm, tag, blkpat) = $1 in (comm, Ast.Extcomm_blk (tag, blkpat))}

anon_item_frag:
    | ITEM block*                               {($1, $2)}

desc_item_frag:
    | ITEM inline_bundle block*                 {($1, $2, $3)}

qanda_frag:
    | QUESTION inline_bundle? block*            {($1, Ast.New_questioner $2, $3)}
    | RQUESTION block*                          {($1, Ast.Same_questioner, $2)}
    | ANSWER inline_bundle? block*                      {($1, Ast.New_answerer $2, $3)}
    | RANSWER block*                            {($1, Ast.Same_answerer, $2)}

bib_author:
    | BIB_AUTHOR inline_bundle                  {($1, $2)}

bib_title:
    | BIB_TITLE inline_bundle                   {($1, $2)}

bib_resource:
    | BIB_RESOURCE inline_bundle                {($1, $2)}

boxoutdef:
    | /* empty */                               {(None, None)}
    | inline_bundle                             {(Some $1, None)}
    | inline_bundle raw_bundle                  {(Some $1, Some $2)}

theoremdef:
    | inline_bundle                             {($1, None)}
    | inline_bundle raw_bundle                  {($1, Some $2)}

sim_blkpat:
    | BLKPAT_EMPTY                              {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_empty)}
    | BLKPAT_SEQ inline_bundle                  {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_seq $2)}
    | BLKPAT_RAW raw_bundle                     {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_raw $2)}
    | BLKPAT_RAW_RAW raw_bundle raw_bundle      {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_raw_raw ($2, $3))}

env_blkpat:
    | begin_block(blkpat_lit) RAW end_block     {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_lit (BatText.to_string $2))}
    | begin_block(blkpat_frag) block* end_block {let (comm, tag) = $1 in (comm, tag, Ast.Blkpat_frag $2)}


/********************************************************************************/
/* Rules for tabular environment.                                               */
/********************************************************************************/

tabular:
    | head? body+ foot?                         {{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
    | row row* body* foot?                      {{Ast.thead = None; Ast.tfoot = $4; Ast.tbodies = (fst $1, $1 :: $2) :: $3;}}

head:   THEAD row+                              {($1, $2)}
foot:   TFOOT row+                              {($1, $2)}
body:   TBODY row+                              {($1, $2)}
row:    cell+ ROW_END                           {($2, $1)}
cell:   CELL_MARK raw_bundle? option(inline+)   {($1, $2, $3)}


/********************************************************************************/
/* Inline context.                              */
/********************************************************************************/

inline:
    | PLAIN                                                                         {let (comm, txt) = $1 in (comm, Ast.Plain (BatText.to_string txt))}
    | ENTITY                                                                        {let (comm, ent) = $1 in (comm, Ast.Entity ent)}
    | LINEBREAK                                                                     {($1, Ast.Linebreak)}
    | BEGIN_MATHTEX_INL push(mathtex_inl) OPEN_DUMMY RAW pop_brk END_MATHTEX_INL    {($1, Ast.Mathtex_inl (BatText.to_string $4))}
    | BEGIN_MATHML_INL push(mathml_inl) OPEN_DUMMY RAW pop_brk END_MATHML_INL       {($1, Ast.Mathml_inl (BatText.to_string $4))}
    | GLYPH raw_bundle raw_bundle                                                   {($1, Ast.Glyph ($2, $3))}
    | BOLD inline_bundle                                                            {($1, Ast.Bold $2)}
    | EMPH inline_bundle                                                            {($1, Ast.Emph $2)}
    | CODE inline_bundle                                                            {($1, Ast.Code $2)}
    | CAPS inline_bundle                                                            {($1, Ast.Caps $2)}
    | INS inline_bundle                                                             {($1, Ast.Ins $2)}
    | DEL inline_bundle                                                             {($1, Ast.Del $2)}
    | SUP inline_bundle                                                             {($1, Ast.Sup $2)}
    | SUB inline_bundle                                                             {($1, Ast.Sub $2)}
    | MBOX inline_bundle                                                            {($1, Ast.Mbox $2)}
    | SPAN inline_bundle                                                            {($1, Ast.Span $2)}
    | LINK raw_bundle inline_bundle?                                                {($1, Ast.Link ($2, $3))}
    | SEE raw_bundle*                                                               {($1, Ast.See $2)}
    | CITE raw_bundle*                                                              {($1, Ast.Cite $2)}
    | DREF raw_bundle inline_bundle?                                                {($1, Ast.Dref ($2, $3))}
    | SREF raw_bundle inline_bundle?                                                {($1, Ast.Sref ($2, $3))}
    | MREF raw_bundle inline_bundle                                                 {($1, Ast.Mref ($2, $3))}
    | MACROARG raw_bundle                                                           {($1, Ast.Macroarg $2)}
    | MACROCALL inline_bundle*                                                      {let (comm, label) = $1 in (comm, Ast.Macrocall (label, $2))}
    | sim_inlpat                                                                    {let (comm, tag, inlpat) = $1 in (comm, Ast.Extcomm_inl (tag, inlpat))}

sim_inlpat:
    | INLPAT_EMPTY                                  {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_empty)}
    | INLPAT_SEQ inline_bundle                      {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_seq $2)}
    | INLPAT_RAW raw_bundle                         {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw $2)}
    | INLPAT_RAW_RAW raw_bundle raw_bundle          {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_raw ($2, $3))}
    | INLPAT_RAW_SEQ raw_bundle inline_bundle       {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_seq ($2, $3))}
    | INLPAT_RAW_SEQOPT raw_bundle inline_bundle?   {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_seqopt ($2, $3))}


/********************************************************************************/
/* Bundles.                                                                     */
/********************************************************************************/

inline_bundle:      BEGIN push(general) OPEN_DUMMY inline* pop_brk END  {$4}
raw_bundle:         BEGIN push(raw) OPEN_DUMMY RAW pop_brk END          {BatText.to_string $4}


/********************************************************************************/
/* Dummy actions.                                                               */
/********************************************************************************/

begin_block(x): push_blk(x) BEGIN_DUMMY         {($2, $1)}
end_block:      pop_blk END_BLOCK               {$2}

push_blk(x):    x                               {let (tag, scanner) = $1 in Globalenv.push (Some tag, scanner); tag}
push(x):        x                               {Globalenv.push $1}
pop_blk:        END_DUMMY                       {Globalenv.pop (Some $1)}
pop_brk:        CLOSE_DUMMY                     {Globalenv.pop None}


/********************************************************************************/

general:        /* empty */                     {(None, Scanner_general)}
raw:            /* empty */                     {(None, Scanner_raw)}
mathtex_inl:    /* empty */                     {(None, Scanner_mathtex_inl)}
mathml_inl:     /* empty */                     {(None, Scanner_mathml_inl)}


/********************************************************************************/

blk_itemize:        BEGIN_ITEMIZE               {($1, Scanner_general)}
blk_enumerate:      BEGIN_ENUMERATE             {($1, Scanner_general)}
blk_description:    BEGIN_DESCRIPTION           {($1, Scanner_general)}
blk_qanda:          BEGIN_QANDA                 {($1, Scanner_general)}
blk_verse:          BEGIN_VERSE                 {($1, Scanner_general)}
blk_quote:          BEGIN_QUOTE                 {($1, Scanner_general)}
blk_mathtex_blk:    BEGIN_MATHTEX_BLK           {($1, Scanner_literal $1)}
blk_mathml_blk:     BEGIN_MATHML_BLK            {($1, Scanner_literal $1)}
blk_source:         BEGIN_SOURCE                {($1, Scanner_literal $1)}
blk_tabular:        BEGIN_TABULAR               {($1, Scanner_tabular)}
blk_subpage:        BEGIN_SUBPAGE               {($1, Scanner_general)}
blk_verbatim:       BEGIN_VERBATIM              {($1, Scanner_literal $1)}
blk_pullquote:      BEGIN_PULLQUOTE             {($1, Scanner_general)}
blk_custom:         BEGIN_CUSTOM                {($1, Scanner_general)}
blk_equation:       BEGIN_EQUATION              {($1, Scanner_general)}
blk_printout:       BEGIN_PRINTOUT              {($1, Scanner_general)}
blk_table:          BEGIN_TABLE                 {($1, Scanner_general)}
blk_figure:         BEGIN_FIGURE                {($1, Scanner_general)}
blk_abstract:       BEGIN_ABSTRACT              {($1, Scanner_general)}
blk_bib:            BEGIN_BIB                   {($1, Scanner_general)}
blk_note:           BEGIN_NOTE                  {($1, Scanner_general)}
blkpat_lit:         BEGIN_BLKPAT_LIT            {($1, Scanner_literal $1)}
blkpat_frag:        BEGIN_BLKPAT_FRAG           {($1, Scanner_general)}

