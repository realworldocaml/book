
/* POY 5.1.1. A phylogenetic analysis program using Dynamic Homologies.       */
/* Copyright (C) 2014 Andrés Varón, Lin Hong, Nicholas Lucaroni, Ward Wheeler,*/
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

/* Parser for nexus files */
%{

let mesquite_error =
    ("This@ seems@ to@ be@ a@ Mesquite@ \"NEXUS\"@ file.@ Unfortunately@ " ^
     "Mesquite@ has@ invented@ a@ new@ command@ TITLE@ that@ is@ not@ a@ " ^
     "valid@ NEXUS@ command.@ You@ will@ have@ to@ remove@ it@ by@ hand.@ " ^
     "You@ can@ read@ their@ information@ about@ it@ here:@ http://mesquiteproject.org/mesquite_folder/docs/mesquite/otherPrograms.html")

let parse_error s =
    try let b = (Parsing.symbol_start_pos ())
        and e = (Parsing.symbol_end_pos ()) in
        let b = string_of_int (b.Lexing.pos_cnum)
        and e = string_of_int (e.Lexing.pos_cnum) in
        !P.print_error (s ^ "@ between@ characters@ " ^ b ^ "@ and@ " ^ e)
    with | _ ->
        !P.print_error s

let report_error text b e =
    let b = string_of_int (b.Lexing.pos_cnum)
    and e = string_of_int (e.Lexing.pos_cnum) in
    !P.print_error ("There@ was@ a@ parsing@ error@ between@ characters@ " ^ b ^ "@ and@ " ^ e ^ " in the " ^ text)

%}
%token EOF
%token <string> ALPHA
%token <string> ANCSTATES
%token <string> ANNOTATIONS
%token <string> APPROXIMATE
%token <string> ASSUMPTIONS
%token <string> AVERAGE
%token <string> BEGIN
%token <string> BINHEX
%token <string> BOTH
%token <string> BREAKPOINT
%token <string> BREAKINVERSION
%token <string> CHANGESET
%token <char>   CHAR
%token <string> CHARACTER
%token <string> CHARACTERBRANCH
%token <string> CHARACTERS
%token <string> CHARLABELS
%token <string> CHARPARTITION
%token <string> CHARSET
%token <string> CHARSTATELABELS
%token <string> CHROMOSOME
%token <string> CIRCULAR
%token <string> CODEORDER
%token <string> CODESET
%token <string> CODONS
%token <string> CONDONPOSSET
%token <string> CONTINUOUS
%token <string> COST
%token <string> COUNT
%token <string> COUPLED
%token <string> CSTREE
%token <string> COVERAGE
%token <string> DATA
%token <string> DATACSTREE
%token <string> DATATYPE
%token <string> DEFTYPE
%token <string> DEFAULT
%token <string> DIAGONAL
%token <string> DIMENSIONS
%token <string> DISTANCES
%token <string> DISTANCE
%token <string> DNA
%token <string> DROS
%token <string> ELIMINATE
%token <string> ENCODE
%token <string> ENDNEXUS
%token <string> EPS
%token <string> EQUATE
%token <string> EXSET
%token <string> EXT
%token <string> EXTENSIONS
%token <string> FILE
%token <string> FORMAT
%token <string> FREQUENCY
%token <string> GAP
%token <string> GAPOPENING
%token <string> GENETICCODE
%token <string> GENOME
%token <string> GIF
%token <string> INDEL
%token <string> INDIVIDUALS
%token <string> INLINE
%token <string> INVERSION
%token <string> INTERLEAVE
%token <string> ITEMS
%token <string> JPEG
%token <string> LABELS
%token <string> LEVEL
%token <string> LINK
%token <string> LIKELIHOOD
%token <string> LOWER
%token <string> MAM
%token <string> MAP
%token <string> MATCHCHAR
%token <string> MAUVE
%token <string> MAX
%token <string> MAXSTEPS
%token <string> MEDIAN
%token <string> MIN
%token <string> MINSTEPS
%token <string> MISSING
%token <string> MODEL
%token <string> MTDNA
%token <string> NCHAR
%token <string> NEWSTATE
%token <string> NEWTAXA
%token <string> NOLABELS
%token <string> NONE
%token <string> NOTES
%token <string> NTAX
%token <string> NOTOKENS
%token <string> NUCLEOTIDE
%token <string> NUCORDER
%token <string> OPTIONS
%token <string> PARAMETERS
%token <string> PERCENT
%token <string> POY
%token <string> PICT
%token <string> PICTURE
%token <string> POLYTCOUNT
%token <string> PRIORS
%token <string> PROTEIN
%token <string> QUALITY
%token <string> QUOTED
%token <string> REARRANGEMENT
%token <string> RESOURCE
%token <string> RESPECTCASE
%token <string> RNA
%token <string> SAMPLESIZE
%token <string> SETS
%token <string> SINGLEQUOTED
%token <string> SITES
%token <string> SOURCE
%token <string> SOLVER
%token <string> STANDARD
%token <string> STATE
%token <string> STATELABELS
%token <string> STATES
%token <string> STATESET
%token <string> STATESFORMAT
%token <string> STATESPRESENT
%token <string> STDERROR
%token <string> STEPMATRIX
%token <string> SYMBOLS
%token <string> SYMMETRIC
%token <string> TAXON
%token <string> TAXA
%token <string> TAXLABELS
%token <string> TAXPARTITION
%token <string> TAXSET
%token <string> TCM
%token <string> TEXT
%token <string> TIFF
%token <string> TITLE
%token <string> TOKENS
%token <string> TRANSLATE
%token <string> TRANSPOSE
%token <string> TREE
%token <string> UTREE
%token <string> TREEPARTITION
%token <string> TREES
%token <string> TREESET
%token <string> TRIANGLE
%token <string> TYPESET
%token <string> UNALIGNED
%token <string> UNIVERSAL
%token <string> UPPER
%token <string> USERTYPE
%token <string> UUENCODE
%token <string> VARIANCE
%token <string> VARIATION
%token <string> VECTOR
%token <string> WTSET
%token <string> YEAST
%token <string> EIDENT
%token NEXUS SEMICOLON EQUAL COMMA QUOTE BACKSLASH DASH LPARENT RPARENT STAR COLON LBRACKET RBRACKET SLASH
%token <string> IDENT
%token <string> FLOAT
%token <string> INTEGER
%start tree
%type <P.tree> tree
%start trees
%type <P.tree list> trees
%start header
%type <unit> header
%start block
%type <P.block> block
%start symbol_pair
%type <(string * string list)> symbol_pair
%start symbol_list
%type <string list> symbol_list
%%

header:
    | NEXUS { () }
    ;
block:
    | BEGIN TAXA SEMICOLON mesquite_broken taxa ENDNEXUS SEMICOLON
        { P.Taxa $5 }
    | BEGIN CHARACTERS SEMICOLON mesquite_broken characters ENDNEXUS SEMICOLON
        { P.Characters $5 }
    | BEGIN DATA SEMICOLON mesquite_broken characters ENDNEXUS SEMICOLON
        { P.Characters $5 }
    | BEGIN UNALIGNED SEMICOLON mesquite_broken unaligned ENDNEXUS SEMICOLON
        { P.Unaligned $5 }
    | BEGIN TREES SEMICOLON mesquite_broken optional_translate tree_list ENDNEXUS SEMICOLON
        { P.Trees ($5,$6) }
    | BEGIN NOTES SEMICOLON mesquite_broken notes ENDNEXUS SEMICOLON
        { P.Notes $5 }
    | BEGIN DISTANCES SEMICOLON mesquite_broken distances ENDNEXUS SEMICOLON
        { P.Distances $5 }
    | BEGIN ASSUMPTIONS SEMICOLON mesquite_broken assumptions ENDNEXUS SEMICOLON
        { P.Assumptions $5 }
    | BEGIN SETS SEMICOLON mesquite_broken sets_block ENDNEXUS SEMICOLON
        { P.Sets $5 }
    | BEGIN POY SEMICOLON poy_block ENDNEXUS SEMICOLON
        { P.Poy $4 }
/* error states  -- some of these are necessary     */
/*  | BEGIN TAXA SEMICOLON error ENDNEXUS SEMICOLON
/*      { P.Error $2 }
/*  | BEGIN UNALIGNED SEMICOLON error ENDNEXUS SEMICOLON
/*      { P.Error $2 }
/*  | BEGIN SETS SEMICOLON error ENDNEXUS SEMICOLON */
/*      { P.Error $2 }                              */
    | BEGIN NOTES SEMICOLON error ENDNEXUS SEMICOLON
        { P.Error $2 }
    | BEGIN DISTANCES SEMICOLON error ENDNEXUS SEMICOLON
        { P.Error $2 }
/* This is to avoid processing unknown blocks       */
    | BEGIN IDENT error ENDNEXUS SEMICOLON
        { P.Error $2 }
    | header
        { P.Error "File Contains Multiple Nexus Files; will try to append" }
    ;

mesquite_broken:
    | mesquite_broken_items mesquite_broken { $1 :: $2  }
    |                                       { []        }
    ;
mesquite_broken_items:
    | TITLE any_thing_minus_end SEMICOLON { () }
    | LINK  any_thing_minus_end EQUAL any_thing_minus_end SEMICOLON { () }
    ;

assumptions:
    | assumption_items assumptions { $1 :: $2 }
    | { [] }
    ;
assumption_items:
    | optional_assumption_options   { P.Options $1 }
    | optional_user_type            { P.UserType $1 }
    | optional_type_set             { P.TypeDef $1 }
    | optional_wtset                { P.WeightDef $1 }
    | optional_exset                { P.ExcludeSet $1 }
    | optional_ancstates            { P.AncestralDef $1 }
    | error
        { report_error "Assumption Block" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ());
          raise Parsing.Parse_error }
    ;

set_in_block:
    | CHARSET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list
        { $2, P.CharacterSet $7     }
    | STATESET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list
        { $2, P.StateSet $7         }
    | TAXSET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list
        { $2, P.TaxonSet $7         }
    | TREESET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list
        { $2, P.TreeSet $7          }
    | CHARPARTITION nexus_word partition_contents
        { $2, P.CharPartition $3    }
    | TAXPARTITION nexus_word partition_contents
        { $2, P.TaxPartition $3     }
    | TREEPARTITION nexus_word partition_contents
        { $2, P.TreePartition $3    }
    ;

optional_lparent:
    | LPARENT  { () }
    | { () }
    ;
optional_rparent:
    | RPARENT { () }
    | { () }
    ;
partition_contents:
    | do_token EQUAL standard_type_set          { P.Standard $3 }
    | do_token STANDARD EQUAL standard_type_set { P.Standard $4 }
    | do_token VECTOR EQUAL vector_type_set     { P.Vector $4   }
    ;
sets_block:
    | set_in_block SEMICOLON sets_block { $1 :: $3 }
    |                                   {       [] }
    ;
optional_standard_or_vector:
    | STANDARD { Some $1 }
    | VECTOR   { Some $1 }
    | { None }
    ;

optional_assumption_options:
    | OPTIONS deftype polytcount gapmode SEMICOLON { ($2, $3, $4) }
    ;
deftype:
    | DEFTYPE EQUAL IDENT { Some $3 }
    | { None }
    ;
polytcount:
    | POLYTCOUNT EQUAL MINSTEPS { P.MinSteps }
    | POLYTCOUNT EQUAL MAXSTEPS { P.MaxSteps }
    | { P.MinSteps }
    ;
gapmode:
    | GAP EQUAL MISSING { P.Missing }
    | GAP EQUAL NEWSTATE { P.NewState }
    | { P.NewState }
    ;
optional_user_type:
    | USERTYPE IDENT user_type_definition SEMICOLON { ($2, $3) }
    ;
user_type_definition:
    | EQUAL INTEGER numbers_and_chars { P.StepMatrix ($2, $3) }
    | STEPMATRIX EQUAL INTEGER numbers_and_chars
        { P.StepMatrix ($3, $4) }
    | LPARENT STEPMATRIX RPARENT EQUAL INTEGER numbers_and_chars
        { P.StepMatrix ($5, $6) }
    | CSTREE DATACSTREE { P.CSTree $2 }
    ;
numbers_and_chars:
    | number_and_char numbers_and_chars { $1 :: $2 }
    | number_and_char { [$1] }
    ;
number_and_char:
    | INTEGER   { $1 }
    | FLOAT     { $1 }
    | CHAR      { Char.escaped $1 }
    | COLON     { ":" }
    | EQUAL     { "=" }
    | COMMA     { "," }
    | BACKSLASH { "/" }
    | DASH      { "-" }
    | STAR      { "*" }
    | IDENT     { $1 }
    ;
optional_type_set:
    | TYPESET optional_set_for_assumptions { $2 }
    ;

optional_wtset:
    | WTSET optional_set_for_assumptions { $2 }
    ;

do_token:
    | NOTOKENS  { false }
    | TOKENS    { true  }
    |           { false }
    ;

do_star:
    | STAR { true }
    |      { false }
    ;

optional_set_for_assumptions:
    | do_star IDENT do_token EQUAL standard_type_set SEMICOLON
        { ($1, $2, $3, P.Standard $5) }
    | do_star IDENT STANDARD do_token EQUAL standard_type_set SEMICOLON
        { ($1, $2, $4, P.Standard $6) }
    | do_star IDENT VECTOR do_token EQUAL vector_type_set SEMICOLON
        { ($1, $2, $4,P.Vector $6) }
    ;

standard_type_set_item:
    | INTEGER COLON characterset_list { P.Code ($1, $3) }
    | FLOAT COLON characterset_list   { P.Code ($1, $3) }
    | IDENT COLON characterset_list   { P.IName ($1, $3) }
    | SINGLEQUOTED COLON characterset_list { P.IName($1, $3) }
    | QUOTED COLON characterset_list { P.IName($1, $3) }
    ;
standard_type_set:
    | standard_type_set_item COMMA standard_type_set { ($1 :: $3) }
    | standard_type_set_item { [$1] }
    |                        { []   }
    ;
vector_type_set:
    | INTEGER vector_type_set { ($1 :: $2)  }
    |                         { []          }
    ;
optional_exset:
    | EXSET do_star nexus_word EQUAL characterset_list SEMICOLON
        { ($2, $3, P.STDStandard $5) }
    | EXSET do_star nexus_word STANDARD EQUAL characterset_list SEMICOLON
        { ($2, $3, P.STDStandard $6) }
    | EXSET do_star nexus_word VECTOR EQUAL vector_type_set SEMICOLON
        { ($2, $3, P.STDVector $6)   }
    ;

optional_ancstates:
    | ANCSTATES optional_set_for_assumptions { $2 }
    ;
distances:
    | optional_distances_dimensions optional_format optional_taxlabels DATA SEMICOLON
        { ($1, $2, $3, $4) }
    ;
optional_distances_dimensions:
    | DIMENSIONS NEWTAXA NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (true, $5, $8) }
    | DIMENSIONS NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (false, $4, $7) }
    | { None }
    ;
notes:
    | optional_text optional_picture
        { $1, $2 }
    ;
optional_text:
    | TEXT optional_set_pair_list SOURCE EQUAL source DATA SEMICOLON
        { Some ($2, $5, $6) }
    | { None }
    ;
optional_picture:
    | PICTURE optional_set_pair_list optional_pictureformat optional_encode SOURCE EQUAL source DATA SEMICOLON
        { Some ($2, $3, $4, $7, $8) }
    |   { None }
    ;
optional_set_pair_list:
    | set_pair optional_set_pair_list { $1 :: $2 }
    | { [] }
    ;
set_pair:
    | TAXON EQUAL characterset_list     { P.TaxonSet $3     }
    | CHARACTER EQUAL characterset_list { P.CharacterSet $3 }
    | STATE EQUAL characterset_list     { P.StateSet $3     }
    | TREE EQUAL characterset_list      { P.TreeSet $3      }
    ;
source:
    | INLINE    {P.Inline }
    | FILE      { P.File }
    | RESOURCE  { P.Resource }
    ;
optional_pictureformat:
    | FORMAT EQUAL pictureformat { Some $3 }
    | { None }
    ;
pictureformat:
    | PICT      { P.Pict }
    | TIFF      { P.Tiff }
    | EPS       { P.Eps }
    | JPEG      { P.Jpeg }
    | GIF       { P.Gif }
    ;
optional_encode:
    | ENCODE EQUAL pictureencoding { Some $3 }
    | { None }
    ;
pictureencoding:
    | NONE  { P.None }
    | UUENCODE { P.UUEncode }
    | BINHEX    { P.BinHex }
    ;
optional_translate:
    | TRANSLATE names SEMICOLON
        { snd (List.fold_left (fun (i,acc) x -> (i+1),(string_of_int i,x)::acc) (1,[]) $2) }
    | TRANSLATE pairs_list SEMICOLON { $2 }
    | { [] }
    ;
tree_list:
    | optional_tree_prequel DATA SEMICOLON tree_list { $2 :: $4 }
    | { [] }
    ;
optional_tree_prequel:
    | TREE do_star optional_label EQUAL {$3}
    | UTREE do_star optional_label EQUAL {$3}
    | { None }
    ;
poy_block:
    | CHARACTERBRANCH charbranch_block poy_block
        { (P.CharacterBranch $2) :: $3}
    | LIKELIHOOD model_block poy_block
        { P.Likelihood $2 :: $3 }
    | GAPOPENING do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (P.GapOpening ($2, $3, $5)) :: $7 }
    | WTSET do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (P.DynamicWeight ($2, $3, $5)) :: $7 }
    | TCM do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (P.Tcm ($2, $3, $5)) :: $7 }
    | LEVEL do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (P.Level($2,$3,$5)) :: $7 }
    | CHROMOSOME chrome_options COLON characterset_list SEMICOLON poy_block
        { (P.Chrom ($2,$4)) :: $6 }
    | GENOME genome_options COLON characterset_list SEMICOLON poy_block
        { (P.Genome ($2,$4)) :: $6 }
    | BREAKINVERSION breakinv_options COLON characterset_list SEMICOLON poy_block
        { (P.BreakInv ($2,$4)) :: $6 }
    |   { [] }
    ;

breakinv_options :
    | MEDIAN EQUAL INTEGER SEMICOLON breakinv_options
            { (P.Chrom_Median (int_of_string $3)) :: $5 }
    | BREAKPOINT EQUAL INTEGER SEMICOLON chrome_options
            { (P.Chrom_Locus_Breakpoint (int_of_string $3)) :: $5 }
    | INVERSION EQUAL INTEGER SEMICOLON chrome_options
            { (P.Chrom_Locus_Inversion (int_of_string $3)) :: $5 }
    |       { [] }
    ;
annotation_options :
    | QUALITY EQUAL FLOAT SEMICOLON annotation_options
            { (P.Annot_Quality (float_of_string $3)) :: $5 }
    | MIN EQUAL INTEGER SEMICOLON annotation_options
            { (P.Annot_Min_Percent (float_of_string $3)) :: $5 }
    | MAX EQUAL INTEGER SEMICOLON annotation_options
            { (P.Annot_Max_Percent (float_of_string $3)) :: $5 }
    | COVERAGE EQUAL FLOAT SEMICOLON annotation_options
            {   let len = float_of_string $3 in
                assert( len > 0.0 && len <= 1.0 );
                (P.Annot_Coverage len) :: $5
            }
    | MODEL EQUAL MAUVE SEMICOLON annotation_options
            { (P.Annot_Type `Mauve) :: $5 }
    | MODEL EQUAL DEFAULT SEMICOLON annotation_options
            { (P.Annot_Type `Default) :: $5 }
    | REARRANGEMENT EQUAL INTEGER SEMICOLON annotation_options
            { (P.Annot_Rearrangement (int_of_string $3)):: $5 }
    |       { [] }
    ;
chrome_options :
    | SOLVER EQUAL IDENT SEMICOLON chrome_options
            { (P.Chrom_Solver $3) :: $5 }
    | MEDIAN EQUAL INTEGER SEMICOLON chrome_options
            { (P.Chrom_Median (int_of_string $3)) :: $5 }
    | BREAKPOINT EQUAL INTEGER SEMICOLON chrome_options
            { (P.Chrom_Locus_Breakpoint (int_of_string $3)) :: $5 }
    | SYMMETRIC SEMICOLON chrome_options
            { (P.Chrom_Symmetric true) :: $3 }
    | APPROXIMATE SEMICOLON chrome_options
            { (P.Chrom_Approx true) :: $3 }
    | INDEL EQUAL INTEGER COMMA FLOAT SEMICOLON chrome_options
            { (P.Chrom_Locus_Indel (int_of_string $3, float_of_string $5)) :: $7 }
    | INVERSION EQUAL INTEGER SEMICOLON chrome_options
            { (P.Chrom_Locus_Inversion (int_of_string $3)) :: $5 }
    | ANNOTATIONS annotation_options SEMICOLON chrome_options
            { (P.Chrom_Annotations $2) :: $4 }
    |       { [] }
    ;
genome_options :
    | CIRCULAR SEMICOLON genome_options
            { (P.Genome_Circular true) :: $3 }
    | BREAKPOINT EQUAL INTEGER SEMICOLON genome_options
            { (P.Genome_Breakpoint (int_of_string $3)) :: $5 }
    | INDEL EQUAL INTEGER COMMA FLOAT SEMICOLON genome_options
            { (P.Genome_Indel (int_of_string $3, float_of_string $5)) :: $7 }
    | MEDIAN EQUAL INTEGER SEMICOLON genome_options
            { (P.Genome_Median (int_of_string $3)) :: $5 }
    | DISTANCE EQUAL FLOAT SEMICOLON genome_options
            { (P.Genome_Distance (float_of_string $3)) :: $5 }
    |       { [] }
    ;
charbranch_block :
    | TREES EQUAL names charbranch_block
        { (P.Tree_Names $3) :: $4 }
    | CHARSET EQUAL characterset_list SEMICOLON charbranch_block
        { (P.Set_Names $3) :: $5 }
    | MAP pairs_list_float SEMICOLON charbranch_block
        { (P.Labeling $2) :: $4 }
    |   { [] }
    ;
model_block:
    | MODEL EQUAL IDENT SEMICOLON model_block
            { (P.Model $3) :: $5 }
    | VARIATION EQUAL NONE SEMICOLON model_block
            { (P.Variation "none") :: $5 }
    | VARIATION EQUAL IDENT SEMICOLON model_block
            { (P.Variation $3) :: $5 }
    | COST EQUAL IDENT SEMICOLON model_block
            { (P.Cost_Mode $3) :: $5 }
    | SITES EQUAL INTEGER SEMICOLON model_block
            { (P.Variation_Sites $3) :: $5 }
    | PERCENT EQUAL FLOAT SEMICOLON model_block
            { (P.Variation_Invar $3) :: $5 }
    | ALPHA EQUAL FLOAT SEMICOLON model_block
            { (P.Variation_Alpha $3) :: $5 }
    | PRIORS EQUAL priors_options model_block
            { $3 :: $4 }
    | CHARSET EQUAL characterset_list SEMICOLON model_block
            { (P.Chars $3) :: $5 }
    | PARAMETERS EQUAL float_list model_block
            { (P.Parameters $3) :: $4 }
    | FILE EQUAL QUOTED SEMICOLON model_block
            { (P.Files $3) :: $5 }
    | GAP EQUAL COUPLED COLON FLOAT SEMICOLON model_block
            { (P.Gap_Mode ($3,(Some (float_of_string $5)))) :: $7 }
    | GAP EQUAL COUPLED SEMICOLON model_block
            { (P.Gap_Mode ($3,None)) :: $5 }
    | GAP EQUAL MISSING SEMICOLON model_block
            { (P.Gap_Mode ($3,None)) :: $5 }
    | GAP EQUAL IDENT SEMICOLON model_block
            { (P.Gap_Mode ($3,None)) :: $5 }
    | SEMICOLON { [] }
    ;

priors_options:
    | pairs_list_float  { P.Given_Priors $1 }
    | IDENT SEMICOLON   { P.Other_Priors $1 }
    ;

float_list:
    | FLOAT float_list      { (float_of_string $1) :: $2 }
    | INTEGER float_list    { (float_of_string $1) :: $2 }
    | FLOAT SEMICOLON       { [(float_of_string $1)] }
    | INTEGER SEMICOLON     { [(float_of_string $1)] }
    ;
names:
    | IDENT COMMA names { $1 :: $3 }
    | INTEGER COMMA names { $1 :: $3 }
    | IDENT SEMICOLON { [$1] }
    ;
pairs_list:
    | nexus_ident IDENT COMMA pairs_list   { ($1, $2) :: $4 }
    | nexus_ident IDENT                    { [$1, $2] }
    ;
pairs_list_float:
    | IDENT FLOAT COMMA pairs_list_float { ($1,(float_of_string $2)) :: $4 }
    | DASH  FLOAT COMMA pairs_list_float { ("-",(float_of_string $2)) :: $4 }
    | IDENT FLOAT SEMICOLON { [($1,(float_of_string $2))] }
    | DASH  FLOAT SEMICOLON { [("-",(float_of_string $2))] }
    ;
characters:
    | DIMENSIONS optional_taxa_dimensions NCHAR EQUAL INTEGER SEMICOLON
     optional_format optional_eliminate optional_taxlabels
     optional_charstatelabels optional_charlabels optional_statelabels
     DATA SEMICOLON
     {
         {
             P.char_taxon_dimensions = $2;
             P.char_char_dimensions = $5;
             P.char_format = $7;
             P.char_eliminate = $8;
             P.char_taxlabels = $9;
             P.char_statelabels = $10;
             P.char_charlabels = $11;
             P.char_charstates = $12;
             P.chars = $13;
        }
     }
     ;
unaligned:
    | optional_unaligned_dimensions optional_format DATA SEMICOLON
        { { P.unal_taxon_dimensions = $1; unal_format = $2; unal = $3 } }
     ;
optional_unaligned_dimensions:
    | DIMENSIONS optional_taxa_dimensions { $2 }
    | { None }
    ;
optional_charlabels:
    | CHARLABELS taxonlist SEMICOLON    { $2 }
    |                                   { [] }
    ;

charstatelabels:
    | INTEGER nexus_word BACKSLASH taxonlist COMMA charstatelabels { ($1, $2, $4) :: $6 }
    | INTEGER nexus_word BACKSLASH taxonlist SEMICOLON             { ($1, $2, $4) :: [] }
    | SEMICOLON                                                    { []                 }
    ;
optional_charstatelabels:
    | CHARSTATELABELS charstatelabels { $2 }
    |                                 { [] }
    ;
optional_statelabels:
    | STATELABELS statelabels SEMICOLON { $2 }
    |                                   { [] }
    ;

statelabels:
    | INTEGER taxonlist COMMA statelabels { ($1, $2) :: $4 }
    | INTEGER taxonlist                   { ($1, $2) :: [] }
    |                                     { []             }
    ;
optional_taxlabels:
    | TAXLABELS taxonlist SEMICOLON { $2 }
    |                           { [] }
    ;
optional_eliminate:
    | ELIMINATE characterset SEMICOLON { Some $2 }
    |                           { None }
    ;
optional_format:
    | FORMAT format_items_list SEMICOLON { $2 }
    |           { [] }
    ;
format_items_list:
    | format_items format_items_list { $1 :: $2 }
    | error format_items_list {
        report_error "Format list" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ());
        $2 }
    |           { [] }
    ;
format_items:
    | DATATYPE EQUAL datatype { P.Datatype $3 }
    | DATATYPE EQUAL error {
        !P.print_error
        ("Many@ \"NEXUS\"@ files@ include@ in@ the@ CHARACTERS@ or@ DATA@ " ^
        "block@ the@ dataype@ @[<b>mixed@]@ or@ @[<b>restriction@]@ or@ something@ else.@ " ^
        "@ Those@ are@ most@ likely@ Mr.Bayes@ files.@ No@ matter@ that@ " ^
        "they@ say@ #NEXUS@ at@ the@ beginning,@ this@ is@ not@ a@ valid@ " ^
        "NEXUS@ file.@ I can@ not@ process@ this.@ I@ am@ sorry. " ^
        "You'll@ have@ to@ convert@ it@ to@ a@ valid@ nexus@ file.");
        raise Parsing.Parse_error }
    | RESPECTCASE { P.RespectCase }
    | MISSING EQUAL symbol { P.FMissing $3 }
    | GAP EQUAL symbol { P.Gap $3 }
    | SYMBOLS EQUAL QUOTED { P.Symbols $3}
    | EQUATE EQUAL QUOTED { P.Equate $3 }
    | MATCHCHAR EQUAL symbol { P.MatchChar $3 }
    | NOLABELS     { P.Labels false }
    | LABELS        { P.Labels true }
    | TRANSPOSE     { P.Transpose }
    | INTERLEAVE    EQUAL {
        !P.print_error
        ("Many@ \"NEXUS\"@ files@ include@ in@ the@ CHARACTERS@ or@ DATA@ " ^
        "block@ the@ directives@ interleave=yes@ or@ interleave=no.@ " ^
        "@[<b>That@ is@ not@ a@ valid@ NEXUS@ directive@].@ To@ fix@ your@ file@ " ^
        "replace@ interleave=yes@ with@ INTERLEAVE,@ or@ just@ remove@ it@ " ^
        "if@ you@ see@ interleave=no.");
        raise Parsing.Parse_error }
    | INTERLEAVE    { P.Interleave }
    | ITEMS EQUAL item { P.Items $3 }
    | STATESFORMAT EQUAL states_format { P.StatesFormat $3 }
    | NOTOKENS     { P.Tokens false }
    | TOKENS        { P.Tokens true }
    | TRIANGLE EQUAL triangle_format { P.Triangle $3 }
    ;
triangle_format:
    | LOWER { P.Lower }
    | UPPER { P.Upper }
    | BOTH { P.Both }
    ;
datatype:
    | STANDARD      { P.DStandard   }
    | DNA           { P.Dna         }
    | RNA           { P.Rna         }
    | NUCLEOTIDE    { P.Nucleotide  }
    | PROTEIN       { P.Protein     }
    | CONTINUOUS    { P.Continuous  }
    ;
symbol:
    | IDENT { $1 }
    | DASH { "-" }
    | CHAR  { Char.escaped $1 }
    ;
symbol_list:
    | symbol symbol_list    { $1 :: $2  }
    |                       { []        }
    ;
symbol_pair:
    | symbol EQUAL symbol {($1, [$3])}
    | symbol EQUAL LPARENT symbol_list RPARENT { ($1, $4) }
    ;
item:
    | MIN           { P.Min         }
    | MAX           { P.Max         }
    | MEDIAN        { P.Median      }
    | AVERAGE       { P.Average     }
    | VARIANCE      { P.Variance    }
    | STDERROR      { P.Stderror    }
    | SAMPLESIZE    { P.SampleSize  }
    | STATES        { P.States      }
    ;
states_format:
    | STATESPRESENT { P.StatesPresent   }
    | INDIVIDUALS   { P.Individuals     }
    | COUNT         { P.Count           }
    | FREQUENCY     { P.Frequency       }
    ;
optional_taxa_dimensions:
    | NTAX EQUAL INTEGER    { Some $3   }
    |                       { None      }
    ;
taxa:
    | DIMENSIONS NTAX EQUAL INTEGER SEMICOLON TAXLABELS taxonlist SEMICOLON
        { ($4, $7) }
    ;
nexus_word :
    | IDENT         { $1 }
    | SINGLEQUOTED  { $1 }
    | DNA           { $1 }
    | RNA           { $1 }
    ;

nexus_ident :
    | nexus_word    { $1 }
    | INTEGER       { $1 }
    ;

taxonlist:
    | nexus_word taxonlist      { $1 :: $2  }
    |                           { []        }
    ;
characterset_list:
    | characterset characterset_list { $1 :: $2 }
    | characterset                   { [$1]     }
    ;
characterset:
    | INTEGER DASH CHAR optional_step
                        { P.Range (int_of_string $1, None, $4) }
    | INTEGER DASH INTEGER optional_step
                        { P.Range (int_of_string $1, Some (int_of_string $3), $4) }
    | INTEGER           { P.Single (int_of_string $1) }
    | IDENT             { P.Name $1 }
    | SINGLEQUOTED      { P.Name $1 }
    | CHAR              { P.Name (String.make 1 $1) }
    | DNA               { P.Name "DNA" }
    | RNA               { P.Name "RNA" }
    | PROTEIN           { P.Name "PROTEIN" }
    ;
optional_step:
    | SLASH INTEGER { int_of_string $2 }
    |               { 1                }

/* -------------------------------------------------------------------------- */
/* Entry of the Tree Parser                                                   */
tree:
    | do_star IDENT EQUAL single_tree EOF { ($2,$4) }
    ;
single_tree:
    | nexus_ident optional_length optional_comment
        { P.Leaf ($1, ($2, $3))         }
    | LPARENT single_tree_list RPARENT optional_label optional_length optional_comment
        { P.Node ($2, $4, ($5, $6))     }
    ;
single_tree_list:
    | single_tree COMMA single_tree_list { $1 :: $3 }
    | single_tree                        { [$1]     }
    ;
optional_length:
    | COLON INTEGER { Some (float_of_string $2) }
    | COLON FLOAT   { Some (float_of_string $2) }
    |               { None                      }
    ;
optional_comment:
    | LBRACKET nexus_ident RBRACKET { Some $2 }
    |                         { None    }
    ;
optional_label:
    | nexus_ident { Some $1   }
    | FLOAT       { None      } /* this is for phylip trees w/ (x,y)0.90:0.019 */
    |             { None      }
    ;

trees:
    | do_star nexus_word EQUAL single_tree SEMICOLON trees {($2,$4)::$6}
    | do_star single_tree SEMICOLON trees                  {("",$2)::$4}
    | EOF                                                  {  []       }

/* -------------------------------------------------------------------------- */
any_thing_minus_end:
    | TAXA  { () }
    | DATA  { () }
    | UNALIGNED  { () }
    | TREES  { () }
    | NOTES  { () }
    | DISTANCES  { () }
    | ASSUMPTIONS  { () }
    | SETS  { () }
    | CHARACTERS { () }
    | ANCSTATES { () }
    | AVERAGE { () }
    | BINHEX { () }
    | BOTH { () }
    | CHANGESET { () }
    | CHAR { () }
    | CHARACTER { () }
    | CHARLABELS { () }
    | CHARPARTITION { () }
    | CHARSTATELABELS { () }
    | CODEORDER { () }
    | CODESET { () }
    | CODONS { () }
    | CONDONPOSSET { () }
    | CONTINUOUS { () }
    | COUNT { () }
    | CSTREE { () }
    | QUOTED { () }
    | DATATYPE { () }
    | DEFTYPE { () }
    | DIAGONAL { () }
    | DIMENSIONS { () }
    | DNA { () }
    | DROS { () }
    | ELIMINATE { () }
    | ENCODE { () }
    | EPS { () }
    | EQUATE { () }
    | EXSET { () }
    | EXT { () }
    | EXTENSIONS { () }
    | FILE { () }
    | FORMAT { () }
    | FREQUENCY { () }
    | GAP { () }
    | GENETICCODE { () }
    | GIF { () }
    | INDIVIDUALS { () }
    | INLINE { () }
    | INTERLEAVE { () }
    | ITEMS { () }
    | JPEG { () }
    | LABELS { () }
    | LOWER { () }
    | MAM { () }
    | MATCHCHAR { () }
    | MAX { () }
    | MAXSTEPS { () }
    | MEDIAN { () }
    | MIN { () }
    | MINSTEPS { () }
    | MISSING { () }
    | MTDNA { () }
    | NCHAR { () }
    | NEWSTATE { () }
    | NEWTAXA { () }
    | NONE { () }
    | NTAX { () }
    | NUCLEOTIDE { () }
    | NUCORDER { () }
    | OPTIONS { () }
    | PICT { () }
    | PICTURE { () }
    | POLYTCOUNT { () }
    | PROTEIN { () }
    | RESOURCE { () }
    | RESPECTCASE { () }
    | RNA { () }
    | SAMPLESIZE { () }
    | SOURCE { () }
    | STANDARD { () }
    | STATE { () }
    | STATELABELS { () }
    | STATES { () }
    | STATESET { () }
    | STATESFORMAT { () }
    | STATESPRESENT { () }
    | STDERROR { () }
    | STEPMATRIX { () }
    | SYMBOLS { () }
    | TAXON { () }
    | TAXLABELS { () }
    | TAXPARTITION { () }
    | TAXSET { () }
    | TEXT { () }
    | TIFF { () }
    | TOKENS { () }
    | TRANSLATE { () }
    | TRANSPOSE { () }
    | TREE { () }
    | TREEPARTITION { () }
    | TREESET { () }
    | TRIANGLE { () }
    | TYPESET { () }
    | UNIVERSAL { () }
    | UPPER { () }
    | USERTYPE { () }
    | UUENCODE { () }
    | VARIANCE { () }
    | VECTOR { () }
    | WTSET { () }
    | YEAST { () }
    | EIDENT { () }
    | STAR { () }
    | COLON { () }
    | IDENT { () }
    | FLOAT { () }
    | INTEGER { () }
    | SEMICOLON  { () }
    | EQUAL { () }
    | COMMA { () }
    | QUOTE { () }
    | BACKSLASH { () }
    | DASH { () }
    | LPARENT { () }
    | RPARENT { () }
    | SINGLEQUOTED { () }
    ;
