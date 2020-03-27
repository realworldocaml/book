/* -*-indented-text-*- ----------------------------------------------


    Copyright (c) 1999 Christian Lindig <lindig@ips.cs.tu-bs.de>. All
    rights reserved. See COPYING for details.
    $Id: xmlparse.mly,v 1.12 1999/01/12 20:25:54 lindig Exp $

    This file implements a parser for XML files.
    See: http://www.w3.org/

    A XML file mixes markup data, comments, processing instructions
    (pi) and character data. Since the different kinds of content
    are indistinguishable by the scanner the parser controlls
    so-called scanner contexts.
*/

%{

(* Helpers *)

open Xml		(* XML abstract syntax 	*)
open Error		(* error() 		*)
open Xmlstate		(* setContext()		*)

let n    = None		(* just to save space 	*)

%}

/* tokens with value */

%token <string>         WORD
%token <string>         CHUNK
%token <string>         NAME
%token <string>         STRING
%token <string>         PIOPEN
%token <string>         OPEN
%token <string>         OPENSLASH

/* token */

%token CLOSE
%token COMMENT
%token DOCTYPE
%token DTDCLOSE
%token DTDOPEN
%token ENCODING
%token EOF
%token EQ
%token ERROR
%token PICLOSE
%token PUBLIC
%token S
%token SLASHCLOSE
%token STANDALONE
%token SYSTEM
%token VERSION
%token XMLCLOSE
%token XMLDECL
%token XMLNAME
%token XMLOPEN

%start document
%type <Xml.document> document

%%

document        : prolog topelement misc EOF{ XML($1,$2,$3) }

topelement      : element                   { setContext DataContext;
                                              $1
                                            }
                                            /* xm dt pi */
prolog          : xmldecl misc              { Prolog($1,None    ,$2   ) }
                | xmldecl misc doctype misc { Prolog($1,Some($3),$2@$4) }
                |         misc doctype misc { Prolog(n ,Some($2),$1@$3) }
                |         misc              { Prolog(n ,None    ,$1   ) }

misc            : /**/                      {     [] }
                | misc pi                   { $2::$1 }
                | misc CHUNK                {     $1 }
                | misc COMMENT              {     $1 }

dtdopen         : DTDOPEN                   { setContext DeclContext}
dtdclose        : DTDCLOSE                  { setContext DataContext }

doctype         : dtdopen NAME ext markup
                  dtdclose                  { DTD($2,$3) }

ext             : /**/                      { None }
                | SYSTEM STRING             { Some (DTDsys($2))    }
                | PUBLIC STRING STRING      { Some (DTDpub($2,$3)) }

markup          : /**/                      { None }
                | error                     { error "DTDs are unsupported" }

element         : emptyElemTag              { let (n,a) = $1 in
                                                single n a
                                            }

                | sTag content eTag         {   let (sn,a) = $1 in
                                                let  en    = $3 in
                                                let  c     = $2 in
                                                  if sn = en then
                                                  element sn a c
                                                  else error ("tag mismatch")
                                            }

opn             : OPEN                      { setContext ElementContext; $1 }
opnslash        : OPENSLASH                 { setContext ElementContext; $1 }
cls             : CLOSE                     { setContext DataContext  }
slashcls        : SLASHCLOSE                { setContext DataContext  }

sTag            : opn attributes cls        { ($1,$2) }
eTag            : opnslash cls              {  $1     }
emptyElemTag    : opn attributes slashcls   { ($1,$2) }

attributes      : /**/                      {     []  }
                | attributes attribute      { $2::$1  }

attribute       : NAME EQ STRING            { ($1,$3) }

content         : /**/                      { empty                    	}
                | content CHUNK             { $1 ^^ chunk $2     	}
                | content element           { $1 ^^ $2            	}
                | content pi                { match $2 with
                                              name,strings ->
                                                $1 ^^ pi name strings   }
                | content COMMENT           { $1                        }

xmlopen         : XMLOPEN                   { setContext DeclContext}
xmlclose        : XMLCLOSE                  { setContext DataContext }

xmlinfo         : version encoding sddecl   { ($1,$2,Some $3) }
                | version                   { ($1,n ,None   ) }
                | version encoding          { ($1,$2,None   ) }
                | version          sddecl   { ($1,n ,Some $2) }

xmldecl         : xmlopen xmlinfo xmlclose  { match $2 with
                                              (vers,enc,sa) ->
                                              Some (XMLDecl(
                                                    vers,       (* version *)
                                                    sa,         (* standalone *)
                                                    enc         (* encoding *)
                                                   ))
                                            }

version         : VERSION EQ STRING         { $3 }

encoding        : ENCODING EQ STRING        { Some $3 }

sddecl          : STANDALONE EQ STRING      { match $3 with
                                            | "yes" -> true
                                            | "no"  -> false
                                            | _     -> error "yes/no expected"
                                            }

piopen          : PIOPEN                    { setContext PiContext; $1}
pi              : piopen picontent PICLOSE  { setContext DataContext;
                                                  ($1,List.rev $2)
                                            }
picontent       : /**/                      { []        }
                | picontent WORD            { $2 :: $1  }

