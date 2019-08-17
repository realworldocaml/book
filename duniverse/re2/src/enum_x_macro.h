/* This is the X-macro trick.  Define X to expand to whatever code is
 * needed (a C enum, an ML variant, a C case statement, &c.) so that
 * the compiler enforces exhaustiveness and synchronization between C
 * and ML.
 *
 * For a description, see
 * "The New C: X Macros" by Randy Meyers, Dr. Dobb's, 01 May 2001,
 * retrieved 11 Sep 2012 from drdobbs.com/184401387
 */
/* All the macros have three standard arguments:
   - first letter of upper-case identifier: to generate ML variants
   - first letter of lower-case identifier: to concatenate into C++ method calls
     (e.g., set_encoding, not set_Encoding) because C preprocessor
     cannot lowercase or uppercase its tokens
   - rest of identifier
   - default: for documentation in the mli

   The X__ENCODING macro has two more arguments:
   - suffix: the C++ field name is encoding (with values EncodingUTF8
     or EncodingLatin1) but the ML and bindings-layer name is
     encoding_latin1 (with boolean values)
   - deserialization translation: from bools to enums
   - serialization translation: from enums to bools
*/

X__ENCODING(E,e,ncoding,false,_latin1,  == RE2::Options::EncodingLatin1, ? RE2::Options::EncodingLatin1 : RE2::Options::EncodingUTF8)
X(P,p,osix_syntax,false)
X(L,l,ongest_match,false)
X(L,l,og_errors,false (differs from re2 defaults!))
X__MAXMEM(M,m,ax_mem,something reasonable)
X(L,l,iteral,false)
X(N,n,ever_nl,false)
X(D,d,ot_nl,false)
X(N,n,ever_capture,false)
X(C,c,ase_sensitive,true)
X(P,p,erl_classes,false)
X(W,w,ord_boundary,false)
X(O,o,ne_line,false)
#undef X__MAXMEM
#undef X__ENCODING
#undef X
