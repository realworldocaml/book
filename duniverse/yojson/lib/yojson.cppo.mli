(**
   The Yojson library provides runtime functions for reading and writing JSON
   data from OCaml. It addresses a few shortcomings of its predecessor
   json-wheel and is about twice as fast (2.7x reading, 1.3x writing; results
   may vary).
   The design goals of Yojson are the following:
   - Reducing inter-package dependencies by the use of polymorphic
   variants for the JSON tree type.
   - Allowing type-aware serializers/deserializers 
   to read and write directly without going through a generic JSON tree,
   for efficiency purposes.
   Readers and writers of all JSON syntaxic elements are provided
   but are undocumented and meant to be used by generated OCaml code.
   - Distinguishing between ints and floats.
   - Providing optional extensions of the JSON syntax.
   These extensions include comments, arbitrary strings,
   optional quotes around field names, tuples and variants.
   
   @author Martin Jambon
   @see <http://json.org> JSON specification
 *)

(** {1 Shared types and functions} *)

#include "common.mli"

(** {1 Basic JSON tree type} *)

module Basic :
sig
(**
   This module supports standard JSON nodes only, i.e. no special syntax
   for variants or tuples as supported by {!Yojson.Safe}.
   Arbitrary integers are not supported as they must all fit within the
   standard OCaml int type (31 or 63 bits depending on the platform).

   The main advantage of this module is its simplicity.
*)

#define INT
#define FLOAT
#define STRING
#include "type.ml"
#include "write.mli"
#include "monomorphic.mli"
#include "write2.mli"
#include "read.mli"
module Util :
sig
  #include "util.mli"
end
#undef INT
#undef FLOAT
#undef STRING
end

(** {1 Multipurpose JSON tree type} *)

module Safe :
sig
(**
   This module supports a specific syntax for variants and tuples
   in addition to the standard JSON nodes.
   Arbitrary integers are supported and represented as a decimal string 
   using [`Intlit] when they cannot be represented using OCaml's int type.

   This module is recommended for intensive use 
   or OCaml-friendly use of JSON.
*)

#define INT
#define INTLIT
#define FLOAT
#define STRING
#define TUPLE
#define VARIANT
#include "type.ml"
#include "monomorphic.mli"
#include "safe.mli"
#include "write.mli"
#include "write2.mli"
#include "read.mli"
module Util :
sig
  #include "util.mli"
end
#undef INT
#undef INTLIT
#undef FLOAT
#undef STRING
#undef TUPLE
#undef VARIANT
end

(** {1 JSON tree type with literal int/float/string leaves} *)

module Raw :
sig
(**
   Ints, floats and strings literals are systematically preserved using
   [`Intlit], [`Floatlit] and [`Stringlit].
   This module also supports the specific syntax for variants and tuples
   supported by {!Yojson.Safe}.
*)

#define INTLIT
#define FLOATLIT
#define STRINGLIT
#define TUPLE
#define VARIANT
#include "type.ml"
#include "monomorphic.mli"
#include "write.mli"
#include "write2.mli"
#include "read.mli"
#undef INTLIT
#undef FLOATLIT
#undef STRINGLIT
#undef TUPLE
#undef VARIANT
end

(** {1 Supertype of all JSON tree types} *)

#define INT
#define INTLIT
#define FLOAT
#define FLOATLIT
#define STRING
#define STRINGLIT
#define TUPLE
#define VARIANT
#include "type.ml"
#include "monomorphic.mli"
type json_max = t
#include "write.mli"
#include "write2.mli"
#undef INT
#undef INTLIT
#undef FLOAT
#undef FLOATLIT
#undef STRING
#undef STRINGLIT
#undef TUPLE
#undef VARIANT

