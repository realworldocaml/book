(**
   The Yojson library provides several types for representing JSON values, with different use cases.

   - The {{!basic}Basic} JSON type,
   - The {{!safe}Safe} JSON type, a superset of JSON with safer support for integers,
   - The {{!raw}Raw} JSON type, a superset of JSON, safer but less integrated with OCaml types.

Each of these different types have their own module.

*)

(** {1 Shared types and functions} *)

#include "common.mli"

(** {1:basic Basic JSON tree type} *)

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
(** This module provides combinators for extracting fields from JSON values. *)
module Util :
sig
  #include "util.mli"
end
#undef INT
#undef FLOAT
#undef STRING
end

(** {1:safe Multipurpose JSON tree type} *)

module Safe :
sig
(**
   This module supports a specific syntax for variants and tuples
   in addition to the standard JSON nodes.
   Arbitrary integers are supported and represented as a decimal string 
   using [`Intlit] when they cannot be represented using OCaml's int type
   (31 or 63 bits depending on the platform).

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
(** This module provides combinators for extracting fields from JSON values. *)
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

(** {1:raw Supertype of all JSON tree types} *)

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
