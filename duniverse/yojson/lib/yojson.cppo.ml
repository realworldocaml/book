#include "common.ml"

#define INT
#define INTLIT
#define FLOAT
#define FLOATLIT
#define STRING
#define STRINGLIT
#define TUPLE
#define VARIANT
#include "type.ml"
type json_max = t
#include "write.ml"
#include "monomorphic.ml"
module Pretty =
struct
#include "pretty.ml"
end
#include "write2.ml"
#undef INT
#undef INTLIT
#undef FLOAT
#undef FLOATLIT
#undef STRING
#undef STRINGLIT
#undef TUPLE
#undef VARIANT

module Basic =
struct
#define INT
#define FLOAT
#define STRING
#include "type.ml"
#include "write.ml"
#include "monomorphic.ml"
#include "write2.ml"
#include "read.ml"
module Util =
struct
  #include "util.ml"
end
#undef INT
#undef FLOAT
#undef STRING
end

module Safe =
struct
#define INT
#define INTLIT
#define FLOAT
#define STRING
#define TUPLE
#define VARIANT
#include "type.ml"
#include "safe.ml"
#include "write.ml"
#include "monomorphic.ml"
#include "write2.ml"
#include "read.ml"
module Util =
struct
  #include "util.ml"
end
#undef INT
#undef INTLIT
#undef FLOAT
#undef STRING
#undef TUPLE
#undef VARIANT
end

module Raw =
struct
#define INTLIT
#define FLOATLIT
#define STRINGLIT
#define TUPLE
#define VARIANT
#include "type.ml"
#include "write.ml"
#include "monomorphic.ml"
#include "write2.ml"
#include "read.ml"
#undef INTLIT
#undef FLOATLIT
#undef STRINGLIT
#undef TUPLE
#undef VARIANT
end
