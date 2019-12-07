include Bin_shape_lib.Std.Shape

(* new base shapes *)
let bin_shape_unit                  = basetype (Uuid.of_string "unit")               []
let bin_shape_bool                  = basetype (Uuid.of_string "bool")               []
let bin_shape_string                = basetype (Uuid.of_string "string")             []
let bin_shape_bytes                 = basetype (Uuid.of_string "bytes")              []
let bin_shape_char                  = basetype (Uuid.of_string "char")               []
let bin_shape_float                 = basetype (Uuid.of_string "float")              []
let bin_shape_int                   = basetype (Uuid.of_string "int")                []
let bin_shape_int32                 = basetype (Uuid.of_string "int32")              []
let bin_shape_int63                 = basetype (Uuid.of_string "int63")              []
let bin_shape_int64                 = basetype (Uuid.of_string "int64")              []
let bin_shape_nativeint             = basetype (Uuid.of_string "nativeint")          []
let bin_shape_nat0                  = basetype (Uuid.of_string "nat0")               []
let bin_shape_digest                = basetype (Uuid.of_string "digest")             []
let bin_shape_float32_vec           = basetype (Uuid.of_string "float32_vec")        []
let bin_shape_float64_vec           = basetype (Uuid.of_string "float64_vec")        []
let bin_shape_vec                   = basetype (Uuid.of_string "vec")                []
let bin_shape_float32_mat           = basetype (Uuid.of_string "float32_mat")        []
let bin_shape_float64_mat           = basetype (Uuid.of_string "float64_mat")        []
let bin_shape_mat                   = basetype (Uuid.of_string "mat")                []
let bin_shape_bigstring             = basetype (Uuid.of_string "bigstring")          []
let bin_shape_variant_int           = basetype (Uuid.of_string "variant_int")        []
let bin_shape_int_8bit              = basetype (Uuid.of_string "int_8bit")           []
let bin_shape_int_16bit             = basetype (Uuid.of_string "int_16bit")          []
let bin_shape_int_32bit             = basetype (Uuid.of_string "int_32bit")          []
let bin_shape_int_64bit             = basetype (Uuid.of_string "int_64bit")          []
let bin_shape_int64_bits            = basetype (Uuid.of_string "int64_bits")         []
let bin_shape_network16_int         = basetype (Uuid.of_string "network16_int")      []
let bin_shape_network32_int         = basetype (Uuid.of_string "network32_int")      []
let bin_shape_network32_int32       = basetype (Uuid.of_string "network32_int32")    []
let bin_shape_network64_int         = basetype (Uuid.of_string "network64_int")      []
let bin_shape_network64_int64       = basetype (Uuid.of_string "network64_int64")    []

(* new shape constructors *)
let bin_shape_ref x                 = basetype (Uuid.of_string "ref")    [x]
let bin_shape_option x              = basetype (Uuid.of_string "option") [x]
let bin_shape_list x                = basetype (Uuid.of_string "list")   [x]
let bin_shape_array x               = basetype (Uuid.of_string "array")  [x]
let bin_shape_hashtbl x y           = basetype (Uuid.of_string "hashtbl")[x;y]

(* shape alias *)
let bin_shape_float_array           = bin_shape_array bin_shape_float

(* shape-constructor aliases *)
let bin_shape_lazy x                = x
let bin_shape_pair x y              = tuple [x;y]
let bin_shape_triple x y z          = tuple [x;y;z]
