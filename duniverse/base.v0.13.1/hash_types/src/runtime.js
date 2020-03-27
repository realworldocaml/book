//Provides: Base_internalhash_fold_int64
//Requires: caml_hash_mix_int64
var Base_internalhash_fold_int64 = caml_hash_mix_int64;
//Provides: Base_internalhash_fold_int
//Requires: caml_hash_mix_int
var Base_internalhash_fold_int = caml_hash_mix_int;
//Provides: Base_internalhash_fold_float
//Requires: caml_hash_mix_float
var Base_internalhash_fold_float = caml_hash_mix_float;
//Provides: Base_internalhash_fold_string
//Requires: caml_hash_mix_string
var Base_internalhash_fold_string = caml_hash_mix_string;
//Provides: Base_internalhash_get_hash_value
//Requires: caml_hash_mix_final
function Base_internalhash_get_hash_value(seed) {
  var h = caml_hash_mix_final(seed);
  return h & 0x3FFFFFFF;
}
