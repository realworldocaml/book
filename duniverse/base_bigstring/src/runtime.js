///////// BIGSTRING

//Provides: bigstring_alloc
//Requires: caml_ba_create
function bigstring_alloc(_,size){
  return caml_ba_create(12, 0, [0,size]);
}

//Provides: bigstring_is_mmapped_stub
function bigstring_is_mmapped_stub(x){
  return 0;
}

//Provides: bigstring_blit_stub
//Requires: caml_ba_get_1, caml_ba_set_1
function bigstring_blit_stub(s1, i1, s2, i2, len){
  for (var i = 0; i < len; i++) caml_ba_set_1(s2,i2 + i,caml_ba_get_1(s1,i1 + i));
  return 0;
}

//Provides: bigstring_blit_bytes_bigstring_stub
//Requires: caml_bytes_get, caml_ba_set_1
function bigstring_blit_bytes_bigstring_stub(v_str, v_src_pos, v_bstr, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_ba_set_1(v_bstr,v_dst_pos + i,caml_bytes_get(v_str,v_src_pos + i));
  return 0;
}

//Provides: bigstring_blit_bigstring_bytes_stub
//Requires: caml_bytes_set, caml_ba_get_1
function bigstring_blit_bigstring_bytes_stub(v_bstr, v_src_pos, v_str, v_dst_pos, v_len){
  for(var i = 0; i < v_len; i++){
    var c = caml_ba_get_1(v_bstr,v_src_pos + i);
    caml_bytes_set(v_str,v_dst_pos + i,c);
  }
  return 0;
}

//Provides: bigstring_blit_string_bigstring_stub
//Requires: caml_string_get, caml_ba_set_1
function bigstring_blit_string_bigstring_stub(v_str, v_src_pos, v_bstr, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_ba_set_1(v_bstr,v_dst_pos + i,caml_string_get(v_str,v_src_pos + i));
  return 0;
}

//Provides: bigstring_memset_stub
//Requires: caml_ba_set_1
function bigstring_memset_stub(bigstring, v_pos, v_len, v_char) {
  for (var i = 0; i < v_len; i++) {
    caml_ba_set_1(bigstring, v_pos + i, v_char);
  }
}

//Provides: bigstring_memcmp_stub
//Requires: caml_ba_get_1
function bigstring_memcmp_stub(v_s1, v_s1_pos, v_s2, v_s2_pos, v_len){
  for (var i = 0; i < v_len; i++) {
    var a = caml_ba_get_1(v_s1,v_s1_pos + i);
    var b = caml_ba_get_1(v_s2,v_s2_pos + i);
    if (a < b) return -1;
    if (a > b) return 1;
  }
  return 0;
}

//Provides: internalhash_fold_bigstring
//Requires: caml_hash_mix_bigstring
var internalhash_fold_bigstring = caml_hash_mix_bigstring

//Provides: bigstring_find
//Requires: caml_ba_get_1
function bigstring_find(bs, chr, pos, len){
  while(len > 0){
    if(caml_ba_get_1(bs,pos) == chr) return pos;
    pos++;
    len--;
  }
  return -1;
}
