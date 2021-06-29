///////// BIN_PROT

//Provides: caml_check_bound_bigstring
//Requires: caml_array_bound_error
function caml_check_bound_bigstring(bigstring, i){
  if (i >>> 0 >= bigstring.data.length) caml_array_bound_error();
}

//Provides: bin_prot_blit_buf_float_array_stub
//Requires: caml_check_bound, caml_check_bound_bigstring
function bin_prot_blit_buf_float_array_stub(src_pos, src, dst_pos, dst, len){
  if(len == 0) return 0;
  caml_check_bound(dst,dst_pos); // first pos
  caml_check_bound(dst,dst_pos+len-1); // last pos
  caml_check_bound_bigstring(src, src_pos);  /* first pos */
  caml_check_bound_bigstring(src, src_pos + len * 8 - 1); /* last pos */
  var view = new joo_global_object.Float64Array(len);
  var buffer = new joo_global_object.Uint8Array(view.buffer);
  buffer.set(src.data.subarray(src_pos, src_pos + (len * 8)));
  for(var i = 0; i < len; i++){
    // [+ 1] because the tag is at pos 0 
    dst[dst_pos+i+1] = view[i];
  }
  return 0
}
//Provides: bin_prot_blit_buf_bytes_stub
//Requires: caml_bigstring_blit_ba_to_bytes
function bin_prot_blit_buf_bytes_stub(src_pos, src, dst_pos, dst, len){
  return caml_bigstring_blit_ba_to_bytes(src, src_pos, dst, dst_pos, len);
}
//Provides: bin_prot_blit_float_array_buf_stub
//Requires: caml_check_bound, caml_check_bound_bigstring
function bin_prot_blit_float_array_buf_stub(src_pos,src, dst_pos, dst, len){
  if(len == 0) return 0 
  caml_check_bound (src, src_pos); // first pos 
  caml_check_bound (src, src_pos + len - 1); // last pos
  caml_check_bound_bigstring(dst, dst_pos); /* first pos */
  caml_check_bound_bigstring(dst, dst_pos + len * 8 - 1); /* last pos */
  // [+ 1] because the tag is at pos 0
  src_pos = src_pos + 1
  var float64 = new joo_global_object.Float64Array(src.slice(src_pos,src_pos + len));
  var float64_uint8 = new joo_global_object.Uint8Array(float64.buffer);
  var view = dst.data.subarray(dst_pos, dst_pos + (len * 8));
  view.set(float64_uint8);
  return 0
}
//Provides: bin_prot_blit_string_buf_stub
//Requires: caml_bigstring_blit_string_to_ba
function bin_prot_blit_string_buf_stub (src_pos, src, dst_pos, dst, len){
  return caml_bigstring_blit_string_to_ba(src,src_pos, dst, dst_pos,len);
}
//Provides: bin_prot_blit_bytes_buf_stub
//Requires: caml_bigstring_blit_string_to_ba
function bin_prot_blit_bytes_buf_stub (src_pos, src, dst_pos, dst, len){
  return caml_bigstring_blit_string_to_ba(src,src_pos, dst, dst_pos,len);
}

//Provides: bin_prot_blit_buf_stub
//Requires: caml_bigstring_blit_ba_to_ba, bigstring_of_typed_array
function bin_prot_blit_buf_stub (src_pos, src, dst_pos, dst, len){
  // [bin_prot_blit_buf_stub] is used with mixed bigarray kinds.
  // Converter everything to bigarray of char before the blit.
  if(src.kind != 12) // 12 is the char kind
    src = bigstring_of_typed_array(src.data);
  if(dst.kind != 12) // 12 is the char kind
    dst = bigstring_of_typed_array(dst.data);
  return caml_bigstring_blit_ba_to_ba(src,src_pos,dst,dst_pos,len);
}
