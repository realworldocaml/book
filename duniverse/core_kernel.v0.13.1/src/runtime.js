///////// CORE_KERNEL

//Provides: core_array_unsafe_int_blit
//Requires: caml_array_blit
var core_array_unsafe_int_blit = caml_array_blit
//Provides: core_array_unsafe_float_blit
//Requires: caml_array_blit
var core_array_unsafe_float_blit = caml_array_blit

//Provides: core_kernel_time_ns_format
//Requires: caml_to_js_string, caml_js_to_string
function core_kernel_time_ns_format(time,format){
  var d = new Date(time * 1000);
  var formatjs = caml_to_js_string(format);
  var jstring = joo_global_object.strftime(formatjs, d);
  return caml_js_to_string(jstring);
}

//Provides: core_kernel_gc_compactions
function core_kernel_gc_compactions () { return 0 }
//Provides: core_kernel_gc_heap_chunks
function core_kernel_gc_heap_chunks () { return 0 }
//Provides: core_kernel_gc_heap_words
function core_kernel_gc_heap_words () { return 0 }
//Provides: core_kernel_gc_major_collections
function core_kernel_gc_major_collections () { return 0 }
//Provides: core_kernel_gc_major_plus_minor_words
function core_kernel_gc_major_plus_minor_words () { return 0 }
//Provides: core_kernel_gc_major_words
function core_kernel_gc_major_words () { return 0 }
//Provides: core_kernel_gc_minor_collections
function core_kernel_gc_minor_collections () { return 0 }
//Provides: core_kernel_gc_minor_words
function core_kernel_gc_minor_words () { return 0 }
//Provides: core_kernel_gc_promoted_words
function core_kernel_gc_promoted_words () { return 0 }
//Provides: core_kernel_gc_top_heap_words
function core_kernel_gc_top_heap_words () { return 0 }

//Provides: Core_kernel_heap_block_is_heap_block
function Core_kernel_heap_block_is_heap_block(x){
  return +(x instanceof Array);
}

//Provides: core_md5_fd
//Requires: caml_ml_open_descriptor_in, caml_md5_chan, caml_ml_close_channel
function core_md5_fd(fd){
    var ic = caml_ml_open_descriptor_in(fd);
    try {
        return caml_md5_chan(ic, -1);
    } finally {
        caml_ml_close_channel(ic);
    }
}

// Provides: core_md5_digest_subbigstring
// Requires: caml_md5_string, caml_blit_string, caml_create_bytes, bigstring_blit_bigstring_bytes_stub
function core_md5_digest_subbigstring(buf, ofs, len, res){
    var bytes = caml_create_bytes(len);
    bigstring_blit_bigstring_bytes_stub(buf, ofs, bytes, 0, len);
    var res2 = caml_md5_string(bytes, 0, len);
    caml_blit_string(res2, 0, res, 0, 16);
    return 0;
}

//Bigstring

//Provides: bigstring_destroy_stub
//Requires: caml_invalid_argument, caml_ba_create_from
function bigstring_destroy_stub(v_bstr) {
  if (v_bstr.data2 != null) {
      caml_invalid_argument("bigstring_destroy: unsupported kind");
  }

  if (v_bstr.hasOwnProperty('__is_deallocated')) {
    caml_invalid_argument("bigstring_destroy: bigstring is already deallocated");
  }

  var destroyed_data = new v_bstr.data.__proto__.constructor(0);
  var destroyed_bigstring =
      caml_ba_create_from(destroyed_data, null, v_bstr.data_type, v_bstr.kind,
                          v_bstr.layout, [0]);
  destroyed_bigstring.__is_deallocated = true;

  // Mutate the original bigstring in-place, to simulate what the C version does
  Object.assign(v_bstr, destroyed_bigstring);

  return 0;
}

//Provides: bigstring_realloc
//Requires: caml_invalid_argument, caml_ba_create_from, bigstring_destroy_stub
function bigstring_realloc(bigstring, size) {
    if (bigstring.data2 != null) {
        caml_invalid_argument("Bigstring.unsafe_destroy_and_resize: unsupported kind");
    }

    if (bigstring.hasOwnProperty('__is_deallocated')) {
        caml_invalid_argument("bigstring_realloc: bigstring is already deallocated");
    }

    var new_data = new bigstring.data.__proto__.constructor(size);
    new_data.set(bigstring.data.slice(0, size));
    var new_bigstring =
        caml_ba_create_from(new_data, null, bigstring.data_type, bigstring.kind,
                            bigstring.layout, [size]);

    bigstring_destroy_stub(bigstring);

    return new_bigstring;
}
