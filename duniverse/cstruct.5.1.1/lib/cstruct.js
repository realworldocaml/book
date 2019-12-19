/*
 * Copyright (c) 2015 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

//Provides: caml_blit_bigstring_to_bigstring
//Requires: caml_bigstring_blit_ba_to_ba
var caml_blit_bigstring_to_bigstring = caml_bigstring_blit_ba_to_ba

//Provides: caml_blit_bigstring_to_string
//Requires: caml_bigstring_blit_ba_to_bytes
var caml_blit_bigstring_to_string = caml_bigstring_blit_ba_to_bytes

//Provides: caml_blit_string_to_bigstring
//Requires: caml_bigstring_blit_string_to_ba
var caml_blit_string_to_bigstring = caml_bigstring_blit_string_to_ba

//Provides: caml_compare_bigstring
//Requires: caml_int_compare, caml_ba_get_1
function caml_compare_bigstring(buf1, buf1_off, buf2, buf2_off, len) {
  var i, r;
  for (i = 0; i < len; i++) {
    r = caml_int_compare(caml_ba_get_1(buf1, buf1_off + i), caml_ba_get_1(buf2, buf2_off + i));
    if (r != 0) return r;
  }
  return 0;
}

//Provides: caml_fill_bigstring
//Requires: caml_ba_set_1
function caml_fill_bigstring(buf, buf_off, buf_len, v) {
  var i;
  for (i = 0; i < buf_len; i++) {
    caml_ba_set_1(buf, buf_off + i, v);
  }
  return 0;
}

//Provides: caml_check_alignment_bigstring
function caml_check_alignment_bigstring(buf, ofs, alignment) {
  return true; // FIXME: No concept of a fixed buffer address?
}
