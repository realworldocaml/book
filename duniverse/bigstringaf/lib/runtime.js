/*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*/

//Provides: bigstringaf_blit_to_bytes
//Requires: caml_bigstring_blit_ba_to_bytes
function bigstringaf_blit_to_bytes(src, src_off, dst, dst_off, len) {
  return caml_bigstring_blit_ba_to_bytes(src,src_off,dst,dst_off,len);
}

//Provides: bigstringaf_blit_to_bigstring
//Requires: caml_bigstring_blit_ba_to_ba
function bigstringaf_blit_to_bigstring(src, src_off, dst, dst_off, len) {
  return caml_bigstring_blit_ba_to_ba(src, src_off, dst, dst_off, len);
}

//Provides: bigstringaf_blit_from_bytes
//Requires: caml_bigstring_blit_string_to_ba
function bigstringaf_blit_from_bytes(src, src_off, dst, dst_off, len) {
  return caml_bigstring_blit_string_to_ba(src, src_off, dst, dst_off, len);
}

//Provides: bigstringaf_memcmp_bigstring
//Requires: caml_ba_get_1, caml_int_compare
function bigstringaf_memcmp_bigstring(ba1, ba1_off, ba2, ba2_off, len) {
  for (var i = 0; i < len; i++) {
    var c = caml_int_compare(caml_ba_get_1(ba1, ba1_off + i), caml_ba_get_1(ba2, ba2_off + i));
    if (c != 0) return c
  }
  return 0;
}

//Provides: bigstringaf_memcmp_string
//Requires: caml_ba_get_1, caml_int_compare, caml_string_unsafe_get
function bigstringaf_memcmp_string(ba, ba_off, str, str_off, len) {
  for (var i = 0; i < len; i++) {
    var c = caml_int_compare(caml_ba_get_1(ba, ba_off + i), caml_string_unsafe_get(str, str_off + i));
    if (c != 0) return c
  }
  return 0;
}

//Provides: bigstringaf_memchr
//Requires: caml_ba_get_1
function bigstringaf_memchr(ba, ba_off, chr, len) {
  for (var i = 0; i < len; i++) {
    if (caml_ba_get_1(ba, ba_off + i) == chr) {
      return (ba_off + i);
    }
  }
  return -1;
}
