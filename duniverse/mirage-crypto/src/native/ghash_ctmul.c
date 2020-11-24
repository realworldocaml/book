/*
 * Copyright (c) 2016 Thomas Pornin <pornin@bolet.org>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * We compute "carryless multiplications" through normal integer
 * multiplications, masking out enough bits to create "holes" in which
 * carries may expand without altering our bits; we really use 8 data
 * bits per 32-bit word, spaced every fourth bit. Accumulated carries
 * may not exceed 8 in total, which fits in 4 bits.
 *
 * It would be possible to use a 3-bit spacing, allowing two operands,
 * one with 7 non-zero data bits, the other one with 10 or 11 non-zero
 * data bits; this asymmetric splitting makes the overall code more
 * complex with thresholds and exceptions, and does not appear to be
 * worth the effort.
 */

#include "mirage_crypto.h"
#include <string.h>

#if defined (__i386__) || defined (__arm__)

/*
 * We cannot really autodetect whether multiplications are "slow" or
 * not. A typical example is the ARM Cortex M0+, which exists in two
 * versions: one with a 1-cycle multiplication opcode, the other with
 * a 32-cycle multiplication opcode. They both use exactly the same
 * architecture and ABI, and cannot be distinguished from each other
 * at compile-time.
 *
 * Since most modern CPU (even embedded CPU) still have fast
 * multiplications, we use the "fast mul" code by default.
 */

/*
 * Simple multiplication in GF(2)[X], using 16 integer multiplications.
 */

/*
 * Multiply two 32-bit integers, with a 64-bit result. This default
 * implementation assumes that the basic multiplication operator
 * yields constant-time code.
 */
#define MUL(x, y)   ((uint64_t)(x) * (uint64_t)(y))

static inline void
br_enc32be(void *dst, uint32_t x)
{
	unsigned char *buf;

	buf = dst;
	buf[0] = (unsigned char)(x >> 24);
	buf[1] = (unsigned char)(x >> 16);
	buf[2] = (unsigned char)(x >> 8);
	buf[3] = (unsigned char)x;
}

static inline uint32_t
br_dec32be(const void *src)
{
	const unsigned char *buf;

	buf = src;
	return ((uint32_t)buf[0] << 24)
		| ((uint32_t)buf[1] << 16)
		| ((uint32_t)buf[2] << 8)
		| (uint32_t)buf[3];
}

static inline void
bmul(uint32_t *hi, uint32_t *lo, uint32_t x, uint32_t y)
{
	uint32_t x0, x1, x2, x3;
	uint32_t y0, y1, y2, y3;
	uint64_t z0, z1, z2, z3;
	uint64_t z;

	x0 = x & (uint32_t)0x11111111;
	x1 = x & (uint32_t)0x22222222;
	x2 = x & (uint32_t)0x44444444;
	x3 = x & (uint32_t)0x88888888;
	y0 = y & (uint32_t)0x11111111;
	y1 = y & (uint32_t)0x22222222;
	y2 = y & (uint32_t)0x44444444;
	y3 = y & (uint32_t)0x88888888;
	z0 = MUL(x0, y0) ^ MUL(x1, y3) ^ MUL(x2, y2) ^ MUL(x3, y1);
	z1 = MUL(x0, y1) ^ MUL(x1, y0) ^ MUL(x2, y3) ^ MUL(x3, y2);
	z2 = MUL(x0, y2) ^ MUL(x1, y1) ^ MUL(x2, y0) ^ MUL(x3, y3);
	z3 = MUL(x0, y3) ^ MUL(x1, y2) ^ MUL(x2, y1) ^ MUL(x3, y0);
	z0 &= (uint64_t)0x1111111111111111;
	z1 &= (uint64_t)0x2222222222222222;
	z2 &= (uint64_t)0x4444444444444444;
	z3 &= (uint64_t)0x8888888888888888;
	z = z0 | z1 | z2 | z3;
	*lo = (uint32_t)z;
	*hi = (uint32_t)(z >> 32);
}

/* see bearssl_hash.h */
void
br_ghash_ctmul(void *y, const void *h, const void *data, size_t len)
{
	const unsigned char *buf, *hb;
	unsigned char *yb;
	uint32_t yw[4];
	uint32_t hw[4];

	/*
	 * Throughout the loop we handle the y and h values as arrays
	 * of 32-bit words.
	 */
	buf = data;
	yb = y;
	hb = h;
	yw[3] = br_dec32be(yb);
	yw[2] = br_dec32be(yb + 4);
	yw[1] = br_dec32be(yb + 8);
	yw[0] = br_dec32be(yb + 12);
	hw[3] = br_dec32be(hb);
	hw[2] = br_dec32be(hb + 4);
	hw[1] = br_dec32be(hb + 8);
	hw[0] = br_dec32be(hb + 12);
	while (len > 0) {
		const unsigned char *src;
		unsigned char tmp[16];
		int i;
		uint32_t a[9], b[9], zw[8];
		uint32_t c0, c1, c2, c3, d0, d1, d2, d3, e0, e1, e2, e3;

		/*
		 * Get the next 16-byte block (using zero-padding if
		 * necessary).
		 */
		if (len >= 16) {
			src = buf;
			buf += 16;
			len -= 16;
		} else {
			memcpy(tmp, buf, len);
			memset(tmp + len, 0, (sizeof tmp) - len);
			src = tmp;
			len = 0;
		}

		/*
		 * Decode the block. The GHASH standard mandates
		 * big-endian encoding.
		 */
		yw[3] ^= br_dec32be(src);
		yw[2] ^= br_dec32be(src + 4);
		yw[1] ^= br_dec32be(src + 8);
		yw[0] ^= br_dec32be(src + 12);

		/*
		 * We multiply two 128-bit field elements. We use
		 * Karatsuba to turn that into three 64-bit
		 * multiplications, which are themselves done with a
		 * total of nine 32-bit multiplications.
		 */

		/*
		 * y[0,1]*h[0,1] -> 0..2
		 * y[2,3]*h[2,3] -> 3..5
		 * (y[0,1]+y[2,3])*(h[0,1]+h[2,3]) -> 6..8
		 */
		a[0] = yw[0];
		b[0] = hw[0];
		a[1] = yw[1];
		b[1] = hw[1];
		a[2] = a[0] ^ a[1];
		b[2] = b[0] ^ b[1];

		a[3] = yw[2];
		b[3] = hw[2];
		a[4] = yw[3];
		b[4] = hw[3];
		a[5] = a[3] ^ a[4];
		b[5] = b[3] ^ b[4];

		a[6] = a[0] ^ a[3];
		b[6] = b[0] ^ b[3];
		a[7] = a[1] ^ a[4];
		b[7] = b[1] ^ b[4];
		a[8] = a[6] ^ a[7];
		b[8] = b[6] ^ b[7];

		for (i = 0; i < 9; i ++) {
			bmul(&b[i], &a[i], b[i], a[i]);
		}

		c0 = a[0];
		c1 = b[0] ^ a[2] ^ a[0] ^ a[1];
		c2 = a[1] ^ b[2] ^ b[0] ^ b[1];
		c3 = b[1];
		d0 = a[3];
		d1 = b[3] ^ a[5] ^ a[3] ^ a[4];
		d2 = a[4] ^ b[5] ^ b[3] ^ b[4];
		d3 = b[4];
		e0 = a[6];
		e1 = b[6] ^ a[8] ^ a[6] ^ a[7];
		e2 = a[7] ^ b[8] ^ b[6] ^ b[7];
		e3 = b[7];

		e0 ^= c0 ^ d0;
		e1 ^= c1 ^ d1;
		e2 ^= c2 ^ d2;
		e3 ^= c3 ^ d3;
		c2 ^= e0;
		c3 ^= e1;
		d0 ^= e2;
		d1 ^= e3;

		/*
		 * GHASH specification has the bits "reversed" (most
		 * significant is in fact least significant), which does
		 * not matter for a carryless multiplication, except that
		 * the 255-bit result must be shifted by 1 bit.
		 */
		zw[0] = c0 << 1;
		zw[1] = (c1 << 1) | (c0 >> 31);
		zw[2] = (c2 << 1) | (c1 >> 31);
		zw[3] = (c3 << 1) | (c2 >> 31);
		zw[4] = (d0 << 1) | (c3 >> 31);
		zw[5] = (d1 << 1) | (d0 >> 31);
		zw[6] = (d2 << 1) | (d1 >> 31);
		zw[7] = (d3 << 1) | (d2 >> 31);

		/*
		 * We now do the reduction modulo the field polynomial
		 * to get back to 128 bits.
		 */
		for (i = 0; i < 4; i ++) {
			uint32_t lw;

			lw = zw[i];
			zw[i + 4] ^= lw ^ (lw >> 1) ^ (lw >> 2) ^ (lw >> 7);
			zw[i + 3] ^= (lw << 31) ^ (lw << 30) ^ (lw << 25);
		}
		memcpy(yw, zw + 4, sizeof yw);
	}

	/*
	 * Encode back the result.
	 */
	br_enc32be(yb, yw[3]);
	br_enc32be(yb + 4, yw[2]);
	br_enc32be(yb + 8, yw[1]);
	br_enc32be(yb + 12, yw[0]);
}

CAMLprim value mc_ghash_key_size_generic (__unit ()) {
  //return size for m below
  return Val_int(16);
}

static inline void __copy (uint64_t key[2], uint32_t m[4]) {
  m[0] = key[0] & 0xFFFFFFFF;
  m[1] = key[0] >> 32;
  m[2] = key[1] & 0xFFFFFFFF;
  m[3] = key[1] >> 32;
}

CAMLprim value mc_ghash_init_key_generic (value key, value off, value m) {
  //push key at off into m
  __copy ((uint64_t *) _ba_uint8_off(key, off), (uint32_t *) m);
  return Val_unit;
}

CAMLprim value mc_ghash_generic (value m, value hash, value src, value off, value len) {
  br_ghash_ctmul(Bp_val(hash), Bp_val(m), _ba_uint8_off(src, off), Int_val(len));
  return Val_unit;
}

#endif /* i386 || arm */
