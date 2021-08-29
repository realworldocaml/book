/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef BITFN_H
#define BITFN_H
#include <stdint.h>

# if (defined(__i386__))
#  define ARCH_HAS_SWAP32
static inline uint32_t bitfn_swap32(uint32_t a)
{
	__asm__ ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}
/**********************************************************/
# elif (defined(__arm__))
#  define ARCH_HAS_SWAP32
static inline uint32_t bitfn_swap32(uint32_t a)
{
	uint32_t tmp = a;
	__asm__ volatile ("eor %1, %0, %0, ror #16\n"
	                  "bic %1, %1, #0xff0000\n"
	                  "mov %0, %0, ror #8\n"
	                  "eor %0, %0, %1, lsr #8\n"
	                  : "=r" (a), "=r" (tmp) : "0" (a), "1" (tmp));
	return a;
}
/**********************************************************/
# elif defined(__x86_64__)
#  define ARCH_HAS_SWAP32
#  define ARCH_HAS_SWAP64
static inline uint32_t bitfn_swap32(uint32_t a)
{
	__asm__ ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}

static inline uint64_t bitfn_swap64(uint64_t a)
{
	__asm__ ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}

# endif

#ifndef ARCH_HAS_SWAP32
static inline uint32_t bitfn_swap32(uint32_t a)
{
	return (a << 24) | ((a & 0xff00) << 8) | ((a >> 8) & 0xff00) | (a >> 24);
}
#endif

#ifndef ARCH_HAS_SWAP64
static inline uint64_t bitfn_swap64(uint64_t a)
{
	return ((uint64_t) bitfn_swap32((uint32_t) (a >> 32))) |
	       (((uint64_t) bitfn_swap32((uint32_t) a)) << 32);
}
#endif

static inline uint32_t rol32(uint32_t word, uint32_t shift)
{
	return (word << shift) | (word >> (32 - shift));
}

static inline uint32_t ror32(uint32_t word, uint32_t shift)
{
	return (word >> shift) | (word << (32 - shift));
}

static inline uint64_t rol64(uint64_t word, uint32_t shift)
{
	return (word << shift) | (word >> (64 - shift));
}

static inline uint64_t ror64(uint64_t word, uint32_t shift)
{
	return (word >> shift) | (word << (64 - shift));
}

static inline void array_swap32(uint32_t *d, uint32_t *s, uint32_t nb)
{
	while (nb--)
		*d++ = bitfn_swap32(*s++);
}

static inline void array_swap64(uint64_t *d, uint64_t *s, uint32_t nb)
{
	while (nb--)
		*d++ = bitfn_swap64(*s++);
}

static inline void array_copy32(uint32_t *d, uint32_t *s, uint32_t nb)
{
	while (nb--) *d++ = *s++;
}

static inline void array_copy64(uint64_t *d, uint64_t *s, uint32_t nb)
{
	while (nb--) *d++ = *s++;
}

#ifdef __BYTE_ORDER__
#if __ORDER_LITTLE_ENDIAN__ == __BYTE_ORDER__

# define be32_to_cpu(a) bitfn_swap32(a)
# define cpu_to_be32(a) bitfn_swap32(a)
# define le32_to_cpu(a) (a)
# define cpu_to_le32(a) (a)
# define be64_to_cpu(a) bitfn_swap64(a)
# define cpu_to_be64(a) bitfn_swap64(a)
# define le64_to_cpu(a) (a)
# define cpu_to_le64(a) (a)

# define cpu_to_le32_array(d, s, l) array_copy32(d, s, l)
# define le32_to_cpu_array(d, s, l) array_copy32(d, s, l)
# define cpu_to_be32_array(d, s, l) array_swap32(d, s, l)
# define be32_to_cpu_array(d, s, l) array_swap32(d, s, l)

# define cpu_to_le64_array(d, s, l) array_copy64(d, s, l)
# define le64_to_cpu_array(d, s, l) array_copy64(d, s, l)
# define cpu_to_be64_array(d, s, l) array_swap64(d, s, l)
# define be64_to_cpu_array(d, s, l) array_swap64(d, s, l)

# define ARCH_IS_LITTLE_ENDIAN

#elif __ORDER_BIG_ENDIAN__ == __BYTE_ORDER__

# define be32_to_cpu(a) (a)
# define cpu_to_be32(a) (a)
# define le32_to_cpu(a) bitfn_swap32(a)
# define cpu_to_le32(a) bitfn_swap32(a)
# define be64_to_cpu(a) (a)
# define cpu_to_be64(a) (a)
# define le64_to_cpu(a) bitfn_swap64(a)
# define cpu_to_le64(a) bitfn_swap64(a)

# define cpu_to_le32_array(d, s, l) array_swap32(d, s, l)
# define le32_to_cpu_array(d, s, l) array_swap32(d, s, l)
# define cpu_to_be32_array(d, s, l) array_copy32(d, s, l)
# define be32_to_cpu_array(d, s, l) array_copy32(d, s, l)

# define cpu_to_le64_array(d, s, l) array_swap64(d, s, l)
# define le64_to_cpu_array(d, s, l) array_swap64(d, s, l)
# define cpu_to_be64_array(d, s, l) array_copy64(d, s, l)
# define be64_to_cpu_array(d, s, l) array_copy64(d, s, l)

# define ARCH_IS_BIG_ENDIAN

#else
# error "endian is neither big nor little endian"
#endif

#else
# error "__BYTE_ORDER__ is not defined"
#endif

#endif /* !BITFN_H */
