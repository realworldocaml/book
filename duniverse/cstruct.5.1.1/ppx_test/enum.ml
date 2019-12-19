(*
 * Copyright (c) 2016 Hannes Mehnert <hannes@mehnert.org>
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
 *)

[%%cenum
type foo64 =
  | ONE64
  | TWO64
  | THREE64
  [@@uint64_t] [@@sexp]
]

[%%cenum
type bar64 =
  | ONE64
  | TWO64 [@id 0xfffffffffffffffeL]
  | THREE64
  [@@uint64_t] [@@sexp]
]

[%%cenum
type foo32 =
  | ONE32
  | TWO32 [@id 0xfffffffel]
  | THREE32
  [@@uint32_t]
]

[%%cenum
type bar16 =
  | ONE [@id 1]
  | TWO
  | FOUR [@id 4]
  | FIVE
  [@@uint16_t]
]

[%%cenum
type foo16 =
  | ONE16
  | TWO16
  | THREE16
  [@@uint16_t]
]

[%%cenum
type foo8 =
  | ONE8
  | TWO8
  | THREE8
  [@@uint8_t]
]

[%%cenum
type reversed =
  | ONE_R [@id 2]
  | TWO_R [@id 1]
  [@@uint8_t]
]

let tests () =
  ignore(int_to_foo64 2L);
  ignore(int_to_foo32 1l);
  ignore(int_to_foo16 1);
  ignore(int_to_foo8 1);
  ignore(foo64_to_int ONE64);
  ignore(foo32_to_int ONE32);
  ignore(foo16_to_int ONE16);
  ignore(foo8_to_int ONE8);
  assert(bar16_to_int FOUR = 4);
  assert(bar16_to_int FIVE = 5);
  assert(foo32_to_int TWO32   = 0xfffffffel);
  assert(foo32_to_int THREE32 = 0xffffffffl);
  assert(int_to_foo32 0xfffffffel = Some (TWO32));
  assert(int_to_foo32 0xffffffffl = Some (THREE32));
  assert(string_to_foo16 "ONE16" = Some ONE16);
  assert(foo8_to_string ONE8 = "ONE8");
  assert(compare_foo8 ONE8 TWO8 = -1);
  assert(compare_foo8 TWO8 ONE8 = 1);
  assert(compare_foo8 TWO8 TWO8 = 0);
  assert(compare_reversed ONE_R TWO_R = 1)

let () = tests ()
