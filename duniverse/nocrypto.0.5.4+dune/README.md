# nocrypto - Simpler crypto

%%VERSION%%

nocrypto is a small cryptographic library that puts emphasis on the applicative
style and ease of use. It includes basic ciphers (AES, 3DES, RC4), hashes (MD5,
SHA1, SHA2 family), AEAD primitives (AES-GCM, AES-CCM), public-key primitives
(RSA, DSA, DH) and a strong RNG (Fortuna).

RSA timing attacks are countered by blinding. AES timing attacks are avoided by
delegating to AES-NI.

## Documentation

[Interface][nocrypto-mli] is documented. Also [online][doc].

[nocrypto-mli]: https://github.com/mirleft/ocaml-nocrypto/blob/master/src/nocrypto.mli
[doc]: http://mirleft.github.io/ocaml-nocrypto/doc

## Build

```bash
dune build
dune runtest
```

## FAQ

#### RNG seeding

If RNG fails with `Fatal error: exception Uncommon.Boot.Unseeded_generator`, you
need to [seed][doc-entropy] it.

Unix:
```OCaml
let () = Nocrypto_entropy_unix.initialize ()
```

Unix/Lwt:
```OCaml
let () = Nocrypto_entropy_lwt.initialize () |> ignore
```

[doc-entropy]: http://mirleft.github.io/ocaml-nocrypto/Nocrypto_entropy_unix.html

#### Illegal instructions

```
Program terminated with signal SIGILL, Illegal instruction.
#0  _mm_aeskeygenassist_si128 (__C=<optimized out>, __X=...)
```

`Nocrypto` has CPU acceleration support (`SSE2`+`AES-NI`), but no run-time
autodetection yet. You compiled the library with acceleration, but you are using
it on a machine that does not support it.

The environment variable `NOCRYPTO_ACCELERATE` can be used to override
detection:

- `NOCRYPTO_ACCELERATE=false dune build` force-disables non-portable code.
- `NOCRYPTO_ACCELERATE=true dune build` force-enables non-portable code.
- Otherwise, it matches the capabilities of the build machine.


[![Build Status](https://travis-ci.org/mirleft/ocaml-nocrypto.svg?branch=master)](https://travis-ci.org/mirleft/ocaml-nocrypto)
