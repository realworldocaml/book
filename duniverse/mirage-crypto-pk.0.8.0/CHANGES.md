## v0.8.0 (2020-06-18)

* New package mirage-crypto-rng-mirage which contains the entropy collection
  code for MirageOS (#69 requested by @samoht, implemented by @hannesm)
* Mirage_crypto_rng_lwt.initialize is not inside the Lwt monad anymore, and
  thus can be called by libraries at top level (#69, requested by @avsm @xguerin
  @talex5 in mirage/ocaml-conduit#318, implemented by @hannesm)
* Both Mirage_crypto_rng_lwt.initialize and Mirage_crypto_rng_unix.initialize
  don't do anything if called a second time (#69, implemented by @hannesm)
* Entropy source registration is now open and done via
  `Entropy.register_source : string -> source`, instead of a closed variant
  (#69, fixes #68, implemented by @hannesm)

## v0.7.0 (2020-05-18)

* CPU feature detection (AESNI, SSE3, PCLMULQ) at runtime instead of compile
  time (#53 @Julow, fixed MirageOS support #61, review by @hannesm)
  performance hit up to 5%
* Revise entropy collection (#64 @hannesm review by @dinosaure @cfcs)
  mirage-crypto-entropy has been folded into mirage-crypto-rng.{unix,lwt,mirage}
  - the RNG is no longer fork() safe, if you use fork in your code, be sure to
    reseed the RNG in the child process
  - on Unix and Lwt, the used RNG is Fortuna, seeded by getrandom(),
    rdrand/rdseed, and whirlwind
  - Mirage_crypto_rng_lwt does entropy collection for Lwt applications
  - entropy collection is now similar to FreeBSD:
    - rdrand/rdseed is executed in a separate task (by default every second)
    - on Unix, getrandom() is executed in another separate task (by default
      every 10 seconds)
    - on every enter of the Lwt event loop, some bits of rdtsc are collected
      (rdrand/rdseed is not on each even loop enter anymore)
  - Fortuna only uses entropy pools if the given period is exhausted (defaults
    to 1s), and the pool size exceeds 64 bytes
  - The unseeded generator exception prints instructions how to seed the RNG
* 32 bit support (for ghash), requested by @TImada in #60, #65 @hannesm
* use Eqaf_cstruct.find_uint8 instead of Cs.ct_find_uint8 (#52 @dinosaure)
* add (:standard) in C flags to allow cross-compilation #47 @samoht
* Mirage_crypto.Uncommon: remove several functions (Cs.create, Option),
  requires OCaml 4.08 (#49 #51 @hannesm)
* remove ocplib-endian dependency, use Bytes directly (since 4.07) #51 @hannesm
* bitfn.h cleanup (#56 #58 @hannesm)
* fix build if opam is not available (#66 @hannesm)
* update test.yml GitHub actions (#44 #57 @imbsky)
* Travis CI for arm64 (#55 @hannesm)

## v0.6.2 (2020-03-19)

* Do not validate hardcoded DH groups to speedup initializatio time
  (reported in #43 by @rbardou, fixed in #42 by @hannesm)

## v0.6.1 (2020-03-15)

* Add Windows 10+ support (#39 @avsm, review by @hannesm @dinosaure)
* Fix unix stubs to be allocating in case of exception (#39 @avsm
  review by @hannesm @dinosaure)
* Add GitHub Actions CI (#39 @avsm)
* Rebuild source if the `MIRAGE_CRYPTO_ACCELERATE` environment
  variable changes value (#40 @avsm)

## v0.6.0 (2020-03-11)

Initial release as mirage-crypto, based on nocrypto 0.5.4 (from January 2017),
written by David Kaloper, and mirage-entropy 0.5.1 (February 2020). The goal is
to maintain mirage-crypto collaboratively with required reviews for all pull
requests, and a CI setup for multiple platforms, thus issues are caught early.
The minimum targeted OCaml version is 4.07.0. Mirage-crypto and nocrypto can be
installed in the same opam switch, and also linked into the same binary.

The major difference is a split into four opam packages:
- mirage-crypto contains the hash algorithms and ciphers
- mirage-crypto-pk contains the public key cryptography
- mirage-crypto-rng contains the Random Number Generators
- mirage-crypto-entropy contains entropy seeding code used by MirageOS
  (previously mirage-entropy)

Only mirage-crypto-pk depends on zarith and gmp, the dependency cone of the
other packages is small.

Changes from nocrypto 0.5.4/mirage-entropy 0.5.1:
- Dune is used as build system (no longer pack()ed, no optional dependencies)
- Base64 has been removed, use [base64](https://github.com/mirage/ocaml-base64)
- Numeric has been removed, Mirage_crypto_pk.Z_extra provides to/of_cstruct_be
- RNG generators over numeric types (Rng.Int / Int32 / Int64) have been removed
  use [randomconv](https://github.com/hannesm/randomconv)
- For random generation of Z.t, previously Rng.Z, now
  Mirage_crypto_pk.Z_extra.gen and gen_r are provided

mirage-crypto
- Hash: Change of the hashing interface `Hash.S`:
  - `t` is now immutable;
  - `feed` and `get` are therefore referentially transparent; and
  - `digestv` was replaced by `digesti`.
  Old behavior of `feed` and `get` can be replicated by wrapping a `t` in a
  reference cell. `digestv` can be obtained by combining `digesti` with
  `List.iter`.
  - New functions: Hash.S.hmaci, Hash.digesti, Hash.maci
  - The type Hash.hash cannot be converted to a S-expression anymore
- Counter module got removed
- CTR mode: custom type `ctr` for the counter
- GCM is accelerated (with pclmulqdq and SSE3)
- CCM now computes correct tag with empty additional data
- CCM fails with Invalid_argument when short or long nonce is used (previously
  assert false)
- CCM decryption of empty ciphertext no longer fails
- GCM fails on empty IV

mirage-crypto-pk
- key data structures are private, smart constructors validate the key data
- DSA blinding now blinds the entire computation of s (previously r was blinded),
  same as what OpenSSL does
- RSA decrypt/sign has an optional argument (crt_hardening) to check whether
  the computed signature is valid (default true for PKCS1 signatures), avoiding
  Lenstra's CRT attack (mentioned again by Weimer)
- use Eqaf_cstruct.equal instead of home-grown ct_eq
- bindings with a bits argument are labeled to avoid confusion of bits and bytes
- Dh secret is now a pair of group and secret, Dh.shared no longer gets the
  group as separate argument
- the powm_sec function is used to improve timing side channels mitigation (see
  https://gmplib.org/~tege/modexp-silent.pdf for further detalls). RSA still
  uses (lacking alternatives) Z.erem and Z.mul/add/sub without timing
  mitigations, introduces 20-25% performance penalty (as shown below, and
  discussed in the pdf)

mirage-crypto-rng
- Default RNG is now Null (previously: Fortuna)
- The Mirage_crypto_rng_unix.initialize () sets the default RNG to direct
  syscalls to getrandom() (on Linux) or getentropy() (on BSD and macOS).
  Previously, /dev/urandom or /dev/random were used.to seed a Fortuna RNG.
  The Mirage_crypto_rng is now fork-safe on Unix systems: the child process no
  longer starts with the RNG seed of the parent.
- As noted above, the generators for numeric types have been removed.

mirage-crypto-entropy
- Control flow inverted, now Mirage_crypto_entropy sets the default RNG in
  Mirage_crypto_rng, and feeds this rng - previously, a RNG could register
  a callback to invoke whenever entropy was harvested.
- AMD CPUs with bad rdrand/rdseed, which always return -1, are detected and not
  used
- Initialization: in addition to whirlwind, rdrand/rdseed is used four times to
  seed the RNG (including check of the CR flag to ensure random was generated);
  if no hardware RNG is available, whirlwind is executed thrice.

These changes were prepared and reviewed by @pqwy, @hannesm, @cfcs, @dinosaure,
@emillon, @haesbaert, @mseri, @samoht, @reynir, @avsm, @TheLortex, @marmarek.

Performance of mirage-crypto 0.6.0 on an i7-5600U CPU @ 2.60GHz (OCaml 4.09.0):
* [aes-128-ecb]
       16:  130.635750 MB/s  (17131413 iters in 2.001 s)
       64:  446.478551 MB/s  (14619573 iters in 1.999 s)
      256:  1390.639142 MB/s  (11394212 iters in 2.000 s)
     1024:  2570.981481 MB/s  (5255924 iters in 1.996 s)
     8192:  3602.365523 MB/s  (923220 iters in 2.002 s)

* [aes-128-cbc-e]
       16:  118.576157 MB/s  (15562918 iters in 2.003 s)
       64:  284.131835 MB/s  (9337111 iters in 2.006 s)
      256:  443.035755 MB/s  (3634381 iters in 2.003 s)
     1024:  517.982442 MB/s  (1060993 iters in 2.000 s)
     8192:  549.691380 MB/s  (141017 iters in 2.004 s)

* [aes-128-cbc-d]
       16:  120.294692 MB/s  (15280286 iters in 1.938 s)
       64:  408.896399 MB/s  (13385151 iters in 1.998 s)
      256:  1239.911665 MB/s  (10218204 iters in 2.012 s)
     1024:  2307.252487 MB/s  (4750356 iters in 2.011 s)
     8192:  3200.883102 MB/s  (822730 iters in 2.008 s)

* [aes-128-ctr]
       16:  128.202344 MB/s  (16596766 iters in 1.975 s)
       64:  423.311050 MB/s  (13761489 iters in 1.984 s)
      256:  1263.233926 MB/s  (10270525 iters in 1.985 s)
     1024:  2168.164371 MB/s  (4474403 iters in 2.015 s)
     8192:  3029.360927 MB/s  (769213 iters in 1.984 s)

* [aes-128-gcm]
       16:  42.201314 MB/s  (5897756 iters in 2.132 s)
       64:  152.244886 MB/s  (5026236 iters in 2.015 s)
      256:  546.479756 MB/s  (4394580 iters in 1.963 s)
     1024:  1223.989968 MB/s  (2499487 iters in 1.994 s)
     8192:  1974.670878 MB/s  (507143 iters in 2.006 s)

* [aes-128-ghash]
       16:  48.954628 MB/s  (6311856 iters in 1.967 s)
       64:  176.553952 MB/s  (5735112 iters in 1.983 s)
      256:  700.256364 MB/s  (5667665 iters in 1.976 s)
     1024:  2095.813910 MB/s  (4238823 iters in 1.975 s)
     8192:  5038.045638 MB/s  (1284257 iters in 1.991 s)

* [aes-128-ccm]
       16:  10.188661 MB/s  (1335151 iters in 2.000 s)
       64:  33.832886 MB/s  (1107442 iters in 1.998 s)
      256:  80.837804 MB/s  (658093 iters in 1.988 s)
     1024:  125.678615 MB/s  (256478 iters in 1.993 s)
     8192:  150.055764 MB/s  (38013 iters in 1.979 s)

* [aes-192-ecb]
       16:  123.502998 MB/s  (16110634 iters in 1.990 s)
       64:  409.911649 MB/s  (13468258 iters in 2.005 s)
      256:  1270.949485 MB/s  (10447481 iters in 2.007 s)
     1024:  2301.964244 MB/s  (4655905 iters in 1.975 s)
     8192:  3106.818520 MB/s  (803606 iters in 2.021 s)

* [aes-256-ecb]
       16:  119.076345 MB/s  (15491470 iters in 1.985 s)
       64:  394.571062 MB/s  (12925749 iters in 1.999 s)
      256:  1202.173821 MB/s  (9784687 iters in 1.987 s)
     1024:  2091.674323 MB/s  (4242987 iters in 1.981 s)
     8192:  2775.548835 MB/s  (708481 iters in 1.994 s)

* [d3des-ecb]
       16:  18.146565 MB/s  (2349955 iters in 1.976 s)
       64:  20.393800 MB/s  (665318 iters in 1.991 s)
      256:  20.864270 MB/s  (172912 iters in 2.023 s)
     1024:  20.749041 MB/s  (42337 iters in 1.993 s)
     8192:  21.299626 MB/s  (5102 iters in 1.871 s)

* [fortuna]
       16:  45.461127 MB/s  (6009127 iters in 2.017 s)
       64:  163.608657 MB/s  (5478371 iters in 2.044 s)
      256:  576.021850 MB/s  (4815192 iters in 2.041 s)
     1024:  1394.787051 MB/s  (2838832 iters in 1.988 s)
     8192:  2375.298268 MB/s  (607658 iters in 1.999 s)

* [rng]
       16:  18.258923 MB/s  (2416789 iters in 2.020 s)
       64:  54.093151 MB/s  (1770883 iters in 1.998 s)
      256:  106.310471 MB/s  (870193 iters in 1.998 s)
     1024:  109.436412 MB/s  (225474 iters in 2.012 s)
     8192:  109.587676 MB/s  (28091 iters in 2.003 s)

* [md5]
       16:  60.932675 MB/s  (7919161 iters in 1.983 s)
       64:  166.939662 MB/s  (5392579 iters in 1.972 s)
      256:  315.792950 MB/s  (2725727 iters in 2.107 s)
     1024:  432.116247 MB/s  (888683 iters in 2.008 s)
     8192:  457.205673 MB/s  (123686 iters in 2.113 s)

* [sha1]
       16:  51.685511 MB/s  (7112274 iters in 2.100 s)
       64:  142.165035 MB/s  (4545000 iters in 1.951 s)
      256:  290.103032 MB/s  (2374321 iters in 1.998 s)
     1024:  392.263515 MB/s  (798932 iters in 1.989 s)
     8192:  441.651051 MB/s  (111759 iters in 1.977 s)

* [sha256]
       16:  32.740541 MB/s  (4248963 iters in 1.980 s)
       64:  82.039587 MB/s  (2667306 iters in 1.984 s)
      256:  148.107628 MB/s  (1206556 iters in 1.989 s)
     1024:  187.633143 MB/s  (378650 iters in 1.971 s)
     8192:  203.079830 MB/s  (51460 iters in 1.980 s)

* [sha512]
       16:  27.174808 MB/s  (3539910 iters in 1.988 s)
       64:  107.114479 MB/s  (3511519 iters in 2.001 s)
      256:  181.582232 MB/s  (1481042 iters in 1.991 s)
     1024:  262.146451 MB/s  (538311 iters in 2.005 s)
     8192:  301.017129 MB/s  (76650 iters in 1.989 s)

* [rsa-generate]
    1024:  112.465 ops per second (1269 iters in 11.283)
    2048:  11.407 ops per second (125 iters in 10.958)
    4096:  1.342 ops per second (9 iters in 6.709)

* [rsa-encrypt]
    1024:  85190.882 ops per second (1086956 iters in 12.759)
    2048:  28593.006 ops per second (349650 iters in 12.229)
    4096:  8813.099 ops per second (102040 iters in 11.578)

* [rsa-decrypt]
    1024:  4032.054 ops per second (29708 iters in 7.368)
    2048:  731.030 ops per second (7274 iters in 9.950)
    4096:  116.340 ops per second (1145 iters in 9.842)

* [rsa-pkcs1-encrypt]
    1024:  77539.233 ops per second (735294 iters in 9.483)
    2048:  24854.606 ops per second (268817 iters in 10.816)
    4096:  7693.667 ops per second (85324 iters in 11.090)

* [rsa-pkcs1-decrypt]
    1024:  3988.810 ops per second (38699 iters in 9.702)
    2048:  744.531 ops per second (7314 iters in 9.824)
    4096:  115.388 ops per second (1141 iters in 9.888)

* [rsa-pkcs1-sign]
    1024:  3417.872 ops per second (34530 iters in 10.103)
    2048:  657.928 ops per second (6568 iters in 9.983)
    4096:  107.867 ops per second (1071 iters in 9.929)

* [rsa-pkcs1-verify]
    1024:  69062.244 ops per second (694444 iters in 10.055)
    2048:  23739.709 ops per second (236966 iters in 9.982)
    4096:  7543.535 ops per second (76687 iters in 10.166)

* [rsa-pss-sign]
    1024:  3866.357 ops per second (41528 iters in 10.741)
    2048:  735.117 ops per second (7337 iters in 9.981)
    4096:  112.564 ops per second (1144 iters in 10.163)

* [rsa-pss-verify]
    1024:  58865.502 ops per second (602409 iters in 10.234)
    2048:  21324.568 ops per second (222222 iters in 10.421)
    4096:  7032.052 ops per second (71225 iters in 10.129)

* [dsa-generate]
    1024:  34.113 ops per second (462 iters in 13.543)
    2048:  2.412 ops per second (26 iters in 10.779)
    3072:  0.395 ops per second (6 iters in 15.194)

* [dsa-sign]
    1024:  6868.305 ops per second (68870 iters in 10.027)
    2048:  1771.636 ops per second (17959 iters in 10.137)
    3072:  807.851 ops per second (8884 iters in 10.997)

* [dsa-verify]
    1024:  5927.364 ops per second (60901 iters in 10.275)
    2048:  1043.694 ops per second (10307 iters in 9.875)
    3072:  505.150 ops per second (5163 iters in 10.221)

* [dh-secret]
    oakley5 (1536):  2965.814 ops per second (27173 iters in 9.162)
    oakley14 (2048):  1829.276 ops per second (19372 iters in 10.590)
    ffdhe2048:  1863.896 ops per second (19215 iters in 10.309)
    ffdhe3072:  705.235 ops per second (7278 iters in 10.320)
    ffdhe4096:  340.646 ops per second (3353 iters in 9.843)
    ffdhe6144:  154.170 ops per second (1508 iters in 9.781)

* [dh-share]
    oakley5 (1536):  3168.025 ops per second (32658 iters in 10.309)
    oakley14 (2048):  1840.663 ops per second (19054 iters in 10.352)
    ffdhe2048:  1816.268 ops per second (19157 iters in 10.547)
    ffdhe3072:  696.719 ops per second (7166 iters in 10.285)
    ffdhe4096:  337.490 ops per second (3399 iters in 10.071)
    ffdhe6144:  152.990 ops per second (1532 iters in 10.014)

compared to nocrypto master (ed7bb8d911dc340e36d85d335d9edb8339f0932d):
* [aes-128-ecb]
       16:  135.442339 MB/s  (17569952 iters in 1.979 s)
       64:  460.922168 MB/s  (15006148 iters in 1.987 s)
      256:  1428.596998 MB/s  (11586270 iters in 1.980 s)
     1024:  2603.066949 MB/s  (5351070 iters in 2.007 s)
     8192:  3562.221449 MB/s  (894266 iters in 1.961 s)

* [aes-128-cbc-e]
       16:  112.052192 MB/s  (15563390 iters in 2.119 s)
       64:  283.095265 MB/s  (8731413 iters in 1.882 s)
      256:  441.262003 MB/s  (3650061 iters in 2.019 s)
     1024:  506.978895 MB/s  (1024850 iters in 1.974 s)
     8192:  521.717454 MB/s  (139145 iters in 2.084 s)

* [aes-128-cbc-d]
       16:  117.319846 MB/s  (15508408 iters in 2.017 s)
       64:  402.086303 MB/s  (13138679 iters in 1.994 s)
      256:  1247.900251 MB/s  (10363209 iters in 2.027 s)
     1024:  2303.701767 MB/s  (4783087 iters in 2.028 s)
     8192:  3177.857027 MB/s  (807868 iters in 1.986 s)

* [aes-128-ctr]
       16:  124.833523 MB/s  (16276319 iters in 1.990 s)
       64:  420.635080 MB/s  (13606571 iters in 1.974 s)
      256:  1251.742047 MB/s  (10235121 iters in 1.996 s)
     1024:  2145.657443 MB/s  (4369951 iters in 1.989 s)
     8192:  2970.097091 MB/s  (757489 iters in 1.992 s)

* [aes-128-gcm]
       16:  44.527800 MB/s  (5854064 iters in 2.006 s)
       64:  152.068032 MB/s  (4992261 iters in 2.004 s)
      256:  542.805976 MB/s  (4414864 iters in 1.986 s)
     1024:  1193.384389 MB/s  (2460672 iters in 2.014 s)
     8192:  1935.605143 MB/s  (487413 iters in 1.967 s)

* [aes-128-ghash]
       16:  47.523782 MB/s  (6246603 iters in 2.006 s)
       64:  172.924057 MB/s  (5636899 iters in 1.990 s)
      256:  689.413807 MB/s  (5634564 iters in 1.995 s)
     1024:  2058.438893 MB/s  (4202766 iters in 1.994 s)
     8192:  4920.604006 MB/s  (1254857 iters in 1.992 s)

* [aes-128-ccm]
       16:  9.845429 MB/s  (1274401 iters in 1.975 s)
       64:  32.275969 MB/s  (1043211 iters in 1.973 s)
      256:  78.221849 MB/s  (600615 iters in 1.875 s)
     1024:  120.482150 MB/s  (245463 iters in 1.990 s)
     8192:  143.803733 MB/s  (36746 iters in 1.996 s)

* [aes-192-ecb]
       16:  117.584221 MB/s  (15668971 iters in 2.033 s)
       64:  400.021369 MB/s  (13021257 iters in 1.987 s)
      256:  1255.330965 MB/s  (10273511 iters in 1.998 s)
     1024:  2229.593050 MB/s  (4530976 iters in 1.985 s)
     8192:  3046.782735 MB/s  (773377 iters in 1.983 s)

* [aes-256-ecb]
       16:  114.745077 MB/s  (14905839 iters in 1.982 s)
       64:  384.193368 MB/s  (12486413 iters in 1.984 s)
      256:  1164.849725 MB/s  (9530435 iters in 1.997 s)
     1024:  2028.763403 MB/s  (4114904 iters in 1.981 s)
     8192:  2694.585208 MB/s  (683879 iters in 1.983 s)

* [d3des-ecb]
       16:  17.672106 MB/s  (2308796 iters in 1.994 s)
       64:  19.903860 MB/s  (648180 iters in 1.988 s)
      256:  20.828452 MB/s  (169190 iters in 1.983 s)
     1024:  21.026390 MB/s  (42581 iters in 1.978 s)
     8192:  21.103543 MB/s  (5365 iters in 1.986 s)

* [fortuna]
       16:  45.528384 MB/s  (5972145 iters in 2.002 s)
       64:  164.170314 MB/s  (5392710 iters in 2.005 s)
      256:  583.800203 MB/s  (4766501 iters in 1.993 s)
     1024:  1384.452890 MB/s  (2828454 iters in 1.995 s)
     8192:  2300.342453 MB/s  (586851 iters in 1.993 s)

* [md5]
       16:  60.352932 MB/s  (7833123 iters in 1.980 s)
       64:  163.484629 MB/s  (4985343 iters in 1.861 s)
      256:  326.053714 MB/s  (2676931 iters in 2.004 s)
     1024:  430.897892 MB/s  (899381 iters in 2.038 s)
     8192:  481.258285 MB/s  (110790 iters in 1.799 s)

* [sha1]
       16:  51.720201 MB/s  (7014270 iters in 2.069 s)
       64:  137.512543 MB/s  (4583413 iters in 2.034 s)
      256:  283.507242 MB/s  (2120380 iters in 1.826 s)
     1024:  381.400222 MB/s  (773643 iters in 1.981 s)
     8192:  434.856626 MB/s  (110993 iters in 1.994 s)

* [sha256]
       16:  32.117877 MB/s  (4162582 iters in 1.978 s)
       64:  78.860229 MB/s  (2562436 iters in 1.983 s)
      256:  143.113584 MB/s  (1167932 iters in 1.992 s)
     1024:  181.775160 MB/s  (370913 iters in 1.993 s)
     8192:  196.245056 MB/s  (49758 iters in 1.981 s)

* [sha512]
       16:  27.343156 MB/s  (3569255 iters in 1.992 s)
       64:  108.955616 MB/s  (3534730 iters in 1.980 s)
      256:  181.403811 MB/s  (1471724 iters in 1.981 s)
     1024:  261.357534 MB/s  (527511 iters in 1.971 s)
     8192:  298.635244 MB/s  (75985 iters in 1.988 s)

* [rsa-generate]
    1024:  137.412 ops per second (1358 iters in 9.883)
    2048:  13.585 ops per second (144 iters in 10.600)
    4096:  1.386 ops per second (9 iters in 6.494)

* [rsa-encrypt]
    1024:  85072.401 ops per second (833333 iters in 9.796)
    2048:  28931.132 ops per second (284090 iters in 9.820)
    4096:  8969.821 ops per second (88028 iters in 9.814)

* [rsa-decrypt]
    1024:  5318.640 ops per second (48875 iters in 9.189)
    2048:  957.132 ops per second (9731 iters in 10.167)
    4096:  139.017 ops per second (1396 iters in 10.042)

* [rsa-pkcs1-encrypt]
    1024:  70783.935 ops per second (537634 iters in 7.595)
    2048:  24159.049 ops per second (241545 iters in 9.998)
    4096:  7667.324 ops per second (73964 iters in 9.647)

* [rsa-pkcs1-decrypt]
    1024:  5227.203 ops per second (48828 iters in 9.341)
    2048:  940.782 ops per second (9591 iters in 10.195)
    4096:  138.289 ops per second (1385 iters in 10.015)

* [rsa-pkcs1-sign]
    1024:  5089.071 ops per second (41806 iters in 8.215)
    2048:  938.950 ops per second (9480 iters in 10.096)
    4096:  138.016 ops per second (1378 iters in 9.984)

* [rsa-pkcs1-verify]
    1024:  64928.671 ops per second (537634 iters in 8.280)
    2048:  23218.845 ops per second (225225 iters in 9.700)
    4096:  7566.439 ops per second (73313 iters in 9.689)

* [rsa-pss-sign]
    1024:  5016.718 ops per second (49701 iters in 9.907)
    2048:  926.741 ops per second (9439 iters in 10.185)
    4096:  136.907 ops per second (1356 iters in 9.905)

* [rsa-pss-verify]
    1024:  55386.995 ops per second (515463 iters in 9.307)
    2048:  20812.506 ops per second (203252 iters in 9.766)
    4096:  6976.815 ops per second (68965 iters in 9.885)

* [dsa-generate]
    1024:  30.547 ops per second (518 iters in 16.957)
    2048:  3.073 ops per second (26 iters in 8.460)
    3072:  0.963 ops per second (7 iters in 7.273)

* [dsa-sign]
    1024:  5169.869 ops per second (50761 iters in 9.819)
    2048:  1037.131 ops per second (10778 iters in 10.392)
    3072:  498.180 ops per second (5203 iters in 10.444)

* [dsa-verify]
    1024:  5811.022 ops per second (56561 iters in 9.733)
    2048:  1047.119 ops per second (10519 iters in 10.046)
    3072:  502.466 ops per second (5080 iters in 10.110)

* [dh-secret]
    oakley5 (1536):  4032.563 ops per second (40160 iters in 9.959)
    oakley14 (2048):  2370.121 ops per second (23397 iters in 9.872)
    ffdhe2048:  2371.094 ops per second (23474 iters in 9.900)
    ffdhe3072:  959.168 ops per second (9328 iters in 9.725)
    ffdhe4096:  477.707 ops per second (4723 iters in 9.887)
    ffdhe6144:  232.990 ops per second (2318 iters in 9.949)

* [dh-share]
    oakley5 (1536):  4109.406 ops per second (42194 iters in 10.268)
    oakley14 (2048):  2361.098 ops per second (23180 iters in 9.817)
    ffdhe2048:  2362.707 ops per second (23463 iters in 9.931)
    ffdhe3072:  954.438 ops per second (9708 iters in 10.171)
    ffdhe4096:  462.487 ops per second (4770 iters in 10.314)
    ffdhe6144:  216.363 ops per second (2320 iters in 10.723)

compared to nocrypto 0.5.4:
* [aes-128-ecb]
       16:  129.593907 MB/s  (17113588 iters in 2.015 s)
       64:  445.385674 MB/s  (14718078 iters in 2.017 s)
      256:  1401.391157 MB/s  (11549531 iters in 2.012 s)
     1024:  2595.914412 MB/s  (5344235 iters in 2.010 s)
     8192:  3501.195690 MB/s  (901692 iters in 2.012 s)

* [aes-128-cbc-e]
       16:  108.325512 MB/s  (14606739 iters in 2.058 s)
       64:  253.049301 MB/s  (8601262 iters in 2.075 s)
      256:  401.720298 MB/s  (3339667 iters in 2.030 s)
     1024:  489.085790 MB/s  (960494 iters in 1.918 s)
     8192:  517.197627 MB/s  (133293 iters in 2.013 s)

* [aes-128-cbc-d]
       16:  107.210047 MB/s  (14669564 iters in 2.088 s)
       64:  381.448329 MB/s  (12527866 iters in 2.005 s)
      256:  1240.270260 MB/s  (9848820 iters in 1.939 s)
     1024:  2312.995617 MB/s  (4691696 iters in 1.981 s)
     8192:  3133.530316 MB/s  (810500 iters in 2.021 s)

* [aes-128-ctr]
       16:  122.343292 MB/s  (15659757 iters in 1.953 s)
       64:  403.448633 MB/s  (13233815 iters in 2.002 s)
      256:  1248.728485 MB/s  (10233026 iters in 2.001 s)
     1024:  2114.395379 MB/s  (4328489 iters in 1.999 s)
     8192:  2877.065507 MB/s  (738833 iters in 2.006 s)

* [aes-128-gcm]
       16:  8.210968 MB/s  (1082052 iters in 2.011 s)
       64:  19.575635 MB/s  (639463 iters in 1.994 s)
      256:  29.907070 MB/s  (245891 iters in 2.007 s)
     1024:  29.262152 MB/s  (57980 iters in 1.935 s)
     8192:  29.581451 MB/s  (7531 iters in 1.989 s)

* [aes-128-ccm]
       16:  9.944615 MB/s  (1292677 iters in 1.983 s)
       64:  32.926246 MB/s  (1063853 iters in 1.972 s)
      256:  78.113169 MB/s  (637793 iters in 1.993 s)
     1024:  122.785527 MB/s  (248373 iters in 1.975 s)
     8192:  145.229084 MB/s  (34938 iters in 1.879 s)

* [aes-192-ecb]
       16:  119.679360 MB/s  (16015977 iters in 2.042 s)
       64:  411.271644 MB/s  (13428694 iters in 1.993 s)
      256:  1289.074612 MB/s  (10622087 iters in 2.012 s)
     1024:  2284.472462 MB/s  (4634843 iters in 1.981 s)
     8192:  3069.150001 MB/s  (783834 iters in 1.995 s)

* [aes-256-ecb]
       16:  118.242682 MB/s  (15540220 iters in 2.005 s)
       64:  394.463508 MB/s  (12884896 iters in 1.994 s)
      256:  1219.668891 MB/s  (9902558 iters in 1.982 s)
     1024:  2088.975918 MB/s  (4237494 iters in 1.981 s)
     8192:  2701.278566 MB/s  (684954 iters in 1.981 s)

* [d3des-ecb]
       16:  17.784525 MB/s  (2330798 iters in 2.000 s)
       64:  20.185889 MB/s  (658308 iters in 1.990 s)
      256:  21.036904 MB/s  (171344 iters in 1.989 s)
     1024:  21.195014 MB/s  (42993 iters in 1.981 s)
     8192:  21.450254 MB/s  (5475 iters in 1.994 s)

* [fortuna]
       16:  44.926539 MB/s  (5858282 iters in 1.990 s)
       64:  147.362742 MB/s  (5285998 iters in 2.189 s)
      256:  556.881662 MB/s  (4692885 iters in 2.057 s)
     1024:  1322.280478 MB/s  (2796713 iters in 2.065 s)
     8192:  2193.681448 MB/s  (464696 iters in 1.655 s)

* [md5]
       16:  47.291886 MB/s  (5869457 iters in 1.894 s)
       64:  140.406459 MB/s  (4424445 iters in 1.923 s)
      256:  298.883345 MB/s  (2448636 iters in 2.000 s)
     1024:  425.476508 MB/s  (864106 iters in 1.983 s)
     8192:  483.577246 MB/s  (123914 iters in 2.002 s)

* [sha1]
       16:  44.741101 MB/s  (5791729 iters in 1.975 s)
       64:  124.553309 MB/s  (4048214 iters in 1.984 s)
      256:  262.417489 MB/s  (2131187 iters in 1.983 s)
     1024:  373.496782 MB/s  (761690 iters in 1.992 s)
     8192:  438.060065 MB/s  (112024 iters in 1.998 s)

* [sha256]
       16:  28.598662 MB/s  (3705886 iters in 1.977 s)
       64:  73.185820 MB/s  (2364851 iters in 1.972 s)
      256:  139.469681 MB/s  (1136138 iters in 1.989 s)
     1024:  180.737238 MB/s  (365880 iters in 1.977 s)
     8192:  197.959258 MB/s  (50160 iters in 1.980 s)

* [sha512]
       16:  24.386785 MB/s  (3171793 iters in 1.985 s)
       64:  97.828998 MB/s  (3192511 iters in 1.992 s)
      256:  172.207635 MB/s  (1401788 iters in 1.987 s)
     1024:  258.055383 MB/s  (523524 iters in 1.981 s)
     8192:  302.046584 MB/s  (76448 iters in 1.977 s)



## mirage-entropy v0.5.1 (2020-02-24)

* use dune for building this package (#47 @hannesm)

## mirage-entropy v0.5.0 (2019-11-01)

* Use mirage-runtime provided hooks (since 3.7.0), see mirage/mirage#1010 (#45 @samoht)
* Drop mirage-os-shim dependency (#45 @samoht)
* Raise minimum OCaml version to 4.06.0 (#46 @hannesm)

## mirage-entropy 0.4.1 (2018-08-15)

* Aarch64 support (#39 by @reynir)

## nocrypto v0.5.4 2017-01-31:
* Relicense from BSD2 to ISC.
* Support MirageOS 3.0.
* Replace OASIS with topkg.
* Stricter base64 decoding.

## mirage-entropy 0.4.0 (2017-01-20)

* Compatibility with MirageOS 3 module types.
* Add a benchmark.
* Obsolete `mirage-entropy-<BACKEND>`; the repository now contains only `mirage-entropy`.
* Support Unix, Xen, and Solo5 backends.
* Prune `oasis`.
* Move `noalloc` to 4.03-style annotations

## nocrypto v0.5.3 2016-03-21:
* Move from Camlp4 to PPX.
* Tweaked the supporting Cstruct module's API.
* Dh.shared returns option instead of throwing if the public message is degenerate.
* Base64.decode returns option instead of throwing

## nocrypto 0.5.2 2015-12-03:
* Avoid including intrinsics-related headers if SSE/AES-NI is disabled.
* Replace opam variable `nocrypto-inhibit-modernity` with `$NOCRYPTO_NO_ACCEL`.
* Remove cstruct equality.

## nocrypto 0.5.1 2015-07-07:
* Disable AES-NI if not supported in the `./configure` step.
* Support the global opam variable `nocrypto-inhibit-modernity`.

## nocrypto 0.5.0 2015-07-02:
* Support for AES-NI and SSE2.
* Support RSA-OAEP and RSA-PSS.
* Drop `ctypes` for internal C calls.
* Generate smaller secret exponents for DH, making operations on large groups much faster.
* Support dynamic switching of RNG algorithms and decouple `Rng` from `Fortuna`.
* Module for injectring entropy into RNG on pure Unix (optional).
* `Nocrypto_entropy_lwt.initialize` no longer needs to be synchronized on.
* Renamed module signatures and modules containing only signatures from `T` to `S`.
* Changes to `CTR`, `CBC`, `Rsa` and `Dh` APIs.

## mirage-entropy 0.3.0 (2015-05-02)

* Remove `mirage-entropy-unix` from the repository; it now only contains `mirage-entropy-xen`.
* Add internal entropy harvesting via timing and CPU RNG if available.
* Temporarily disable `xentropyd`.
* The API is no longer `V1.ENTROPY` compatible.

## nocrypto 0.4.0 2015-05-02:
* Module for injecting entropy into RNG on Unix/Lwt (optional).
* Module for injecting entropy into RNG on Mirage/Xen (optional; depends on mirage-entropy-xen).
* API changes in `Rng`.
* Do not 0-pad DH public and shared representations.
* More named DH groups.

## mirage-entropy 0.2.0 (09-Mar-2015)

* Do not wrap `Entropy_unix` in a functor as it is meant to be used directly.
* Xen: read entropy from a Xen PV device.  This is implemented by the `xentropyd` daemon.

## nocrypto 0.3.1 2015-02-01:
* Support for Mirage/Xen (contributed by Thomas Leonard <talex5@gmail.com>).

## nocrypto 0.3.0 2014-12-21:
* Removed ad-hoc key marshalling functions as key material typically comes non-trivially encoded anyways.
* Changed how module interfaces for the packed module are handled: `module type of` constructs are gone.
* More consistent errors in `Rsa`.
* Small API breakage here and there.

## nocrypto 0.2.2 2014-11-04:
* Replaced hashing sources with the ones from hs-cryptohash
  (https://github.com/vincenthz/hs-cryptohash) by Vincent Hanquez.
* Renamed various symbols likely to conflict with other crypto libraries.

## nocrypto 0.2.0 2014-10-30:
* DSA (initial version contributed by Hannes Mehnert <hannes@mehnert.org>).
* CCM mode for AES (contributed by Hannes Mehnert <hannes@mehnert.org>).
* Switched from hand written stubs to ctypes for intefacing with the C code.
* Packed the module to avoid clobbering global namespace; some modules renamed.
* Various bugfixes and improvements.

## nocrypto 0.1.0 2014-07-08:
* Initial (beta) release.

## mirage-entropy 0.1.6 (06-July-2014)

* Rework the module to be event-driven, more in line with entropy gathering.

## mirage-entropy 0.1.5 (06-July-2014)

* Guarantee that all of the required entropy is read on Unix.
* Add a `Entropy_xen_weak` that uses the builtin `Random.self_init` as a measure of last-resort.

## mirage-entropy 0.1.4 (04-July-2014)

* provide Mirage 1.2.0 interfaces (`V1_LWT.ENTROPY`).
* name modules `Entropy_xen` and `Entropy_unix` to not clash.

## mirage-entropy 0.1.3 (03-July-2014)

* Unbreak build: ocamlfind wasn't able to locate the package previously.

## mirage-entropy 0.1.2 (03-July-2014)

* Use `/dev/urandom` instead of `/dev/random` (for non-blocking behaviour).

## mirage-entropy 0.1.1 (03-July-2014)

* Use Makefile instead of oasis as build system.

## mirage-entropy 0.1.0 (03-July-2014)

* Initial release: on Unix, use `/dev/random`; on XEN, error out.
