# This file is part of mirage-crypto-ec, and used to generate C files
# As a prerequisite, fiat-crypto (https://github.com/mit-plv/fiat-crypto)
# needs to be cloned and "make standalone-ocaml" invoked
# The lowest bound of fiat-crypto is git commit
# dabaf4b3132e8bb4a3f5fcd8366eec6ac9bb4232 (July 16th 2021)
# Generated on FreeBSD 12.2p2 with coq 8.13.1 (OCaml 4.12.0)

WBW_MONT ?= ../../../fiat-crypto/src/ExtractionOCaml/word_by_word_montgomery --static --use-value-barrier --inline-internal
UNSAT_SOLINAS ?= ../../../fiat-crypto/src/ExtractionOCaml/unsaturated_solinas --static --use-value-barrier --inline-internal
N_FUNCS=mul add opp from_montgomery to_montgomery one msat divstep_precomp divstep to_bytes from_bytes selectznz

# The NIST curve P-224 (AKA SECP224R1)
P224="2^224 - 2^96 + 1"

.PHONY: p224_64.h
p224_64.h:
	$(WBW_MONT) p224 64 $(P224) > $@

.PHONY: p224_32.h
p224_32.h:
	$(WBW_MONT) p224 32 $(P224) > $@

# The group order N of P-224
P224N="0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D"

.PHONY: np224_64.h
np224_64.h:
	$(WBW_MONT) np224 64 $(P224N) $(N_FUNCS) > $@

.PHONY: np224_32.h
np224_32.h:
	$(WBW_MONT) np224 32 $(P224N) $(N_FUNCS) > $@

.PHONY: p224
p224: p224_64.h p224_32.h np224_64.h np224_32.h


# The NIST curve P-256 (AKA SECP256R1)
P256="2^256 - 2^224 + 2^192 + 2^96 - 1"

.PHONY: p256_64.h
p256_64.h:
	$(WBW_MONT) p256 64 $(P256) > $@

.PHONY: p256_32.h
p256_32.h:
	$(WBW_MONT) p256 32 $(P256) > $@

# The group order N of P-256
P256N="0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"

.PHONY: np256_64.h
np256_64.h:
	$(WBW_MONT) np256 64 $(P256N) $(N_FUNCS) > $@

.PHONY: np256_32.h
np256_32.h:
	$(WBW_MONT) np256 32 $(P256N) $(N_FUNCS) > $@

.PHONY: p256
p256: p256_64.h p256_32.h np256_64.h np256_32.h

# The NIST curve P-384 (AKA SECP384R1)
P384="2^384 - 2^128 - 2^96 + 2^32 - 1"

.PHONY: p384_64.h
p384_64.h:
	$(WBW_MONT) p384 64 $(P384) > $@

.PHONY: p384_32.h
p384_32.h:
	$(WBW_MONT) p384 32 $(P384) > $@

# The group order N of P-384
P384N="0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973"

.PHONY: np384_64.h
np384_64.h:
	$(WBW_MONT) np384 64 $(P384N) $(N_FUNCS) > $@

.PHONY: np384_32.h
np384_32.h:
	$(WBW_MONT) np384 32 $(P384N) $(N_FUNCS) > $@

.PHONY: p384
p384: p384_64.h p384_32.h np384_64.h np384_32.h

# The NIST curve P-521 (AKA SECP521R1)
P521="2^521 - 1"

.PHONY: p521_64.h
p521_64.h:
	$(WBW_MONT) p521 64 $(P521) > $@

.PHONY: p521_32.h
p521_32.h:
	$(WBW_MONT) p521 32 $(P521) > $@

# The group order N of P-521
P521N="0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409"

.PHONY: np521_64.h
np521_64.h:
	$(WBW_MONT) np521 64 $(P521N) $(N_FUNCS) > $@

.PHONY: np521_32.h
np521_32.h:
	$(WBW_MONT) np521 32 $(P521N) $(N_FUNCS) > $@

.PHONY: p521
p521: p521_64.h p521_32.h np521_64.h np521_32.h

# 25519
25519="2^255 - 19"
25519_FUNS=carry_mul carry_square carry add sub opp selectznz to_bytes from_bytes carry_scmul121666

.PHONY: curve25519_64.h
curve25519_64.h:
	$(UNSAT_SOLINAS) 25519 64 '(auto)' $(25519) $(25519_FUNS) > $@

.PHONY: curve25519_32.h
curve25519_32.h:
	$(UNSAT_SOLINAS) 25519 32 '(auto)' $(25519) $(25519_FUNS) > $@

.PHONY: curve25519
curve25519: curve25519_64.h curve25519_32.h


.PHONY: clean
clean:
	$(RM) p224_32.h p224_64.h np224_32.h np224_64.h
	$(RM) p256_32.h p256_64.h np256_32.h np256_64.h
	$(RM) p384_32.h p384_64.h np384_32.h np384_64.h
	$(RM) p521_32.h p521_64.h np521_32.h np521_64.h
	$(RM) curve25519_32.h curve25519_64.h

.PHONY: all
all: p224 p256 p384 p521 curve25519
