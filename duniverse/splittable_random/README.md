PRNG that can be split into independent streams

A splittable pseudo-random number generator (SPRNG) functions like a PRNG in that it can
be used as a stream of random values; it can also be \"split\" to produce a second,
independent stream of random values.

This library implements a splittable pseudo-random number generator that sacrifices
cryptographic-quality randomness in favor of performance.
