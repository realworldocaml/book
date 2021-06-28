Benchmark for Yojson
====================

These benchmarks require `Core_bench` which is not a dependency of Yojson,
because it is not part of the regular installation/testing flow. This is solely
meant for developers that are worried about performance changes in Yojson.

This is also why it is deemed appropriate to depend on a benchmarking library
with this many dependencies: we rather spend time writing benchmarks than to
build a benchmarking library if a perfectly good one already exists in OCaml.

Install
-------

`opam install core_benchmark`

Running the benchmark
---------------------

`make bench` in the top level directory.
