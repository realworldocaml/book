.PHONY: build bench clean

CFLAGS=-Wall -O2

CTYPES_INCLUDE=`ocamlfind query ctypes`/..

BENCH_MICRO_STUBS=bench_micro_stubs.h bench_micro_lib.c bench_micro_stubs.c
BENCH_MICRO_GENERATED=bench_micro_generated_stubs.c bench_micro_generated.ml

build: bench_micro

bench: build
	LD_LIBRARY_PATH=. ./bench_micro -ascii -q 10 +time +cycles \
		-clear-columns -no-compactions -v -ci-absolute -all-values \
		-overheads -save > summary.txt
	ocaml process_summary.ml summary.txt
	gnuplot bench_micro.gnuplot
	gnuplot bench_micro_interpreted.gnuplot

libbench_micro.so: $(BENCH_MICRO_STUBS)
	gcc -o libbench_micro.so -shared -fPIC $(CFLAGS) bench_micro_stubs.c

bench_micro_gen: $(BENCH_MICRO_STUBS) bench_micro_bindings.ml bench_micro_gen.ml
	ocamlfind opt -o bench_micro_gen \
		-linkpkg -package ctypes.foreign,ctypes.stubs \
		bench_micro_lib.c bench_micro_stubs.c \
		bench_micro_bindings.ml bench_micro_gen.ml

bench_micro_generated.ml bench_micro_generated_stubs.c: bench_micro_gen
	./bench_micro_gen

bench_micro: $(BENCH_MICRO_STUBS) $(BENCH_MICRO_GENERATED) libbench_micro.so bench_micro_bindings.ml bench_micro.ml
	ocamlfind opt -o bench_micro $(patsubst %,-ccopt %,$(CFLAGS)) \
		-cclib -lbench_micro -cclib -L. \
		-thread -linkpkg -I $(CTYPES_INCLUDE) \
		-package ctypes.foreign,ctypes.stubs,core,core_bench \
		bench_micro_lib.c bench_micro_stubs.c \
		bench_micro_generated_stubs.c \
		bench_micro_bindings.ml bench_micro_generated.ml bench_micro.ml

clean:
	rm -f bench_micro bench_micro_gen libbench_micro.so
	rm -f bench_micro_generated.ml bench_micro_generated_stubs.c
	rm -f bench_micro.eps bench_micro_interpreted.eps
	rm -f *.o *.cmo *.cmx *.cmi
