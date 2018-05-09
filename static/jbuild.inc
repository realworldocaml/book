(jbuild_version 1)

(alias (
  (name site)
  (deps (
    css/app.css
    css/prism.css
    images/anil.jpg
    images/bg.jpg
    images/book-cover.jpg
    images/cover.png
    images/jason.jpg
    images/plain-bg.jpg
    images/yaron.jpg
    images/front-end/pipeline.png
    images/gc/minor_heap.png
    images/imperative-programming/edit_distance.png
    images/imperative-programming/edit_distance2.png
    images/lists-and-patterns/lists_layout.png
    images/memory-repr/block.png
    images/memory-repr/float_array_layout.png
    images/memory-repr/string_block.png
    images/memory-repr/string_size_calc.png
    images/memory-repr/tuple_layout.png
    js/_functions.js
    js/app.js
    js/discourse.js
    js/jquery.min.js
    js/modernizr.js
    js/prism.js
    js/min/app-min.js
    js/min/modernizr-min.js
    theme/pdf/pdf.css))))

(alias ((name site) (deps (classes.html))))

(rule (
  (targets (classes.html))
  (deps (
    ../book/classes.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/classes-async/multiple_inheritance.ml
    ../examples/code/classes-async/multiple_inheritance_wrong.ml
    ../examples/code/classes-async/shapes/build_shapes.sh
    ../examples/code/classes-async/shapes/jbuild
    ../examples/code/classes-async/shapes/shapes.ml
    ../examples/code/classes-async/verbose_shapes.ml
    ../examples/code/classes/Iterator.java
    ../examples/code/classes/binary.mlt
    ../examples/code/classes/binary_larger.ml
    ../examples/code/classes/binary_module.ml
    ../examples/code/classes/citerator.cpp
    ../examples/code/classes/class_types_stack.ml
    ../examples/code/classes/doc.ml
    ../examples/code/classes/initializer.mlt
    ../examples/code/classes/istack.mlt
    ../examples/code/classes/iter.mlt
    ../examples/code/classes/stack.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (command-line-parsing.html))))

(rule (
  (targets (command-line-parsing.html))
  (deps (
    ../book/command-line-parsing.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/command-line-parsing/cal_add_days/cal.ml
    ../examples/code/command-line-parsing/cal_add_days/cal.sh
    ../examples/code/command-line-parsing/cal_add_interactive/cal.ml
    ../examples/code/command-line-parsing/cal_add_interactive2/cal.ml
    ../examples/code/command-line-parsing/cal_add_interactive2/cal.sh
    ../examples/code/command-line-parsing/cal_add_sub_days/cal.ml
    ../examples/code/command-line-parsing/cal_add_sub_days/cal.sh
    ../examples/code/command-line-parsing/cal_add_sub_days/jbuild
    ../examples/code/command-line-parsing/cal_completion.rawsh
    ../examples/code/command-line-parsing/main.mlt
    ../examples/code/command-line-parsing/md5/jbuild
    ../examples/code/command-line-parsing/md5/md5.ml
    ../examples/code/command-line-parsing/md5/md5.sh
    ../examples/code/command-line-parsing/md5_as_filename/md5.ml
    ../examples/code/command-line-parsing/md5_let_syntax/md5.ml
    ../examples/code/command-line-parsing/md5_let_syntax2/md5.ml
    ../examples/code/command-line-parsing/md5_multiarg/md5.ml
    ../examples/code/command-line-parsing/md5_multiarg/md5.sh
    ../examples/code/command-line-parsing/md5_sequence/md5.ml
    ../examples/code/command-line-parsing/md5_sequence/md5.sh
    ../examples/code/command-line-parsing/md5_succinct/md5.ml
    ../examples/code/command-line-parsing/md5_with_custom_arg/md5.ml
    ../examples/code/command-line-parsing/md5_with_custom_arg/run.errsh
    ../examples/code/command-line-parsing/md5_with_default_file/md5.ml
    ../examples/code/command-line-parsing/md5_with_default_file/md5.sh
    ../examples/code/command-line-parsing/md5_with_flags/md5.ml
    ../examples/code/command-line-parsing/md5_with_flags/md5.sh
    ../examples/code/command-line-parsing/md5_with_optional_file/md5.ml
    ../examples/code/command-line-parsing/md5_with_optional_file/md5.sh
    ../examples/code/command-line-parsing/md5_with_optional_file_broken/build.errsh
    ../examples/code/command-line-parsing/md5_with_optional_file_broken/md5.ml
    ../examples/code/command-line-parsing/opam.rawsh))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (compiler-backend.html))))

(rule (
  (targets (compiler-backend.html))
  (deps (
    ../book/compiler-backend.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/back-end-embed/build_embed.sh
    ../examples/code/back-end-embed/build_embed_binary.rawsh
    ../examples/code/back-end-embed/build_embed_native.rawsh
    ../examples/code/back-end-embed/embed_me1.ml
    ../examples/code/back-end-embed/embed_me2.ml
    ../examples/code/back-end-embed/link_custom.rawsh
    ../examples/code/back-end-embed/link_dllib.rawsh
    ../examples/code/back-end-embed/main.c
    ../examples/code/back-end-embed/run_debug_hello.sh
    ../examples/code/back-end/alternate_list/alternate_list.ml
    ../examples/code/back-end/alternate_list/jbuild
    ../examples/code/back-end/alternate_list/run_alternate_list.sh
    ../examples/code/back-end/asm_from_compare_mono.sh
    ../examples/code/back-end/bench_patterns/bench_patterns.ml
    ../examples/code/back-end/bench_patterns/jbuild
    ../examples/code/back-end/bench_patterns/run_bench_patterns.sh
    ../examples/code/back-end/bench_poly_and_mono/bench_poly_and_mono.ml
    ../examples/code/back-end/bench_poly_and_mono/jbuild
    ../examples/code/back-end/bench_poly_and_mono/run_bench_poly_and_mono.sh
    ../examples/code/back-end/cmp.S
    ../examples/code/back-end/compare_mono.ml
    ../examples/code/back-end/compare_poly.ml
    ../examples/code/back-end/compare_poly_asm.S
    ../examples/code/back-end/gdb_alternate0.rawsh
    ../examples/code/back-end/gdb_alternate1.rawsh
    ../examples/code/back-end/gdb_alternate2.rawsh
    ../examples/code/back-end/gdb_alternate3.rawsh
    ../examples/code/back-end/instr_for_pattern_monomorphic_small.sh
    ../examples/code/back-end/lambda_for_pattern_monomorphic_large.sh
    ../examples/code/back-end/lambda_for_pattern_monomorphic_small.sh
    ../examples/code/back-end/lambda_for_pattern_polymorphic.sh
    ../examples/code/back-end/opam_switch.rawsh
    ../examples/code/back-end/pattern_monomorphic_large.ml
    ../examples/code/back-end/pattern_monomorphic_small.ml
    ../examples/code/back-end/pattern_polymorphic.ml
    ../examples/code/back-end/perf_record.rawsh
    ../examples/code/back-end/perf_report.rawsh))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (compiler-frontend.html))))

(rule (
  (targets (compiler-frontend.html))
  (deps (
    ../book/compiler-frontend.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/front-end/alice.ml
    ../examples/code/front-end/alice.mli
    ../examples/code/front-end/alice_combined.ml
    ../examples/code/front-end/broken_module.ml
    ../examples/code/front-end/broken_poly.ml
    ../examples/code/front-end/broken_poly_with_annot.ml
    ../examples/code/front-end/build_broken_module.errsh
    ../examples/code/front-end/build_broken_poly.errsh
    ../examples/code/front-end/build_broken_poly_with_annot.errsh
    ../examples/code/front-end/build_follow_on_function.errsh
    ../examples/code/front-end/build_non_principal.sh
    ../examples/code/front-end/build_ocamldoc.rawsh
    ../examples/code/front-end/build_principal.sh
    ../examples/code/front-end/build_principal_corebuild.sh
    ../examples/code/front-end/build_type_conv_with_camlp4.rawsh
    ../examples/code/front-end/build_type_conv_without_camlp4.errsh
    ../examples/code/front-end/camlp4_dump.cmd
    ../examples/code/front-end/camlp4_toplevel.mlt
    ../examples/code/front-end/comparelib_test.ml
    ../examples/code/front-end/comparelib_test.mli
    ../examples/code/front-end/conflicting_interface.ml
    ../examples/code/front-end/conflicting_interface.mli
    ../examples/code/front-end/conflicting_interfaces.errsh
    ../examples/code/front-end/doc.ml
    ../examples/code/front-end/fixed_module.ml
    ../examples/code/front-end/follow_on_function.ml
    ../examples/code/front-end/inconsistent_compilation_units.rawsh
    ../examples/code/front-end/indent_follow_on_function.sh
    ../examples/code/front-end/indent_follow_on_function_fixed.sh
    ../examples/code/front-end/infer_typedef.sh
    ../examples/code/front-end/install_ocp_index.rawsh
    ../examples/code/front-end/let_notunit.ml
    ../examples/code/front-end/let_unit.syntax
    ../examples/code/front-end/non_principal.ml
    ../examples/code/front-end/parsetree_typedef.sh
    ../examples/code/front-end/principal.ml
    ../examples/code/front-end/process_comparelib_interface.sh
    ../examples/code/front-end/process_comparelib_test.sh
    ../examples/code/front-end/short_paths_1.rawsh
    ../examples/code/front-end/short_paths_2.rawsh
    ../examples/code/front-end/short_paths_3.rawsh
    ../examples/code/front-end/type_conv_example.ml
    ../examples/code/front-end/typedef.ml
    ../examples/code/front-end/typedef_objinfo.sh
    ../examples/code/front-end/typedtree_typedef.sh
    ../examples/code/front-end/unused_var.ml
    ../examples/code/ocp-index/index_ncurses.sh
    ../examples/code/packing/build_test.sh
    ../examples/code/packing/show_files.sh
    ../examples/code/packing/test.ml))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (concurrent-programming.html))))

(rule (
  (targets (concurrent-programming.html))
  (deps (
    ../book/concurrent-programming.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/async/better_echo.ml
    ../examples/code/async/echo/echo.ml
    ../examples/code/async/echo/jbuild
    ../examples/code/async/echo/run_echo.sh
    ../examples/code/async/main.mlt
    ../examples/code/async/pipe_write_break.rawscript
    ../examples/code/async/run_native_code_log_delays.rawsh
    ../examples/code/async/search/jbuild
    ../examples/code/async/search/run_search.sh
    ../examples/code/async/search/search.ml
    ../examples/code/async/search_out_of_order.ml
    ../examples/code/async/search_with_configurable_server/jbuild
    ../examples/code/async/search_with_configurable_server/run_search_with_configurable_server.errsh
    ../examples/code/async/search_with_configurable_server/search_with_configurable_server.ml
    ../examples/code/async/search_with_error_handling/jbuild
    ../examples/code/async/search_with_error_handling/run_search_with_error_handling.sh
    ../examples/code/async/search_with_error_handling/search_with_error_handling.ml
    ../examples/code/async/search_with_timeout.ml
    ../examples/code/async/search_with_timeout_no_leak/jbuild
    ../examples/code/async/search_with_timeout_no_leak/run_search_with_timeout_no_leak.sh
    ../examples/code/async/search_with_timeout_no_leak/search_with_timeout_no_leak.ml
    ../examples/code/async/search_with_timeout_no_leak_simple/search_with_timeout_no_leak_simple.ml))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (data-serialization.html))))

(rule (
  (targets (data-serialization.html))
  (deps (
    ../book/data-serialization.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/sexpr/auto_making_sexp.mlt
    ../examples/code/sexpr/basic.scm
    ../examples/code/sexpr/comment_heavy.scm
    ../examples/code/sexpr/example.scm
    ../examples/code/sexpr/example_load.mlt
    ../examples/code/sexpr/inline_sexp.mlt
    ../examples/code/sexpr/int_interval_manual_sexp.mli
    ../examples/code/sexpr/int_interval_nosexp.mli
    ../examples/code/sexpr/list_top_packages.sh
    ../examples/code/sexpr/manually_making_sexp.mlt
    ../examples/code/sexpr/print_sexp.mlt
    ../examples/code/sexpr/read_foo/build_read_foo.errsh
    ../examples/code/sexpr/read_foo/foo_broken_example.scm
    ../examples/code/sexpr/read_foo/jbuild
    ../examples/code/sexpr/read_foo/read_foo.ml
    ../examples/code/sexpr/read_foo_better_errors/build_read_foo_better_errors.errsh
    ../examples/code/sexpr/read_foo_better_errors/jbuild
    ../examples/code/sexpr/read_foo_better_errors/read_foo_better_errors.ml
    ../examples/code/sexpr/sexp.mli
    ../examples/code/sexpr/sexp_default.mlt
    ../examples/code/sexpr/sexp_list.mlt
    ../examples/code/sexpr/sexp_opaque.mlt
    ../examples/code/sexpr/sexp_option.mlt
    ../examples/code/sexpr/sexp_override.ml
    ../examples/code/sexpr/sexp_printer.mlt
    ../examples/code/sexpr/test_interval/build_test_interval.sh
    ../examples/code/sexpr/test_interval/int_interval.ml
    ../examples/code/sexpr/test_interval/int_interval.mli
    ../examples/code/sexpr/test_interval/jbuild
    ../examples/code/sexpr/test_interval/test_interval.ml
    ../examples/code/sexpr/test_interval_nosexp/build_test_interval_nosexp.errsh
    ../examples/code/sexpr/test_interval_nosexp/jbuild
    ../examples/code/sexpr/to_from_sexp.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (error-handling.html))))

(rule (
  (targets (error-handling.html))
  (deps (
    ../book/error-handling.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/error-handling/blow_up/blow_up.ml
    ../examples/code/error-handling/blow_up/build_blow_up.errsh
    ../examples/code/error-handling/blow_up/jbuild
    ../examples/code/error-handling/exn_cost/exn_cost.ml
    ../examples/code/error-handling/exn_cost/jbuild
    ../examples/code/error-handling/exn_cost/run_exn_cost.sh
    ../examples/code/error-handling/exn_cost/run_exn_cost_notrace.sh
    ../examples/code/error-handling/main.mlt
    ../examples/code/error-handling/result.mli
    ../examples/code/error-handling/sexpr.scm
    ../examples/code/error-handling/try_with.syntax))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (faqs.html))))

(rule (
  (targets (faqs.html))
  (deps (../book/faqs.html ../bin/bin/app.exe))
  (action (run rwo-build build faqs -o . -repo-root ..))))

(alias ((name site) (deps (files-modules-and-programs.html))))

(rule (
  (targets (files-modules-and-programs.html))
  (deps (
    ../book/files-modules-and-programs.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/files-modules-and-programs/abstract_username.ml
    ../examples/code/files-modules-and-programs/common.ml
    ../examples/code/files-modules-and-programs/ext_list.ml
    ../examples/code/files-modules-and-programs/ext_list.mli
    ../examples/code/files-modules-and-programs/freq-cyclic1/build.errsh
    ../examples/code/files-modules-and-programs/freq-cyclic1/counter.ml
    ../examples/code/files-modules-and-programs/freq-cyclic2/build.errsh
    ../examples/code/files-modules-and-programs/freq-cyclic2/counter.ml
    ../examples/code/files-modules-and-programs/freq-fast/counter.ml
    ../examples/code/files-modules-and-programs/freq-median/counter.ml
    ../examples/code/files-modules-and-programs/freq-median/counter.mli
    ../examples/code/files-modules-and-programs/freq-median/use_median_1.ml
    ../examples/code/files-modules-and-programs/freq-median/use_median_2.ml
    ../examples/code/files-modules-and-programs/freq-obuild/freq.sh
    ../examples/code/files-modules-and-programs/freq-obuild/jbuild
    ../examples/code/files-modules-and-programs/freq-with-counter/build.sh
    ../examples/code/files-modules-and-programs/freq-with-counter/counter.ml
    ../examples/code/files-modules-and-programs/freq-with-counter/freq.ml
    ../examples/code/files-modules-and-programs/freq-with-missing-def/build.errsh
    ../examples/code/files-modules-and-programs/freq-with-missing-def/counter.mli
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract-fixed/build.sh
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract-fixed/freq.ml
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/build.errsh
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/counter.ml
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/counter.mli
    ../examples/code/files-modules-and-programs/freq-with-sig-mismatch/build.errsh
    ../examples/code/files-modules-and-programs/freq-with-sig-mismatch/counter.mli
    ../examples/code/files-modules-and-programs/freq-with-sig/counter.mli
    ../examples/code/files-modules-and-programs/freq-with-type-mismatch/build.errsh
    ../examples/code/files-modules-and-programs/freq-with-type-mismatch/counter.mli
    ../examples/code/files-modules-and-programs/freq/freq.ml
    ../examples/code/files-modules-and-programs/freq/simple_build.sh
    ../examples/code/files-modules-and-programs/freq/simple_build_fail.errsh
    ../examples/code/files-modules-and-programs/intro.mlt
    ../examples/code/files-modules-and-programs/main.mlt
    ../examples/code/files-modules-and-programs/module.syntax
    ../examples/code/files-modules-and-programs/session_info/build_session_info.errsh
    ../examples/code/files-modules-and-programs/session_info/session_info.ml
    ../examples/code/files-modules-and-programs/val.syntax))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (first-class-modules.html))))

(rule (
  (targets (first-class-modules.html))
  (deps (
    ../book/first-class-modules.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/fcm/loader_cli1.rawsh
    ../examples/code/fcm/loader_cli2.rawsh
    ../examples/code/fcm/loader_cli3.rawsh
    ../examples/code/fcm/loader_cli4.rawsh
    ../examples/code/fcm/main.mlt
    ../examples/code/fcm/pack.syntax
    ../examples/code/fcm/query-syntax.scm
    ../examples/code/fcm/query_example.rawscript
    ../examples/code/fcm/query_handler.mlt
    ../examples/code/fcm/query_handler_loader/build_query_handler_loader.sh
    ../examples/code/fcm/query_handler_loader/jbuild
    ../examples/code/fcm/query_handler_loader/query_handler.ml
    ../examples/code/fcm/query_handler_loader/query_handler_core.ml
    ../examples/code/fcm/query_handler_loader/query_handler_loader.ml
    ../examples/code/fcm/unpack.syntax))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (foreign-function-interface.html))))

(rule (
  (targets (foreign-function-interface.html))
  (deps (
    ../book/foreign-function-interface.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/ctypes/ctypes.mli
    ../examples/code/ctypes/ctypes_impl.ml
    ../examples/code/ffi/datetime/build_datetime.sh
    ../examples/code/ffi/datetime/datetime.ml
    ../examples/code/ffi/datetime/jbuild
    ../examples/code/ffi/hello/build_hello.sh
    ../examples/code/ffi/hello/hello.ml
    ../examples/code/ffi/hello/jbuild
    ../examples/code/ffi/install.rawsh
    ../examples/code/ffi/ncurses/infer_ncurses.sh
    ../examples/code/ffi/ncurses/ncurses.h
    ../examples/code/ffi/ncurses/ncurses.ml
    ../examples/code/ffi/ncurses/ncurses.mli
    ../examples/code/ffi/posix.mlt
    ../examples/code/ffi/posix_headers.h
    ../examples/code/ffi/qsort.mlt
    ../examples/code/ffi/qsort/build_qsort.sh
    ../examples/code/ffi/qsort/jbuild
    ../examples/code/ffi/qsort/qsort.h
    ../examples/code/ffi/qsort/qsort.ml
    ../examples/code/ffi/qsort/qsort.mli
    ../examples/code/ffi/qsort/qsort_typedef.h
    ../examples/code/ffi/return_c_frag.c
    ../examples/code/ffi/return_c_frag.h
    ../examples/code/ffi/return_c_uncurried.c
    ../examples/code/ffi/return_frag.ml
    ../examples/code/ffi/timeval_headers.h))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (functors.html))))

(rule (
  (targets (functors.html))
  (deps (
    ../book/functors.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/functors/compare_example.ml
    ../examples/code/functors/destructive_sub.syntax
    ../examples/code/functors/extended_fqueue.ml
    ../examples/code/functors/extended_fqueue.mli
    ../examples/code/functors/foldable.ml
    ../examples/code/functors/fqueue.ml
    ../examples/code/functors/fqueue.mli
    ../examples/code/functors/main.mlt
    ../examples/code/functors/multi_sharing_constraint.syntax
    ../examples/code/functors/sexpable.ml
    ../examples/code/functors/sharing_constraint.syntax))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (garbage-collector.html))))

(rule (
  (targets (garbage-collector.html))
  (deps (
    ../book/garbage-collector.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/gc/barrier_bench/barrier_bench.ml
    ../examples/code/gc/barrier_bench/barrier_bench.sh
    ../examples/code/gc/barrier_bench/jbuild
    ../examples/code/gc/finalizer/finalizer.ml
    ../examples/code/gc/finalizer/jbuild
    ../examples/code/gc/finalizer/run_finalizer.sh
    ../examples/code/gc/tune.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (guided-tour.html))))

(rule (
  (targets (guided-tour.html))
  (deps (
    ../book/guided-tour.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/guided-tour/local_let.mlt
    ../examples/code/guided-tour/main.mlt
    ../examples/code/guided-tour/recursion.ml
    ../examples/code/guided-tour/sum/build_sum.sh
    ../examples/code/guided-tour/sum/jbuild
    ../examples/code/guided-tour/sum/sum.ml
    ../examples/code/guided-tour/sum/sum.rawsh))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (imperative-programming.html))))

(rule (
  (targets (imperative-programming.html))
  (deps (
    ../book/imperative-programming.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/imperative-programming/array-get.syntax
    ../examples/code/imperative-programming/array-set.syntax
    ../examples/code/imperative-programming/bigarray.syntax
    ../examples/code/imperative-programming/dictionary.ml
    ../examples/code/imperative-programming/dictionary.mli
    ../examples/code/imperative-programming/dictionary2.ml
    ../examples/code/imperative-programming/dlist.ml
    ../examples/code/imperative-programming/dlist.mli
    ../examples/code/imperative-programming/examples.mlt
    ../examples/code/imperative-programming/fib.mlt
    ../examples/code/imperative-programming/file.mlt
    ../examples/code/imperative-programming/file2.mlt
    ../examples/code/imperative-programming/for.mlt
    ../examples/code/imperative-programming/lazy.mlt
    ../examples/code/imperative-programming/let-unit.syntax
    ../examples/code/imperative-programming/let_rec.ml
    ../examples/code/imperative-programming/letrec.mlt
    ../examples/code/imperative-programming/memo.mlt
    ../examples/code/imperative-programming/order.mlt
    ../examples/code/imperative-programming/printf.mlt
    ../examples/code/imperative-programming/ref.mlt
    ../examples/code/imperative-programming/remember_type.ml
    ../examples/code/imperative-programming/semicolon.syntax
    ../examples/code/imperative-programming/string.syntax
    ../examples/code/imperative-programming/time_converter/time_converter.ml
    ../examples/code/imperative-programming/time_converter/time_converter.rawsh
    ../examples/code/imperative-programming/time_converter2.ml
    ../examples/code/imperative-programming/time_converter2.rawsh
    ../examples/code/imperative-programming/value_restriction.mlt
    ../examples/code/imperative-programming/weak.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (index.html))))

(rule (
  (targets (index.html))
  (deps (../book/index.html ../bin/bin/app.exe))
  (action (run rwo-build build index -o . -repo-root ..))))

(alias ((name site) (deps (install.html))))

(rule (
  (targets (install.html))
  (deps (../book/install.html ../bin/bin/app.exe))
  (action (run rwo-build build install -o . -repo-root ..))))

(alias ((name site) (deps (json.html))))

(rule (
  (targets (json.html))
  (deps (
    ../book/json.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/json/book.json
    ../examples/code/json/build_github_atd.sh
    ../examples/code/json/build_json.mlt
    ../examples/code/json/github.atd
    ../examples/code/json/github_j_excerpt.mli
    ../examples/code/json/github_org_info/github_org.atd
    ../examples/code/json/github_org_info/github_org.sh
    ../examples/code/json/github_org_info/github_org_info.ml
    ../examples/code/json/github_org_info/jbuild
    ../examples/code/json/install.mlt
    ../examples/code/json/install_atdgen.rawsh
    ../examples/code/json/list_excerpt.mli
    ../examples/code/json/parse_book.mlt
    ../examples/code/json/parse_book/jbuild
    ../examples/code/json/parse_book/parse_book.ml
    ../examples/code/json/parse_book/run_parse_book.sh
    ../examples/code/json/read_json/jbuild
    ../examples/code/json/read_json/read_json.ml
    ../examples/code/json/read_json/run_read_json.sh
    ../examples/code/json/yojson_basic.mli
    ../examples/code/json/yojson_basic_simple.mli
    ../examples/code/json/yojson_safe.mli))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (lists-and-patterns.html))))

(rule (
  (targets (lists-and-patterns.html))
  (deps (
    ../book/lists-and-patterns.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/lists-and-patterns/main.mlt
    ../examples/code/lists-and-patterns/poly.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (maps-and-hashtables.html))))

(rule (
  (targets (maps-and-hashtables.html))
  (deps (
    ../book/maps-and-hashtables.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/files-modules-and-programs/freq-fast/counter.ml
    ../examples/code/files-modules-and-programs/freq-fast/counter.mli
    ../examples/code/maps-and-hash-tables/core_phys_equal.mlt
    ../examples/code/maps-and-hash-tables/main.mlt
    ../examples/code/maps-and-hash-tables/map_vs_hash/jbuild
    ../examples/code/maps-and-hash-tables/map_vs_hash/map_vs_hash.ml
    ../examples/code/maps-and-hash-tables/map_vs_hash/run_map_vs_hash.sh
    ../examples/code/maps-and-hash-tables/map_vs_hash2/jbuild
    ../examples/code/maps-and-hash-tables/map_vs_hash2/map_vs_hash2.ml
    ../examples/code/maps-and-hash-tables/map_vs_hash2/run_map_vs_hash2.sh
    ../examples/code/maps-and-hash-tables/phys_equal.rawscript))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (objects.html))))

(rule (
  (targets (objects.html))
  (deps (
    ../book/objects.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/objects/IsBarbell.java
    ../examples/code/objects/Shape.java
    ../examples/code/objects/immutable.mlt
    ../examples/code/objects/is_barbell.ml
    ../examples/code/objects/narrowing.ml
    ../examples/code/objects/polymorphism.mlt
    ../examples/code/objects/row_polymorphism.mlt
    ../examples/code/objects/stack.mlt
    ../examples/code/objects/subtyping.ml
    ../examples/code/objects/subtyping.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (parsing-with-ocamllex-and-menhir.html))))

(rule (
  (targets (parsing-with-ocamllex-and-menhir.html))
  (deps (
    ../book/parsing-with-ocamllex-and-menhir.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/parsing-test/build_test.sh
    ../examples/code/parsing-test/run_broken_test.errsh
    ../examples/code/parsing-test/test.ml
    ../examples/code/parsing-test/test1.json
    ../examples/code/parsing/build_short_parser.sh
    ../examples/code/parsing/example.json
    ../examples/code/parsing/jbuild
    ../examples/code/parsing/json.ml
    ../examples/code/parsing/lexer.mll
    ../examples/code/parsing/lexer_int_fragment.mll
    ../examples/code/parsing/manual_token_type.ml
    ../examples/code/parsing/parsed_example.ml
    ../examples/code/parsing/parser.mly
    ../examples/code/parsing/prog.mli
    ../examples/code/parsing/quadratic_rule.mly
    ../examples/code/parsing/right_rec_rule.mly
    ../examples/code/parsing/short_parser.mly
    ../examples/code/parsing/tokens.ml))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (prologue.html))))

(rule (
  (targets (prologue.html))
  (deps (../book/prologue.html ../bin/bin/app.exe ../book/toc.scm))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (records.html))))

(rule (
  (targets (records.html))
  (deps (
    ../book/records.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/records/functional_update.syntax
    ../examples/code/records/main.mlt
    ../examples/code/records/main2.mlt
    ../examples/code/records/record.syntax
    ../examples/code/records/warn_help.sh))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (runtime-memory-layout.html))))

(rule (
  (targets (runtime-memory-layout.html))
  (deps (
    ../book/runtime-memory-layout.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/memory-repr/custom_ops.c
    ../examples/code/memory-repr/reprs.mlt
    ../examples/code/memory-repr/simple_record.mlt))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (toc.html))))

(rule (
  (targets (toc.html))
  (deps (../book/toc.html ../bin/bin/app.exe ../book/toc.scm))
  (action (run rwo-build build toc -o . -repo-root ..))))

(alias ((name site) (deps (variables-and-functions.html))))

(rule (
  (targets (variables-and-functions.html))
  (deps (
    ../book/variables-and-functions.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/variables-and-functions/abs_diff.mli
    ../examples/code/variables-and-functions/htable_sig1.ml
    ../examples/code/variables-and-functions/htable_sig2.ml
    ../examples/code/variables-and-functions/let.syntax
    ../examples/code/variables-and-functions/let_in.syntax
    ../examples/code/variables-and-functions/main.mlt
    ../examples/code/variables-and-functions/numerical_deriv_alt_sig.mli
    ../examples/code/variables-and-functions/operators.syntax
    ../examples/code/variables-and-functions/substring_sig1.ml
    ../examples/code/variables-and-functions/substring_sig2.ml))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (variants.html))))

(rule (
  (targets (variants.html))
  (deps (
    ../book/variants.html
    ../bin/bin/app.exe
    ../book/toc.scm
    ../examples/code/variants-termcol-annotated/build.errsh
    ../examples/code/variants-termcol-annotated/jbuild
    ../examples/code/variants-termcol-annotated/terminal_color.ml
    ../examples/code/variants-termcol-fixed/terminal_color.ml
    ../examples/code/variants-termcol/terminal_color.ml
    ../examples/code/variants-termcol/terminal_color.mli
    ../examples/code/variants/blang.mlt
    ../examples/code/variants/catch_all.mlt
    ../examples/code/variants/logger.mlt
    ../examples/code/variants/main.mlt
    ../examples/code/variants/variant.syntax))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

