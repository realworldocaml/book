(jbuild_version 1)

(alias (
  (name site)
  (deps (
    css/app.css
    css/prism.css
    images/jason.jpg
    images/plain-bg.jpg
    images/anil.jpg
    images/yaron.jpg
    images/cover.png
    images/book-cover.jpg
    images/bg.jpg
    images/front-end/pipeline.png
    images/gc/minor_heap.png
    images/imperative-programming/edit_distance2.png
    images/imperative-programming/edit_distance.png
    images/lists-and-patterns/lists_layout.png
    images/memory-repr/string_block.png
    images/memory-repr/string_size_calc.png
    images/memory-repr/block.png
    images/memory-repr/float_array_layout.png
    images/memory-repr/tuple_layout.png
    js/_functions.js
    js/prism.js
    js/jquery.min.js
    js/modernizr.js
    js/app.js
    js/discourse.js
    js/min/modernizr-min.js
    js/min/app-min.js
    theme/pdf/pdf.css))))

(alias ((name site) (deps (00-prologue.html))))

(rule (
  (targets (00-prologue.html))
  (deps (../book/00-prologue.html ../bin/bin/app.exe))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (01-guided-tour.html))))

(rule (
  (targets (01-guided-tour.html))
  (deps (
    ../book/01-guided-tour.html
    ../bin/bin/app.exe
    ../examples/code/guided-tour/local_let.topscript.sexp
    ../examples/code/guided-tour/main.topscript.sexp
    ../examples/code/guided-tour/recursion.ml.sexp
    ../examples/code/guided-tour/sum/build_sum.sh.sexp
    ../examples/code/guided-tour/sum/jbuild.sexp
    ../examples/code/guided-tour/sum/sum.ml.sexp
    ../examples/code/guided-tour/sum/sum.rawsh.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (02-variables-and-functions.html))))

(rule (
  (targets (02-variables-and-functions.html))
  (deps (
    ../book/02-variables-and-functions.html
    ../bin/bin/app.exe
    ../examples/code/variables-and-functions/abs_diff.mli.sexp
    ../examples/code/variables-and-functions/htable_sig1.ml.sexp
    ../examples/code/variables-and-functions/htable_sig2.ml.sexp
    ../examples/code/variables-and-functions/let.syntax.sexp
    ../examples/code/variables-and-functions/let_in.syntax.sexp
    ../examples/code/variables-and-functions/main.topscript.sexp
    ../examples/code/variables-and-functions/numerical_deriv_alt_sig.mli.sexp
    ../examples/code/variables-and-functions/operators.syntax.sexp
    ../examples/code/variables-and-functions/substring_sig1.ml.sexp
    ../examples/code/variables-and-functions/substring_sig2.ml.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (03-lists-and-patterns.html))))

(rule (
  (targets (03-lists-and-patterns.html))
  (deps (
    ../book/03-lists-and-patterns.html
    ../bin/bin/app.exe
    ../examples/code/lists-and-patterns/main.topscript.sexp
    ../examples/code/lists-and-patterns/poly.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (04-files-modules-and-programs.html))))

(rule (
  (targets (04-files-modules-and-programs.html))
  (deps (
    ../book/04-files-modules-and-programs.html
    ../bin/bin/app.exe
    ../examples/code/files-modules-and-programs/abstract_username.ml.sexp
    ../examples/code/files-modules-and-programs/common.ml.sexp
    ../examples/code/files-modules-and-programs/ext_list.ml.sexp
    ../examples/code/files-modules-and-programs/ext_list.mli.sexp
    ../examples/code/files-modules-and-programs/freq-cyclic1/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-cyclic1/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-cyclic2/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-cyclic2/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-fast/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-median/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-median/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq-median/use_median_1.ml.sexp
    ../examples/code/files-modules-and-programs/freq-median/use_median_2.ml.sexp
    ../examples/code/files-modules-and-programs/freq-obuild/build.sh.sexp
    ../examples/code/files-modules-and-programs/freq-obuild/jbuild.sexp
    ../examples/code/files-modules-and-programs/freq-obuild/test.sh.sexp
    ../examples/code/files-modules-and-programs/freq-with-counter/build.sh.sexp
    ../examples/code/files-modules-and-programs/freq-with-counter/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-with-counter/freq.ml.sexp
    ../examples/code/files-modules-and-programs/freq-with-missing-def/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-with-missing-def/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract-fixed/build.sh.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract-fixed/freq.ml.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-abstract/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-mismatch/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig-mismatch/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq-with-sig/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq-with-type-mismatch/build.errsh.sexp
    ../examples/code/files-modules-and-programs/freq-with-type-mismatch/counter.mli.sexp
    ../examples/code/files-modules-and-programs/freq/freq.ml.sexp
    ../examples/code/files-modules-and-programs/freq/simple_build.sh.sexp
    ../examples/code/files-modules-and-programs/freq/simple_build_fail.errsh.sexp
    ../examples/code/files-modules-and-programs/intro.topscript.sexp
    ../examples/code/files-modules-and-programs/main.topscript.sexp
    ../examples/code/files-modules-and-programs/module.syntax.sexp
    ../examples/code/files-modules-and-programs/session_info/build_session_info.errsh.sexp
    ../examples/code/files-modules-and-programs/session_info/session_info.ml.sexp
    ../examples/code/files-modules-and-programs/val.syntax.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (05-records.html))))

(rule (
  (targets (05-records.html))
  (deps (
    ../book/05-records.html
    ../bin/bin/app.exe
    ../examples/code/records/functional_update.syntax.sexp
    ../examples/code/records/main.topscript.sexp
    ../examples/code/records/main2.topscript.sexp
    ../examples/code/records/record.syntax.sexp
    ../examples/code/records/warn_help.sh.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (06-variants.html))))

(rule (
  (targets (06-variants.html))
  (deps (
    ../book/06-variants.html
    ../bin/bin/app.exe
    ../examples/code/variants-termcol-annotated/build.errsh.sexp
    ../examples/code/variants-termcol-annotated/jbuild.sexp
    ../examples/code/variants-termcol-annotated/terminal_color.ml.sexp
    ../examples/code/variants-termcol-fixed/terminal_color.ml.sexp
    ../examples/code/variants-termcol/terminal_color.ml.sexp
    ../examples/code/variants-termcol/terminal_color.mli.sexp
    ../examples/code/variants/blang.topscript.sexp
    ../examples/code/variants/catch_all.topscript.sexp
    ../examples/code/variants/logger.topscript.sexp
    ../examples/code/variants/main.topscript.sexp
    ../examples/code/variants/variant.syntax.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (07-error-handling.html))))

(rule (
  (targets (07-error-handling.html))
  (deps (
    ../book/07-error-handling.html
    ../bin/bin/app.exe
    ../examples/code/error-handling/blow_up/blow_up.ml.sexp
    ../examples/code/error-handling/blow_up/build_blow_up.errsh.sexp
    ../examples/code/error-handling/blow_up/build_blow_up_notrace.errsh.sexp
    ../examples/code/error-handling/blow_up/jbuild.sexp
    ../examples/code/error-handling/exn_cost/exn_cost.ml.sexp
    ../examples/code/error-handling/exn_cost/jbuild.sexp
    ../examples/code/error-handling/exn_cost/run_exn_cost.sh.sexp
    ../examples/code/error-handling/exn_cost/run_exn_cost_notrace.sh.sexp
    ../examples/code/error-handling/main.topscript.sexp
    ../examples/code/error-handling/result.mli.sexp
    ../examples/code/error-handling/sexpr.scm.sexp
    ../examples/code/error-handling/try_with.syntax.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (08-imperative-programming.html))))

(rule (
  (targets (08-imperative-programming.html))
  (deps (
    ../book/08-imperative-programming.html
    ../bin/bin/app.exe
    ../examples/code/imperative-programming/array-get.syntax.sexp
    ../examples/code/imperative-programming/array-set.syntax.sexp
    ../examples/code/imperative-programming/bigarray.syntax.sexp
    ../examples/code/imperative-programming/dictionary.ml.sexp
    ../examples/code/imperative-programming/dictionary.mli.sexp
    ../examples/code/imperative-programming/dictionary2.ml.sexp
    ../examples/code/imperative-programming/dlist.ml.sexp
    ../examples/code/imperative-programming/dlist.mli.sexp
    ../examples/code/imperative-programming/examples.topscript.sexp
    ../examples/code/imperative-programming/fib.topscript.sexp
    ../examples/code/imperative-programming/file.topscript.sexp
    ../examples/code/imperative-programming/file2.topscript.sexp
    ../examples/code/imperative-programming/for.topscript.sexp
    ../examples/code/imperative-programming/lazy.topscript.sexp
    ../examples/code/imperative-programming/let-unit.syntax.sexp
    ../examples/code/imperative-programming/let_rec.ml.sexp
    ../examples/code/imperative-programming/letrec.topscript.sexp
    ../examples/code/imperative-programming/memo.topscript.sexp
    ../examples/code/imperative-programming/order.topscript.sexp
    ../examples/code/imperative-programming/printf.topscript.sexp
    ../examples/code/imperative-programming/ref.topscript.sexp
    ../examples/code/imperative-programming/remember_type.ml.sexp
    ../examples/code/imperative-programming/semicolon.syntax.sexp
    ../examples/code/imperative-programming/string.syntax.sexp
    ../examples/code/imperative-programming/time_converter/time_converter.ml.sexp
    ../examples/code/imperative-programming/time_converter/time_converter.rawsh.sexp
    ../examples/code/imperative-programming/time_converter2.ml.sexp
    ../examples/code/imperative-programming/time_converter2.rawsh.sexp
    ../examples/code/imperative-programming/value_restriction.topscript.sexp
    ../examples/code/imperative-programming/weak.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (09-functors.html))))

(rule (
  (targets (09-functors.html))
  (deps (
    ../book/09-functors.html
    ../bin/bin/app.exe
    ../examples/code/functors/compare_example.ml.sexp
    ../examples/code/functors/destructive_sub.syntax.sexp
    ../examples/code/functors/extended_fqueue.ml.sexp
    ../examples/code/functors/extended_fqueue.mli.sexp
    ../examples/code/functors/foldable.ml.sexp
    ../examples/code/functors/fqueue.ml.sexp
    ../examples/code/functors/fqueue.mli.sexp
    ../examples/code/functors/main.topscript.sexp
    ../examples/code/functors/multi_sharing_constraint.syntax.sexp
    ../examples/code/functors/sexpable.ml.sexp
    ../examples/code/functors/sharing_constraint.syntax.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (10-first-class-modules.html))))

(rule (
  (targets (10-first-class-modules.html))
  (deps (
    ../book/10-first-class-modules.html
    ../bin/bin/app.exe
    ../examples/code/fcm/loader_cli1.rawsh.sexp
    ../examples/code/fcm/loader_cli2.rawsh.sexp
    ../examples/code/fcm/loader_cli3.rawsh.sexp
    ../examples/code/fcm/loader_cli4.rawsh.sexp
    ../examples/code/fcm/main.topscript.sexp
    ../examples/code/fcm/pack.syntax.sexp
    ../examples/code/fcm/query-syntax.scm.sexp
    ../examples/code/fcm/query_example.rawscript.sexp
    ../examples/code/fcm/query_handler.topscript.sexp
    ../examples/code/fcm/query_handler_loader/build_query_handler_loader.sh.sexp
    ../examples/code/fcm/query_handler_loader/jbuild.sexp
    ../examples/code/fcm/query_handler_loader/query_handler.ml.sexp
    ../examples/code/fcm/query_handler_loader/query_handler_core.ml.sexp
    ../examples/code/fcm/query_handler_loader/query_handler_loader.ml.sexp
    ../examples/code/fcm/unpack.syntax.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (11-objects.html))))

(rule (
  (targets (11-objects.html))
  (deps (
    ../book/11-objects.html
    ../bin/bin/app.exe
    ../examples/code/objects/IsBarbell.java.sexp
    ../examples/code/objects/Shape.java.sexp
    ../examples/code/objects/immutable.topscript.sexp
    ../examples/code/objects/is_barbell.ml.sexp
    ../examples/code/objects/narrowing.ml.sexp
    ../examples/code/objects/polymorphism.topscript.sexp
    ../examples/code/objects/row_polymorphism.topscript.sexp
    ../examples/code/objects/stack.topscript.sexp
    ../examples/code/objects/subtyping.ml.sexp
    ../examples/code/objects/subtyping.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (12-classes.html))))

(rule (
  (targets (12-classes.html))
  (deps (
    ../book/12-classes.html
    ../bin/bin/app.exe
    ../examples/code/classes-async/multiple_inheritance.ml.sexp
    ../examples/code/classes-async/multiple_inheritance_wrong.ml.sexp
    ../examples/code/classes-async/shapes/build_shapes.sh.sexp
    ../examples/code/classes-async/shapes/jbuild.sexp
    ../examples/code/classes-async/shapes/shapes.ml.sexp
    ../examples/code/classes-async/verbose_shapes.ml.sexp
    ../examples/code/classes/Iterator.java.sexp
    ../examples/code/classes/binary.topscript.sexp
    ../examples/code/classes/binary_larger.ml.sexp
    ../examples/code/classes/binary_module.ml.sexp
    ../examples/code/classes/citerator.cpp.sexp
    ../examples/code/classes/class_types_stack.ml.sexp
    ../examples/code/classes/doc.ml.sexp
    ../examples/code/classes/initializer.topscript.sexp
    ../examples/code/classes/istack.topscript.sexp
    ../examples/code/classes/iter.topscript.sexp
    ../examples/code/classes/stack.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (13-maps-and-hashtables.html))))

(rule (
  (targets (13-maps-and-hashtables.html))
  (deps (
    ../book/13-maps-and-hashtables.html
    ../bin/bin/app.exe
    ../examples/code/files-modules-and-programs/freq-fast/counter.ml.sexp
    ../examples/code/files-modules-and-programs/freq-fast/counter.mli.sexp
    ../examples/code/maps-and-hash-tables/comparable.ml.sexp
    ../examples/code/maps-and-hash-tables/core_phys_equal.topscript.sexp
    ../examples/code/maps-and-hash-tables/main.topscript.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash/jbuild.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash/map_vs_hash.ml.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash/run_map_vs_hash.sh.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash2/jbuild.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash2/map_vs_hash2.ml.sexp
    ../examples/code/maps-and-hash-tables/map_vs_hash2/run_map_vs_hash2.sh.sexp
    ../examples/code/maps-and-hash-tables/phys_equal.rawscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (14-command-line-parsing.html))))

(rule (
  (targets (14-command-line-parsing.html))
  (deps (
    ../book/14-command-line-parsing.html
    ../bin/bin/app.exe
    ../examples/code/command-line-parsing/basic.topscript.sexp
    ../examples/code/command-line-parsing/basic_md5/basic_md5.ml.sexp
    ../examples/code/command-line-parsing/basic_md5/build_basic_md5.sh.sexp
    ../examples/code/command-line-parsing/basic_md5/get_basic_md5_help.errsh.sexp
    ../examples/code/command-line-parsing/basic_md5/get_basic_md5_version.sh.sexp
    ../examples/code/command-line-parsing/basic_md5/jbuild.sexp
    ../examples/code/command-line-parsing/basic_md5/run_basic_md5.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_as_filename/basic_md5_as_filename.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_as_filename/run_basic_md5_as_filename.errsh.sexp
    ../examples/code/command-line-parsing/basic_md5_sequence.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_succinct.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_custom_arg/basic_md5_with_custom_arg.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_custom_arg/run_basic_md5_with_custom_arg.errsh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_flags/basic_md5_with_flags.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_flags/build_basic_md5_with_flags.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_flags/jbuild.sexp
    ../examples/code/command-line-parsing/basic_md5_with_flags/md5_completion.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_flags/run_basic_md5_flags_help.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file/basic_md5_with_default_file.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file/basic_md5_with_optional_file.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file/build_basic_and_default_md5.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file/jbuild.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file/run_basic_and_default_md5.sh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file_broken/basic_md5_with_optional_file_broken.ml.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file_broken/build_basic_md5_with_optional_file_broken.errsh.sexp
    ../examples/code/command-line-parsing/basic_md5_with_optional_file_broken/jbuild.sexp
    ../examples/code/command-line-parsing/cal_add_days.ml.sexp
    ../examples/code/command-line-parsing/cal_add_interactive/build_and_run_cal_add_interactive.sh.sexp
    ../examples/code/command-line-parsing/cal_add_interactive/cal_add_interactive.ml.sexp
    ../examples/code/command-line-parsing/cal_add_labels.ml.sexp
    ../examples/code/command-line-parsing/cal_add_sub_days/build_cal_add_sub_days.sh.sexp
    ../examples/code/command-line-parsing/cal_add_sub_days/cal_add_sub_days.ml.sexp
    ../examples/code/command-line-parsing/cal_add_sub_days/jbuild.sexp
    ../examples/code/command-line-parsing/cal_add_sub_days/run_cal_add_sub_days.sh.sexp
    ../examples/code/command-line-parsing/cal_append.ml.sexp
    ../examples/code/command-line-parsing/cal_append_broken/build_cal_append_broken.errsh.sexp
    ../examples/code/command-line-parsing/cal_append_broken/jbuild.sexp
    ../examples/code/command-line-parsing/cal_completion.rawsh.sexp
    ../examples/code/command-line-parsing/command_types.topscript.sexp
    ../examples/code/command-line-parsing/group.topscript.sexp
    ../examples/code/command-line-parsing/opam.rawsh.sexp
    ../examples/code/command-line-parsing/step.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (15-json.html))))

(rule (
  (targets (15-json.html))
  (deps (
    ../book/15-json.html
    ../bin/bin/app.exe
    ../examples/code/json/book.json.sexp
    ../examples/code/json/build_github_atd.sh.sexp
    ../examples/code/json/build_json.topscript.sexp
    ../examples/code/json/github.atd.sexp
    ../examples/code/json/github_j_excerpt.mli.sexp
    ../examples/code/json/github_org_info/build_github_org.sh.sexp
    ../examples/code/json/github_org_info/generate_github_org_json.sh.sexp
    ../examples/code/json/github_org_info/generate_github_org_types.sh.sexp
    ../examples/code/json/github_org_info/github_org.atd.sexp
    ../examples/code/json/github_org_info/github_org_info.ml.sexp
    ../examples/code/json/github_org_info/jbuild.sexp
    ../examples/code/json/github_org_info/run_github_org.sh.sexp
    ../examples/code/json/install.topscript.sexp
    ../examples/code/json/install_atdgen.rawsh.sexp
    ../examples/code/json/list_excerpt.mli.sexp
    ../examples/code/json/parse_book.topscript.sexp
    ../examples/code/json/parse_book/jbuild.sexp
    ../examples/code/json/parse_book/parse_book.ml.sexp
    ../examples/code/json/parse_book/run_parse_book.sh.sexp
    ../examples/code/json/read_json/jbuild.sexp
    ../examples/code/json/read_json/read_json.ml.sexp
    ../examples/code/json/read_json/run_read_json.sh.sexp
    ../examples/code/json/yojson_basic.mli.sexp
    ../examples/code/json/yojson_basic_simple.mli.sexp
    ../examples/code/json/yojson_safe.mli.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (16-parsing-with-ocamllex-and-menhir.html))))

(rule (
  (targets (16-parsing-with-ocamllex-and-menhir.html))
  (deps (
    ../book/16-parsing-with-ocamllex-and-menhir.html
    ../bin/bin/app.exe
    ../examples/code/parsing-test/build_test.sh.sexp
    ../examples/code/parsing-test/run_broken_test.errsh.sexp
    ../examples/code/parsing-test/test.ml.sexp
    ../examples/code/parsing-test/test1.json.sexp
    ../examples/code/parsing/build_short_parser.sh.sexp
    ../examples/code/parsing/example.json.sexp
    ../examples/code/parsing/jbuild.sexp
    ../examples/code/parsing/json.ml.sexp
    ../examples/code/parsing/lexer.mll.sexp
    ../examples/code/parsing/lexer_int_fragment.mll.sexp
    ../examples/code/parsing/manual_token_type.ml.sexp
    ../examples/code/parsing/parsed_example.ml.sexp
    ../examples/code/parsing/parser.mly.sexp
    ../examples/code/parsing/prog.mli.sexp
    ../examples/code/parsing/quadratic_rule.mly.sexp
    ../examples/code/parsing/right_rec_rule.mly.sexp
    ../examples/code/parsing/short_parser.mly.sexp
    ../examples/code/parsing/tokens.ml.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (17-data-serialization.html))))

(rule (
  (targets (17-data-serialization.html))
  (deps (
    ../book/17-data-serialization.html
    ../bin/bin/app.exe
    ../examples/code/sexpr/auto_making_sexp.topscript.sexp
    ../examples/code/sexpr/basic.scm.sexp
    ../examples/code/sexpr/comment_heavy.scm.sexp
    ../examples/code/sexpr/example.scm.sexp
    ../examples/code/sexpr/example_load.topscript.sexp
    ../examples/code/sexpr/inline_sexp.topscript.sexp
    ../examples/code/sexpr/int_interval_manual_sexp.mli.sexp
    ../examples/code/sexpr/int_interval_nosexp.mli.sexp
    ../examples/code/sexpr/list_top_packages.sh.sexp
    ../examples/code/sexpr/manually_making_sexp.topscript.sexp
    ../examples/code/sexpr/print_sexp.topscript.sexp
    ../examples/code/sexpr/read_foo/build_read_foo.errsh.sexp
    ../examples/code/sexpr/read_foo/foo_broken_example.scm.sexp
    ../examples/code/sexpr/read_foo/jbuild.sexp
    ../examples/code/sexpr/read_foo/read_foo.ml.sexp
    ../examples/code/sexpr/read_foo_better_errors/build_read_foo_better_errors.errsh.sexp
    ../examples/code/sexpr/read_foo_better_errors/jbuild.sexp
    ../examples/code/sexpr/read_foo_better_errors/read_foo_better_errors.ml.sexp
    ../examples/code/sexpr/sexp.mli.sexp
    ../examples/code/sexpr/sexp_default.topscript.sexp
    ../examples/code/sexpr/sexp_list.topscript.sexp
    ../examples/code/sexpr/sexp_opaque.topscript.sexp
    ../examples/code/sexpr/sexp_option.topscript.sexp
    ../examples/code/sexpr/sexp_override.ml.sexp
    ../examples/code/sexpr/sexp_printer.topscript.sexp
    ../examples/code/sexpr/test_interval/build_test_interval.sh.sexp
    ../examples/code/sexpr/test_interval/int_interval.ml.sexp
    ../examples/code/sexpr/test_interval/int_interval.mli.sexp
    ../examples/code/sexpr/test_interval/jbuild.sexp
    ../examples/code/sexpr/test_interval/test_interval.ml.sexp
    ../examples/code/sexpr/test_interval_nosexp/build_test_interval_nosexp.errsh.sexp
    ../examples/code/sexpr/test_interval_nosexp/jbuild.sexp
    ../examples/code/sexpr/to_from_sexp.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (18-concurrent-programming.html))))

(rule (
  (targets (18-concurrent-programming.html))
  (deps (
    ../book/18-concurrent-programming.html
    ../bin/bin/app.exe
    ../examples/code/async/better_echo.ml.sexp
    ../examples/code/async/echo/echo.ml.sexp
    ../examples/code/async/echo/jbuild.sexp
    ../examples/code/async/echo/run_echo.sh.sexp
    ../examples/code/async/main.topscript.sexp
    ../examples/code/async/pipe_write_break.rawscript.sexp
    ../examples/code/async/run_native_code_log_delays.rawsh.sexp
    ../examples/code/async/search/jbuild.sexp
    ../examples/code/async/search/run_search.sh.sexp
    ../examples/code/async/search/search.ml.sexp
    ../examples/code/async/search_out_of_order.ml.sexp
    ../examples/code/async/search_with_configurable_server/jbuild.sexp
    ../examples/code/async/search_with_configurable_server/run_search_with_configurable_server.errsh.sexp
    ../examples/code/async/search_with_configurable_server/search_with_configurable_server.ml.sexp
    ../examples/code/async/search_with_error_handling/jbuild.sexp
    ../examples/code/async/search_with_error_handling/run_search_with_error_handling.sh.sexp
    ../examples/code/async/search_with_error_handling/search_with_error_handling.ml.sexp
    ../examples/code/async/search_with_timeout.ml.sexp
    ../examples/code/async/search_with_timeout_no_leak/jbuild.sexp
    ../examples/code/async/search_with_timeout_no_leak/run_search_with_timeout_no_leak.sh.sexp
    ../examples/code/async/search_with_timeout_no_leak/search_with_timeout_no_leak.ml.sexp
    ../examples/code/async/search_with_timeout_no_leak_simple/search_with_timeout_no_leak_simple.ml.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (19-foreign-function-interface.html))))

(rule (
  (targets (19-foreign-function-interface.html))
  (deps (
    ../book/19-foreign-function-interface.html
    ../bin/bin/app.exe
    ../examples/code/ctypes/ctypes.mli.sexp
    ../examples/code/ctypes/ctypes_impl.ml.sexp
    ../examples/code/ffi/datetime/build_datetime.sh.sexp
    ../examples/code/ffi/datetime/datetime.ml.sexp
    ../examples/code/ffi/datetime/jbuild.sexp
    ../examples/code/ffi/hello/build_hello.sh.sexp
    ../examples/code/ffi/hello/hello.ml.sexp
    ../examples/code/ffi/hello/jbuild.sexp
    ../examples/code/ffi/install.rawsh.sexp
    ../examples/code/ffi/ncurses/infer_ncurses.sh.sexp
    ../examples/code/ffi/ncurses/ncurses.h.sexp
    ../examples/code/ffi/ncurses/ncurses.ml.sexp
    ../examples/code/ffi/ncurses/ncurses.mli.sexp
    ../examples/code/ffi/posix.topscript.sexp
    ../examples/code/ffi/posix_headers.h.sexp
    ../examples/code/ffi/qsort.topscript.sexp
    ../examples/code/ffi/qsort/build_qsort.sh.sexp
    ../examples/code/ffi/qsort/jbuild.sexp
    ../examples/code/ffi/qsort/qsort.h.sexp
    ../examples/code/ffi/qsort/qsort.ml.sexp
    ../examples/code/ffi/qsort/qsort.mli.sexp
    ../examples/code/ffi/qsort/qsort_typedef.h.sexp
    ../examples/code/ffi/return_c_frag.c.sexp
    ../examples/code/ffi/return_c_frag.h.sexp
    ../examples/code/ffi/return_c_uncurried.c.sexp
    ../examples/code/ffi/return_frag.ml.sexp
    ../examples/code/ffi/timeval_headers.h.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (20-runtime-memory-layout.html))))

(rule (
  (targets (20-runtime-memory-layout.html))
  (deps (
    ../book/20-runtime-memory-layout.html
    ../bin/bin/app.exe
    ../examples/code/memory-repr/custom_ops.c.sexp
    ../examples/code/memory-repr/reprs.topscript.sexp
    ../examples/code/memory-repr/simple_record.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (21-garbage-collector.html))))

(rule (
  (targets (21-garbage-collector.html))
  (deps (
    ../book/21-garbage-collector.html
    ../bin/bin/app.exe
    ../examples/code/gc/barrier_bench/barrier_bench.ml.sexp
    ../examples/code/gc/barrier_bench/jbuild.sexp
    ../examples/code/gc/barrier_bench/run_barrier_bench.sh.sexp
    ../examples/code/gc/barrier_bench/show_barrier_bench_help.sh.sexp
    ../examples/code/gc/finalizer/finalizer.ml.sexp
    ../examples/code/gc/finalizer/jbuild.sexp
    ../examples/code/gc/finalizer/run_finalizer.sh.sexp
    ../examples/code/gc/tune.topscript.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (22-compiler-frontend.html))))

(rule (
  (targets (22-compiler-frontend.html))
  (deps (
    ../book/22-compiler-frontend.html
    ../bin/bin/app.exe
    ../examples/code/front-end/alice.ml.sexp
    ../examples/code/front-end/alice.mli.sexp
    ../examples/code/front-end/alice_combined.ml.sexp
    ../examples/code/front-end/broken_module.ml.sexp
    ../examples/code/front-end/broken_poly.ml.sexp
    ../examples/code/front-end/broken_poly_with_annot.ml.sexp
    ../examples/code/front-end/build_broken_module.errsh.sexp
    ../examples/code/front-end/build_broken_poly.errsh.sexp
    ../examples/code/front-end/build_broken_poly_with_annot.errsh.sexp
    ../examples/code/front-end/build_follow_on_function.errsh.sexp
    ../examples/code/front-end/build_non_principal.sh.sexp
    ../examples/code/front-end/build_ocamldoc.rawsh.sexp
    ../examples/code/front-end/build_principal.sh.sexp
    ../examples/code/front-end/build_principal_corebuild.sh.sexp
    ../examples/code/front-end/build_type_conv_with_camlp4.rawsh.sexp
    ../examples/code/front-end/build_type_conv_without_camlp4.errsh.sexp
    ../examples/code/front-end/camlp4_dump.cmd.sexp
    ../examples/code/front-end/camlp4_toplevel.topscript.sexp
    ../examples/code/front-end/comparelib_test.ml.sexp
    ../examples/code/front-end/comparelib_test.mli.sexp
    ../examples/code/front-end/conflicting_interface.ml.sexp
    ../examples/code/front-end/conflicting_interface.mli.sexp
    ../examples/code/front-end/conflicting_interfaces.errsh.sexp
    ../examples/code/front-end/doc.ml.sexp
    ../examples/code/front-end/fixed_module.ml.sexp
    ../examples/code/front-end/follow_on_function.ml.sexp
    ../examples/code/front-end/inconsistent_compilation_units.rawsh.sexp
    ../examples/code/front-end/indent_follow_on_function.sh.sexp
    ../examples/code/front-end/indent_follow_on_function_fixed.sh.sexp
    ../examples/code/front-end/infer_typedef.sh.sexp
    ../examples/code/front-end/install_ocp_index.rawsh.sexp
    ../examples/code/front-end/let_notunit.ml.sexp
    ../examples/code/front-end/let_unit.syntax.sexp
    ../examples/code/front-end/non_principal.ml.sexp
    ../examples/code/front-end/parsetree_typedef.sh.sexp
    ../examples/code/front-end/principal.ml.sexp
    ../examples/code/front-end/process_comparelib_interface.sh.sexp
    ../examples/code/front-end/process_comparelib_test.sh.sexp
    ../examples/code/front-end/short_paths_1.rawsh.sexp
    ../examples/code/front-end/short_paths_2.rawsh.sexp
    ../examples/code/front-end/short_paths_3.rawsh.sexp
    ../examples/code/front-end/type_conv_example.ml.sexp
    ../examples/code/front-end/typedef.ml.sexp
    ../examples/code/front-end/typedef_objinfo.sh.sexp
    ../examples/code/front-end/typedtree_typedef.sh.sexp
    ../examples/code/front-end/unused_var.ml.sexp
    ../examples/code/ocp-index/index_ncurses.sh.sexp
    ../examples/code/packing/build_test.sh.sexp
    ../examples/code/packing/show_files.sh.sexp
    ../examples/code/packing/test.ml.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (23-compiler-backend.html))))

(rule (
  (targets (23-compiler-backend.html))
  (deps (
    ../book/23-compiler-backend.html
    ../bin/bin/app.exe
    ../examples/code/back-end-embed/build_embed.sh.sexp
    ../examples/code/back-end-embed/build_embed_binary.rawsh.sexp
    ../examples/code/back-end-embed/build_embed_c.sh.sexp
    ../examples/code/back-end-embed/build_embed_native.rawsh.sexp
    ../examples/code/back-end-embed/embed_me1.ml.sexp
    ../examples/code/back-end-embed/embed_me2.ml.sexp
    ../examples/code/back-end-embed/link_custom.rawsh.sexp
    ../examples/code/back-end-embed/link_dllib.rawsh.sexp
    ../examples/code/back-end-embed/main.c.sexp
    ../examples/code/back-end-embed/run_debug_hello.sh.sexp
    ../examples/code/back-end/alternate_list/alternate_list.ml.sexp
    ../examples/code/back-end/alternate_list/jbuild.sexp
    ../examples/code/back-end/alternate_list/run_alternate_list.sh.sexp
    ../examples/code/back-end/asm_from_compare_mono.sh.sexp
    ../examples/code/back-end/bench_patterns/bench_patterns.ml.sexp
    ../examples/code/back-end/bench_patterns/jbuild.sexp
    ../examples/code/back-end/bench_patterns/run_bench_patterns.sh.sexp
    ../examples/code/back-end/bench_poly_and_mono/bench_poly_and_mono.ml.sexp
    ../examples/code/back-end/bench_poly_and_mono/jbuild.sexp
    ../examples/code/back-end/bench_poly_and_mono/run_bench_poly_and_mono.sh.sexp
    ../examples/code/back-end/cmp.S.sexp
    ../examples/code/back-end/compare_mono.ml.sexp
    ../examples/code/back-end/compare_poly.ml.sexp
    ../examples/code/back-end/compare_poly_asm.S.sexp
    ../examples/code/back-end/gdb_alternate0.rawsh.sexp
    ../examples/code/back-end/gdb_alternate1.rawsh.sexp
    ../examples/code/back-end/gdb_alternate2.rawsh.sexp
    ../examples/code/back-end/gdb_alternate3.rawsh.sexp
    ../examples/code/back-end/instr_for_pattern_monomorphic_small.sh.sexp
    ../examples/code/back-end/lambda_for_pattern_monomorphic_large.sh.sexp
    ../examples/code/back-end/lambda_for_pattern_monomorphic_small.sh.sexp
    ../examples/code/back-end/lambda_for_pattern_polymorphic.sh.sexp
    ../examples/code/back-end/opam_switch.rawsh.sexp
    ../examples/code/back-end/pattern_monomorphic_large.ml.sexp
    ../examples/code/back-end/pattern_monomorphic_small.ml.sexp
    ../examples/code/back-end/pattern_polymorphic.ml.sexp
    ../examples/code/back-end/perf_record.rawsh.sexp
    ../examples/code/back-end/perf_report.rawsh.sexp))
  (action (
    run rwo-build build chapter -o . -code ../examples -repo-root .. ${<}))))

(alias ((name site) (deps (faqs.html))))

(rule (
  (targets (faqs.html))
  (deps (../book/faqs.html ../bin/bin/app.exe))
  (action (run rwo-build build faqs -o . -repo-root ..))))

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

(alias ((name site) (deps (toc.html))))

(rule (
  (targets (toc.html))
  (deps (../book/toc.html ../bin/bin/app.exe))
  (action (run rwo-build build toc -o . -repo-root ..))))

